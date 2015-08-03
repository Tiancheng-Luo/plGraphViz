:- module(
  gv_color,
  [
    gv_color/2, % ?Colorscheme:oneof([svg,x11])
                % ?Color:atom
    color//1, % +Color:compound
    colorList//1 % +Pairs:list(pair(compound,float))
  ]
).

/** <module> GraphViz color

@author Wouter Beek
@tbd Color value `transparent` is only available in the output formats
     ps, svg, fig, vmrl, and the bitmap formats.
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(deb_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(file_ext)).
:- use_module(library(html/html_download)).
:- use_module(library(lists)).
:- use_module(library(persistency)).
:- use_module(library(xpath)).
:- use_module(library(xpath/xpath_table)).

%! gv_color(?Colorscheme:oneof([svg,x11]), ?Color:atom) is nondet.

:- persistent(gv_color(colorscheme:oneof([svg,x11]),color:atom)).

:- initialization(gv_color_init).





% color(+Color:compound)// .
% A *color* is represented by a compound term of one of the following forms:
%   1. `rgb(Red:nonneg,Green:nonneg,Blue:nonneg)`
%   2. `rgba(Red:nonneg,Green:nonneg,Blue:nonneg,Alpha:nonneg)`
%   3. `hsv(Hue:between(0.0,1.0),Saturation:between(0.0,1.0),Value:between(0.0,1.0))`

color(rgb(Red,Green,Blue)) --> !,
  "#",
  '#'(3, hex_color, [Red,Green,Blue], []).
color(rgbs(Red,Green,Blue,Alpha)) --> !,
  color(rgb(Red,Green,Blue)),
  hex_color(Alpha).
color(hsv(Hue,Saturation,Value)) --> !,
  '#'(3, hsv_color, [Hue,Saturation,Value], []).
color(Name) -->
  {gv_color(_, Name)},
  atom(Name).

hex_color(I) -->
  xinteger(I).

hsv_color(D, Head, Tail):-
  format(codes(Head,Tail), '~2f', [D]).



%! colorList(+Pairs:list(pair(compound,float)))// .

colorList(L) -->
  '+'(wc, L, []).

wc(Color-Float) -->
  color(Color),
  (   wc_weight(Float)
  ;   ""
  ).

wc_weight(Float) -->
  ";",
  float(Float).





% INITIALIZATION %

%! gv_color_download is det.

gv_color_download:-
  verbose_call(
    'updating the GraphViz color table',
    gv_color_download0
  ).

gv_color_download0:-
  gv_color_uri(Uri),
  html_download(Uri, Dom),
  xpath_chk(Dom, //table(1), TableDom1),
  xpath_chk(Dom, //table(2), TableDom2),
  maplist(assert_color_table, [x11,svg], [TableDom1,TableDom2]).


assert_color_table(Colorscheme, TableDom):-
  xpath_table(TableDom, _, Rows),
  append(Rows, Cells),
  forall(
    member(Cell, Cells),
    assert_gv_color(Colorscheme, Cell)
  ).


%! gv_color_file(-File:atom) is det.

gv_color_file(File):-
  absolute_file_name('gv_color.log', File, [access(write)]).


%! gv_color_init is det.

gv_color_init:-
  gv_color_file(File),
  (   exists_file(File)
  ->  true
  ;   touch(File)
  ),
  db_attach(File, []),
  file_age(File, Age),
  gv_color_update(Age).


%! gv_color_update(+Age:float) is det.

% The persistent store is still fresh.
gv_color_update(Age):-
  once(gv_color(_, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
gv_color_update(_):-
  retractall_gv_color(_, _),
  gv_color_download.


%! gv_color_uri(-Url:url) is det.

gv_color_uri('http://www.graphviz.org/doc/info/colors.html').
