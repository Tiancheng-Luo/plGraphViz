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
@version 2014/06, 2014/10-2014/11, 2015/01, 2015/03
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics), [float//1]).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(persistency)).
:- use_module(library(xpath)).

:- use_module(plc(dcg/abnf_core_rules)).
:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(generics/db_ext)).
:- use_module(plc(generics/persistent_db_ext)).
:- use_module(plc(generics/print_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(io/file_gnu)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_table)).

:- db_add_novel(user:prolog_file_type(log, logging)).

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
  "#",
  '#'(4, hex_color, [Red,Green,Blue,Alpha], []).
color(hsv(Hue,Saturation,Value)) --> !,
  '#'(3, hsv_color, [Hue,Saturation,Value], []).
color(Name) -->
  {gv_color(_, Name)},
  atom(Name).

hex_color(I) -->
  {W1 is I / 16},
  'HEX'(W1),
  {W2 is I mod 16},
  'HEX'(W2).

hsv_color(D, Head, Tail):-
  format(codes(Head,Tail), '~2f', [D]).


%! colorList(+Pairs:list(pair(compound,float)))// .

colorList(Pairs) -->
  '+'(wc, Pairs, []).

wc(Color-Float) -->
  color(Color),
  '?'(wc_weight(Float), []).

wc_weight(Float) -->
  ";",
  float(Float).





% INITIALIZATION %

%! gv_color_download is det.

gv_color_download:-
  report_on_process(
    'Updating the GraphViz color table...',
    (
      gv_color_uri(Uri),
      download_html_dom(Uri, Dom, [html_dialect(html4),silent(true)]),
      xpath(Dom, //table(1), TableDom1),
      xpath(Dom, //table(2), TableDom2),
      maplist(assert_color_table, [x11,svg], [TableDom1,TableDom2])
    )
  ).

assert_color_table(Colorscheme, TableDom):-
  html_to_table(TableDom, _, Rows),
  append(Rows, Cells),
  forall(
    member(Cell, Cells),
    assert_gv_color(Colorscheme, Cell)
  ).


%! gv_color_file(-File:atom) is det.

gv_color_file(File):-
  absolute_file_name(
    data(gv_color),
    File,
    [access(write),file_type(logging)]
  ).


%! gv_color_init is det.

gv_color_init:-
  gv_color_file(File),
  persistent_db_init(File, gv_color_update).


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

