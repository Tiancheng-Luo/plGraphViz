:- module(
  gv_color,
  [
    gv_color/2, % ?Colorscheme:oneof([svg,x11])
                % ?Color:atom
    color//1, % +Color:compound
    colorList//1 % +Pairs:list(pair(compound,float))
  ]
).
:- ensure_loaded(library('gv/gv_color.data')).

/** <module> GraphViz color

@author Wouter Beek
@tbd Color value `transparent` is only available in the output formats
     ps, svg, fig, vmrl, and the bitmap formats.
@version 2015/08, 2015/10, 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).





% color(+Color:compound)// .
% A *color* is represented by a compound term of one of the following forms:
%   1. `rgb(Red:nonneg,Green:nonneg,Blue:nonneg)`
%   2. `rgba(Red:nonneg,Green:nonneg,Blue:nonneg,Alpha:nonneg)`
%   3. `hsv(Hue:between(0.0,1.0),Saturation:between(0.0,1.0),Value:between(0.0,1.0))`

color(rgb(Red,Green,Blue)) --> !,
  "#",
  '#'(3, hex_color, [Red,Green,Blue]).
color(rgbs(Red,Green,Blue,Alpha)) --> !,
  color(rgb(Red,Green,Blue)),
  hex_color(Alpha).
color(hsv(Hue,Saturation,Value)) --> !,
  '#'(3, hsv_color, [Hue,Saturation,Value]).
color(Name) -->
  {gv_color(_, Name)},
  atom(Name).

hex_color(I) -->
  xinteger(I).

hsv_color(D, Head, Tail):-
  format(codes(Head,Tail), '~2f', [D]).



%! colorList(+Pairs:list(pair(compound,float)))// .

colorList(L) -->
  '+'(wc, L).

wc(Color-Float) -->
  color(Color),
  (   wc_weight(Float)
  ;   ""
  ).

wc_weight(Float) -->
  ";",
  float(Float).
