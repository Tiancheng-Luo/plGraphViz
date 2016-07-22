:- module(
  gv_color_scrape,
  [
    gv_color_scrape/1 % +File
  ]
).

/** <module> GraphViz: Scrape colors

@author Wouter Beek
@version 2015/10, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/http_download)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(pl_term)).
:- use_module(library(print_ext)).
:- use_module(library(xpath)).
:- use_module(library(xpath/xpath_table)).
:- use_module(library(yall)).





%! gv_color_scrape(+File) is det.

gv_color_scrape(File):-
  debug(io, "Updating the GraphViz color table.", []),
  call_to_stream(File, [In,Meta,Meta]>>gv_color_download(In)).


gv_color_download(Write):-
  gv_color_iri(Iri),
  html_download(Iri, Dom),
  xpath_chk(Dom, //table(1), TableDom1),
  xpath_chk(Dom, //table(2), TableDom2),
  maplist(write_color_table(Write), [x11,svg], [TableDom1,TableDom2]).


write_color_table(Write, Colorscheme, TableDom):-
  xpath_table(TableDom, _, Rows),
  append(Rows, Cells),
  forall(
    member(Cell, Cells),
    with_output_to(Write, write_fact(gv_color(Colorscheme, Cell)))
  ).

gv_color_iri('http://www.graphviz.org/doc/info/colors.html').
