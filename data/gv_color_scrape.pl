:- module(
  gv_color_scrape,
  [
    gv_color_scrape/1 % +File:atom
  ]
).

/** <module> GraphViz: Scrape colors

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(http/http_download)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(msg_ext)).
:- use_module(library(pl/pl_term)).
:- use_module(library(xpath)).
:- use_module(library(xpath/xpath_table)).





%! gv_color_scrape(+File:atom) is det.

gv_color_scrape(File):-
  setup_call_cleanup(
    open_any(File, write, Write, Close, []),
    verbose(
      gv_color_download(Write),
      "Updating the GraphViz color table."
    ),
    close_any(Close)
  ).


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
