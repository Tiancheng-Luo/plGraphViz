:- module(
  gv_dom,
  [
    gv_dom/3 % +ExportGraph:compound
             % -Dom:list(compound)
             % +Options:list(compound)
  ]
).

/** <module> GraphViz DOM

@author Wouter Beek
@version 2015/07, 2016/01
*/

:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(svg/svg_dom)).





%! gv_dom(
%!   +ExportGraph:compound,
%!   -Dom:list(compound),
%!   +Options:list(compound)
%! ) is det.

gv_dom(ExportG, Dom, Opts1):-
  % Make sure the file type of the output file is SvgDom.
  merge_options([output(svg)], Opts1, Opts2),
  graph_viz(ExportG, ToFile, Opts2),
  svg_dom(ToFile, Dom),
  delete_file(ToFile).
