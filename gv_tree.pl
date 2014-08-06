:- module(
  gv_tree,
  [
    tree_to_gv_file/3 % +Tree:compound
                      % ?ToFile:atom
                      % +Options:list(nvpair)
  ]
).

/** <module> GraphViz tree

Export trees to GraphViz.

@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(aggregate)).

:- use_module(generics(trees)).

:- use_module(plGraphViz(gv_file)).
:- use_module(plGraphViz(gv_gif)).


%! tree_to_gv_file(
%!   +Tree:compound,
%!   ?ToFile:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Stores the given tree term into a GraphViz file.
%
% Options are passed on to build_gif/3 and gif_to_gv_file/3.

tree_to_gv_file(Tree, ToFile, Options):-
  tree_to_gif(Tree, Gif, Options),
  gif_to_gv_file(Gif, ToFile, Options).


tree_to_gif(H-T, Gif, Options):-
  tree_to_vertices_edges(Tree, Vs, Es),
  build_gif(Vs, Es, Gif, Options).

