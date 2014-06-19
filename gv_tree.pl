:- module(
  gv_tree,
  [
    tree_to_gv_file/3 % +Options:list(nvpair)
                      % +Tree:compound
                      % ?ToFile:atom
  ]
).

/** <module> GraphViz tree

Export trees to GraphViz.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(aggregate)).

:- use_module(generics(trees)).

:- use_module(plGraphViz(gv_file)).
:- use_module(plGraphViz(gv_gif)).


%! tree_to_gv_file(+Options:list(nvpair), +Tree:compound, ?ToFile:atom) is det.
% Stores the given tree term into a GraphViz file.
%
% Options are passed on to create_gif/3 and gif_to_gv_file/3.

tree_to_gv_file(Options, Tree, ToFile):-
  tree_to_gif(Tree, Gif, Options),
  gif_to_gv_file(Options, Gif, ToFile).


tree_to_gif(H-T, Gif, Options):-
  tree_to_vertices_edges(Tree, Vs, Es),
  create_gif(Vs, Es, Gif, Options).

