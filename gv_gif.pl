:- module(
  gv_gif,
  [
    create_gif/3, % +Edges:ordset
                  % -Gif:compound
                  % +Options:list(nvpair)
    create_gif/4 % +Vertices:ordset
                 % +Edges:ordset
                 % -Gif:compound
                 % +Options:list(nvpair)
  ]
).

/** <module> GraphViz Graph Interchange Format (GIF)

Support for creating GIF representations.

@author Wouter Beek
@version 2014/06-2014/07
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).
:- use_module(graph_theory(graph_generic)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.

:- predicate_options(create_gif/3, 3, [
     pass_to(create_gif/4, 4)
   ]).
:- predicate_options(create_gif/4, 4, [
     pass_to(vertex_term/3, 3),
     pass_to(edge_term/3, 3),
     graph_label(+atom)
   ]).
:- predicate_options(edge_term/3, 3, [
   ]).
:- predicate_options(vertex_term/3, 3, [
     vertex_label(+atom)
   ]).



%! create_gif(+Edges:ordset, -Gif:compound, +Options:list(nvpair)) is det.

create_gif(Es, Gif, Options):-
  edges_to_vertices(Es, Vs),
  create_gif(Vs, Es, Gif, Options).

%! create_gif(
%!   +Vertices:ordset,
%!   +Edges:ordset,
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|graph_label(+LabelFunction)|=
%     The functions that assigns names to graphs.
%     No default.

create_gif(Vs, Es, graph(VTerms,ETerms,GAttrs), Options):-
  % Vertex terms.
  maplist(\V^VTerm^vertex_term(Vs, V, VTerm, Options), Vs, VTerms),
  
  % Edge terms.
  maplist(\E^ETerm^edge_term(Vs, E, ETerm, Options), Es, ETerms),
  
  % Graph attributes.
  (
    option(graph_label(LabelFunction), Options)
  ->
    call(LabelFunction, Graph, GraphLabel)
  ;
    true
  ),
  
  merge_options([directed=true,label=GraphLabel], GAttrs).


%! edge_term(
%!   +Vertices:ordset,
%!   +Edge:pair,
%!   -EdgeTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.

edge_term(Vs, E, edge(FromId,ToId,EAttrs), _):-
  edge_components(E, FromV, ToV),
  
  nth0chk(FromId, Vs, FromV),
  nth0chk(ToId, Vs, ToV),
  
  EAttrs = [].


%! vertex_term(
%!   +Vertices:ordset,
%!   +Vertex,
%!   -VertexTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|vertex_label(+LabelFunction)|=
%     A function that assigns labels to vertices.

vertex_term(Vs, V, vertex(Id,V,VAttrs), Options):-
  nth0chk(Id, Vs, V),
  
  % Label.
  option(vertex_label(LabelFunction), Options, =),
  call(LabelFunction, V, VLabel),
  
  VAttrs = [label=VLabel].

