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
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.



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

create_gif(Vs, Es, graph(V_Terms,E_Terms,G_Attrs), Options):-
  maplist(vertex_term0(Vs, Options), Vs, V_Terms),
  maplist(edge_term0(Vs, Options), Es, E_Terms),
  G_Attrs = [directed=true].

edge_term0(Vs, Options, E, E_Term):-
  edge_term(Vs, E, E_Term, Options).

vertex_term0(Vs, Options, V, V_Term):-
  vertex_term(Vs, V, V_Term, Options).


%! edge_term(
%!   +Vertices:ordset,
%!   +Edge:pair,
%!   -EdgeTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.

edge_term(Vs, E, edge(FromId,ToId,E_Attrs), _):-
  edge_components(E, FromV, _, ToV),
  nth0chk(FromId, Vs, FromV),
  nth0chk(ToId, Vs, ToV),
  E_Attrs = [].


%! vertex_term(
%!   +Vertices:ordset,
%!   +Vertex,
%!   -VertexTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.

vertex_term(Vs, V, vertex(Id,V,V_Attrs), Options):-
  nth0chk(Id, Vs, V),
  
  % Label.
  option(vertex_label(VertexLabel), Options, =),
  call(VertexLabel, V, V_Label),
  
  V_Attrs = [label=V_Label].

vertex_label(V, V_Label):-
  dcg_with_output_to(atom(V_Label), rdf_term_name([literal_ellipsis(50)], V)).



% Helpers

%! edge_components(+Edge:compound, -FromVertex, -EdgeType, -ToVertex) is det.
%! edge_components(-Edge:compound, +FromVertex, ?EdgeType, +ToVertex) is det.

edge_components(FromV-EdgeType-ToV, FromV, EdgeType, ToV):-
  nonvar(EdgeType).
edge_components(FromV-ToV, FromV, EdgeType, ToV):-
  var(EdgeType).


%! edges_to_vertices(+Edges:ordset, -Vertices:ordset) is det.

edges_to_vertices([], []):- !.
edges_to_vertices([S-_-O|T], S3):-
  edges_to_vertices(T, S1),
  ord_add_element(S1, S, S2),
  ord_add_element(S2, O, S3).

