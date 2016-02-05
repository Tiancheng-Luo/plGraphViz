:- module(
  build_export_graph,
  [
    build_export_graph/2, % +Graph, -ExportGraph
    build_export_graph/3 % +Graph
                         % -ExportGraph:compound
                         % +Options:list(compound)
  ]
).

/** <module> Build graph representation for exporting

Support for building GIF representations.

# Graph Intermediate Format (GIF)

## Graph

```prolog
graph(Vs:ordset(compound),Ranks,Es:compound,Attributes:list(compound))
```

### Edge

```prolog
edge(FromVertexId,ToVertexId,Attributes:list(compound))
```

### Rank

```prolog
RankVertex:compound-ContentVertices:ordset(compound)
```

### Vertex

```prolog
vertex(Id,Attributes:list(compound))
```

# Property functions

Edge label:
  1. [[graph_edge]] edge_label/2

Vertex coordinates:
  1. [[circle_coords]] circular_coord/4
  2. [[random_coords]] random_coord/4

---

@author Wouter Beek
@version 2015/07, 2015/09-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(list_ext)).
:- use_module(library(option_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(yall)).

:- predicate_options(build_export_graph/4, 4, [
     pass_to(edge_term/3, 3),
     pass_to(graph_attributes/2, 2),
     pass_to(vertex_term/3, 3)
   ]).
:- predicate_options(edge_term/3, 3, [
     edge_arrowhead(+callable),
     edge_color(+callable),
     edge_id(+callable),
     edge_label(+callable),
     edge_penwidth(+callable),
     edge_style(+callable)
   ]).
:- predicate_options(graph_attributes/2, 2, [
     graph_charset(+oneof(['iso-8859-1','Latin1','UTF-8'])),
     graph_colorscheme(+oneof([none,svg,x11])),
     graph_directed(+boolean),
     graph_fontsize(+float),
     graph_label(+atom),
     graph_overlap(+boolean)
   ]).
:- predicate_options(vertex_term/3, 3, [
     vertex_color(+callable),
     vertex_id(+callable),
     vertex_image(+callable),
     vertex_label(+callable),
     vertex_peripheries(+callable),
     vertex_position(+callable),
     vertex_rank(+callable),
     vertex_shape(+callable),
     vertex_uri(+callable)
   ]).

:- meta_predicate(build_export_graph(+,-,:)).

is_meta(edge_arrowhead).
is_meta(edge_color).
is_meta(edge_id).
is_meta(edge_label).
is_meta(edge_penwidth).
is_meta(edge_style).
is_meta(vertex_color).
is_meta(vertex_id).
is_meta(vertex_image).
is_meta(vertex_label).
is_meta(vertex_peripheries).
is_meta(vertex_position).
is_meta(vertex_rank).
is_meta(vertex_shape).
is_meta(vertex_uri).





%! build_export_graph(+Graph, -ExportGraph:compound) is det.
% Wrapper around build_export_graph/3 with default options.

build_export_graph(G, ExportG):-
  build_export_graph(G, ExportG, []).


%! build_export_graph(
%!   +Graph,
%!   -ExportGraph:compound,
%!   +Options:list(compound)
%! ) is det.
% Graph is either:
%   * a coumpound term `graph(Vs,Es)`, or
%   * an unlabeled graph as defined by `library(ugraph)`.
%
% The following options are supported:
%   * `vertex_rank(:RankFunction)`
%     Assigns a non-negative integer to each vertex.
%     No default.

build_export_graph(G, graph(VTerms2,VRanks,ETerms,GAttrs), Opts1):-
  graph_components(G, Vs, Es),
  meta_options(is_meta, Opts1, Opts2),
  maplist([V,VTerm]>>vertex_term(Vs, V, VTerm, Opts2), Vs, VTerms1),
  build_export_ranks(Vs, VTerms1, VRanks, VTerms2, Opts2),
  maplist([E,ETerm]>>edge_term(Vs, E, ETerm, Opts2), Es, ETerms),
  graph_attributes(GAttrs, Opts2).

graph_components(graph(Vs,Es), Vs, Es):- !.
graph_components(G, Vs, Es):-
  s_graph_components(G, Vs, Es).

build_export_ranks(Vs, VTerms, VRanks, [], Opts):-
  option(vertex_rank(VRank_2), Opts), !,
  maplist(VRank_2, Vs, Ranks),
  pairs_keys_values(Pairs, Ranks, VTerms),
  group_pairs_by_key(Pairs, GroupedPairs),
  build_export_rank_terms(GroupedPairs, VRanks).
build_export_ranks(_, VTerms, [], VTerms, _).

build_export_rank_terms([N-VTerms|T1], [RankTerm-VTerms|T2]):- !,
  build_export_rank_term(N, RankTerm),
  build_export_rank_terms(T1, T2).
build_export_rank_terms([], []).

build_export_rank_term(N, vertex(Id,[label(""),shape(none)])):-
  format(atom(Id), "r~d", [N]).



%! edge_term(
%!   +Vertices:ordset(compound),
%!   +Edge:compound,
%!   -EdgeTerm:compound,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * `edge_arrowhead(+callable)`
%     No default.
%   * `edge_color(+callable)`
%     No default.
%   * `edge_id(+callable)`
%     Function that assignes the unique identifiers for an edge's
%     incident vertices.
%   * `edge_label(+callable)`
%     No default.
%   * `edge_penwidth(+callable)`
%     No default.
%   * `edge_style(+callable)`
%     No default.

edge_term(Vs, E, edge(FromId,ToId,EAttrs), Opts):-
  % Arrowhead
  if_option(edge_arrowhead(Arrowhead_2), Opts,
    call(Arrowhead_2, E, EArrowhead)
  ),
  
  % Color.
  if_option(edge_color(ColorFunction), Opts, call(ColorFunction, E, EColor)),
  
  % Id.
  (   option(edge_id(Id_2), Opts)
  ->  call(Id_2, E, FromId, ToId)
  ;   edge_components(E, FromV, ToV),
      nth0chk(FromId, Vs, FromV),
      nth0chk(ToId, Vs, ToV)
  ),
  
  % Label.
  if_option(edge_label(ELabel_3), Opts, string_phrase(dcg_call(ELabel_3, E), ELabel)),
  
  % Penwidth.
  if_option(edge_penwidth(Penwidth_2), Opts, call(Penwidth_2, E, EPenwidth)),
  
  % Style.
  if_option(edge_style(Style_2), Opts, call(Style_2, E, EStyle)),
  
  exclude(
    option_has_var_value,
    [
      arrowhead(EArrowhead),
      color(EColor),
      label(ELabel),
      penwidth(EPenwidth),
      style(EStyle)
    ],
    EAttrs
  ).



%! graph_attributes(
%!   -GraphAttributes:list(compound),
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * `graph_charset(+oneof(['iso-8859-1','Latin1','UTF-8']))`
%     The name of the character set that is used to encode text in the graph.
%     Default: `UTF-8`.
%   * `graph_colorscheme(+oneof([none,svg,x11]))`
%     The colorscheme from which the color in this graph are taken.
%     Default: `x11`.
%   * `graph_directed(+boolean)`
%     Whether the graph is directed (`true`) or undirected (`false`).
%     Default: `false`.
%   * `graph_fontsize(+float)`
%     The font size of text in the graph.
%     Default: `11.0`.
%   * `graph_label(+atom)`
%     The graph label.
%     No default.
%   * `graph_overlap(+boolean)`
%     Whether the vertices are allowed to overlap.
%     Default: `false`.

graph_attributes(GAttrs, Opts):-
  % Characer set.
  option(graph_charset(Charset), Opts, 'UTF-8'),
  % Colorscheme.
  option(graph_colorscheme(Colorscheme), Opts, x11),
  % Directed.
  option(graph_directed(Directed), Opts, false),
  % Fontsize.
  option(graph_fontsize(Fontsize), Opts, 11.0),
  % Label.
  % Defaults to the empty string.
  option(graph_label(GLabel), Opts, '""'),
  % Overlap.
  option(graph_overlap(Overlap), Opts, false),
  exclude(
    option_has_var_value,
    [
      charset(Charset),
      colorscheme(Colorscheme),
      directed(Directed),
      fontsize(Fontsize),
      label(GLabel),
      overlap(Overlap)
    ],
    GAttrs
  ).



%! vertex_term(
%!   +Vertices:ordset(compound),
%!   +Vertex:compound,
%!   -VertexTerm:compound,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * `vertex_color(:ColorFunction)`
%     A function that assigns colors to vertices.
%     No default.
%   * `vertex_id(:ColorFunction)`
%     A functions that assigns unique identifiers to vertices.
%   * `vertex_image(:ImageFunction)`
%     A function that assigns images to vertices.
%     No default.
%   * `vertex_label(:LabelFunction)`
%     A function that assigns labels to vertices.
%     No default.
%   * `vertex_peripheries(:PeripheriesFunction)`
%     A function that assinges peripheries to vertices.
%     No default.
%   * `vertex_position(:PositionFunction)`
%     No default.
%   * `vertex_shape(:ShapeFunction)`
%     A function that assinges shapes to vertices.
%     No default.
%   * `vertex_uri(:UriFunction)`

vertex_term(Vs, V, vertex(VId,VAttrs), Opts):-
  % Color.
  if_option(vertex_color(Color_2), Opts, call(Color_2, V, VColor)),
  
  % Id.
  (option(vertex_id(Id_2), Opts) -> call(Id_2, V, VId) ; nth0chk(VId, Vs, V)),
  
  % Image.
  ignore(if_option(vertex_image(Image_2), Opts, call(Image_2, V, VImage))),

  % Label.
  if_option(vertex_label(VLabel_2), Opts, string_phrase(dcg_call(VLabel_2, V), VLabel)),

  % Peripheries.
  if_option(vertex_peripheries(Peripheries_2), Opts,
    call(Peripheries_2, V, VPeripheries)
  ),

  % Position.
  if_option(vertex_position(Position_4), Opts,
    call(Position_4, Vs, Opts, V, VPosition)
  ),

  % Shape.
  if_option(vertex_shape(Shape_2), Opts, call(Shape_2, V, VShape)),
  
  % URI
  if_option(vertex_uri(Uri_2), Opts, call(Uri_2, V, VUri)),

  exclude(
    option_has_var_value,
    [
      color(VColor),
      image(VImage),
      label(VLabel),
      peripheries(VPeripheries),
      pos(VPosition),
      shape(VShape),
      'URL'(VUri)
    ],
    VAttrs
  ).





% HELPERS %

%! edge_components(+Edge:compound, -FromV, -ToV) is det.

edge_components(edge(FromV,_,ToV), FromV, ToV):- !.
edge_components(edge(FromV,ToV), FromV, ToV):- !.
edge_components(FromV-ToV, FromV, ToV):- !.
