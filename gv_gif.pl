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
:- use_module(graph_theory(graph_theory)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.

:- predicate_options(create_gif/3, 3, [
     pass_to(create_gif/4, 4)
   ]).
:- predicate_options(create_gif/4, 4, [
     pass_to(edge_term/3, 3),
     pass_to(graph_attributes/2, 2),
     pass_to(vertex_term/3, 3)
   ]).
:- predicate_options(edge_term/3, 3, [
     edge_arrowhead(+atom),
     edge_color(+atom),
     edge_label(+atom),
     edge_style(+atom)
   ]).
:- predicate_options(graph_attributes/2, 2, [
     directedness(+boolean),
     graph_colorscheme(+oneof([none,svg,x11])),
     graph_label(+atom)
   ]).

:- predicate_options(vertex_term/3, 3, [
     vertex_color(+atom),
     vertex_coordinates(+atom),
     vertex_image(+atom),
     vertex_label(+atom),
     vertex_peripheries(+atom),
     vertex_shape(+atom)
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
%   * =|graph_colorscheme(+oneof([none,svg,x11]))
%     No default.
%   * =|graph_label(+LabelFunction)|=
%     The functions that assigns names to graphs.
%     No default.

create_gif(Vs, Es, graph(VTerms,ETerms,GAttrs), Options):-
  % Vertex terms.
  maplist(\V^VTerm^vertex_term(Vs, V, VTerm, Options), Vs, VTerms),

  % Edge terms.
  maplist(\E^ETerm^edge_term(Vs, E, ETerm, Options), Es, ETerms),

  % Graph attributes.
  graph_attributes(GAttrs, Options).


%! edge_term(
%!   +Vertices:ordset,
%!   +Edge:pair,
%!   -EdgeTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|edge_arrowhead(+atom)|=
%   * =|edge_color(+atom)|=
%   * =|edge_label(+atom)|=
%   * =|edge_style(+atom)|=

edge_term(Vs, E, edge(FromId,ToId,EAttrs), Options):-
  edge_components(E, FromV, ToV),
  nth0chk(FromId, Vs, FromV),
  nth0chk(ToId, Vs, ToV),

  % Arrowhead
  if_option(edge_arrowhead(ArrowheadFunction), Options,
    call(ArrowheadFunction, E, EArrowhead)
  ),
  % Color.
  if_option(edge_color(ColorFunction), Options,
    call(ColorFunction, E, EColor)
  ),
  % Label.
  if_option(edge_label(LabelFunction), Options,
    call(LabelFunction, E, ELabel)
  ),
  % Style.
  if_option(edge_style(StyleFunction), Options,
    call(StyleFunction, E, EStyle)
  ),

  merge_options(
    [arrowhead=EArrowhead,color=EColor,label=ELabel,style=EStyle],
    EAttrs
  ).


%! graph_attributes(
%!   -GraphAttributes:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|directedness(+boolean)|=
%     Whether the graph is directed (`true`) or undirected (`false`).
%     Default: `false`.
%   * =|graph_colorscheme(+oneof([none,svg,x11]))|=
%     The colorscheme from which the color in this graph are taken.
%     Default: `svg`.
%   * =|graph_label(+atom)|=
%     The graph label.
%     No default.

graph_attributes(GAttrs, Options):-
  % Directedness.
  option(directedness(Directedness), Options, false),
  % Colorscheme.
  if_option(graph_colorscheme(Colorscheme), Options, true),
  % Label.
  if_option(graph_label(GLabel), Options, true),

  merge_options(
    [colorscheme=Colorscheme,directedness=Directedness,label=GLabel],
    GAttrs
  ).


%! vertex_term(
%!   +Vertices:ordset,
%!   +Vertex,
%!   -VertexTerm:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|vertex_color(+ColorFunction)|=
%     A function that assigns colors to vertices.
%     No default.
%   * =|vertex_image(+ImageFunction)|=
%     A function that assinges images to vertices.
%     No default.
%   * =|vertex_label(+LabelFunction)|=
%     A function that assigns labels to vertices.
%     No default.
%   * =|vertex_peripheries(+PeripheriesFunction)|=
%     A function that assinges peripheries to vertices.
%     No default.
%   * =|vertex_shape(+ShapeFunction)|=
%     A function that assinges shapes to vertices.
%     No default.

vertex_term(Vs, V, vertex(Id,V,VAttrs), Options):-
  nth0chk(Id, Vs, V),

  % Color.
  if_option(vertex_color(ColorFunction), Options,
    call(ColorFunction, V, VColor)
  ),
  % Image.
  if_option(image(ImageFunction), Options,
    call(ImageFunction, V, VImage)
  ),
  % Label.
  if_option(vertex_label(LabelFunction), Options,
    call(LabelFunction, V, VLabel)
  ),
  % Peripheries.
  if_option(vertex_peripheries(PeripheriesFunction), Options,
    call(PeripheriesFunction, V, VPeripheries)
  ),
  % Shape.
  if_option(vertex_shape(ShapeFunction), Options,
    call(ShapeFunction, V, VShape)
  ),

  merge_options(
    [
      color=VColor,
      image=VImage,
      label=VLabel,
      peripheries=VPeripheries,
      shape=VShape
    ],
    VAttrs
  ).

