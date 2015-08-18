:- module(
  gv_graph,
  [
    gv_graph//1 % +Graph:compound
  ]
).

/** <module> GraphViz graph

Generates GraphViz graphs in the DOT format based on
a Prolog representation of a graph.

In GraphViz vertices are called 'nodes'.

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(gv/gv_graph_comp)).
:- use_module(library(lists)).
:- use_module(library(option)).





%! gv_graph(+Graph:compound)//
% The follow graph attributes are supported,
% beyond the GraphViz attributes for graphs:
%   * `directed(+boolean)`
%      Whether the graph is directed (`true`) or undirected (`false`).
%      Default: `false`.
%   * `name(+GraphName:atom)`
%   * `strict(+StrictGraph:boolean)`
%      This forbids the creation of self-arcs and multi-edges;
%      they are ignored in the input file.
%      Only in combinattion with directionality `directed`.
%
% ```abnf
% graph = ["strict"] ("graph" / "digraph") [ID] "{" stmt_list "}"
% ```
%
% `GraphTerm` is a compound term of the following form:
% ```prolog
% graph(VertexTerms,RankedVertexTerms,EdgeTerms,GraphAttributes)
% ```
%
% `RankedVertexTerms` is a list of compound terms of the following form:
% ```prolog
% rank(RankNode,ContentNodes)
% ```
%
% @tbd Add support for subgraphs (arbitrary nesting).
% @tbd Add support for escape strings:
%      http://www.graphviz.org/doc/info/attrs.html#k:escString
% @tbd Assert attributes that are generic with respect to a subgraph.
% @tbd Not all vertex and edge properties can be shared it seems (e.g., label).

gv_graph(G) -->
  gv_graph(G, 0).

gv_graph(G1, I) -->
  {
    include_ranks(G1, G2),
    G2 = graph(VTerms,RankedVTerms,ETerms,GAttrs1),
    shared_attributes(VTerms, SharedVAttrs, NewVTerms),
    shared_attributes(ETerms, SharedEAttrs, NewETerms),
    add_default(GAttrs1, overlap(false), GAttrs2),
    I = 0
  },

  % The first statement in the GraphViz output.
  % States that this file represents a graph according to the GraphViz format.
  indent(I),
  
  % Strictness.
  {select_option(strict(Strict), GAttrs2, GAttrs3, false)},
  gv_strict(Strict),
  
  % Directedness.
  {select_option(directed(Directed), GAttrs3, GAttrs4, true)},
  gv_graph_type(Directed),
  " ",
  
  % Graph name.
  (   {select_option(name(GName), GAttrs4, GAttrs5)}
  ->  gv_id(GName),
      " "
  ;   {GAttrs5 = GAttrs4}
  ),

  % The body of the DOT file appears between curly braces.
  "{\n",

  % The following lines are indented.
  {NewI is I + 1},

  % Attributes that apply to the graph as a whole.
  gv_generic_attributes_statement(graph, NewI, GAttrs5),

  % Attributes that are the same for all nodes.
  gv_generic_attributes_statement(node, NewI, SharedVAttrs),

  % Attributes that are the same for all edges.
  gv_generic_attributes_statement(edge, NewI, SharedEAttrs),

  % Only add a line_feed if some content was already written
  % and some content is about to be written.
  (   % Succeeds if no content was written.
      {(GAttrs5 == [], SharedVAttrs == [], SharedEAttrs == [])}
  ->  ""
  ;   % Succeeds if no content is about to be written.
      {(NewVTerms == [], RankedVTerms == [])}
  ->  ""
  ;   "\n"
  ),

  % The list of GraphViz nodes.
  gv_node_statements(NewI, NewVTerms),
  (   {NewVTerms == []}
  ->  ""
  ;   "\n"
  ),

  % The ranked GraphViz nodes (displayed at the same height).
  gv_ranked_node_collections(NewI, RankedVTerms),
  (   {RankedVTerms == []}
  ->  ""
  ;   "\n"
  ),

  {
    findall(
      edge(FromId,ToId,[]),
      (
        nth0(Index1, RankedVTerms, rank(vertex(FromId,_),_)),
        nth0(Index2, RankedVTerms, rank(vertex(ToId,_),_)),
        % We assume that the rank vertices are nicely ordered.
        succ(Index1, Index2)
      ),
      RankEdges
    )
  },

  % The rank edges.
  gv_edge_statements(NewI, Directed, RankEdges),

  % The non-rank edges.
  gv_edge_statements(NewI, Directed, NewETerms),
  
  % Note that we do not include a line_feed here.

  % We want to indent the closing curly brace.
  indent(I),
  "}\n".

gv_edge_statements(I, Dir, L) -->
  *(gv_edge_statement(I, Dir), L, []), !.

gv_node_statements(I, L) -->
  *(gv_node_statement(I), L, []), !.

gv_ranked_node_collections(I, L) -->
  *(gv_ranked_node_collection(I), L, []), !.





% HELPERS %

%! add_default_option(
%!   +Options:list(compound),
%!   +Default:compound,
%!   -NewOptions:list(compound)
%! ) is det.

add_default(L1, Opt, L2):-
  Opt =.. [N,_Value],
  Opt0 =.. [N,_FreshVar],
  (   option(Opt0, L1)
  ->  L2 = L1
  ;   L2 = [Opt|L1]
  ).


%! gv_graph_type(+Directed:boolean)// is det.
% The type of graph that is represented.

gv_graph_type(false) --> "graph".
gv_graph_type(true) --> "digraph".


%! gv_strict(+Strict:boolean)// is det.
% The keyword denoting that the graph is strict, i.e., has no self-arcs and
% no multi-edges.
% This only applies to directed graphs.

gv_strict(false) --> "".
gv_strict(true) --> "strict ".


%! invlude_ranges(+Graph:compound, -GraphWithRanks:compound) is det.
% Ensures that there is a ranks components in
% the graph-denoting compound term.

include_ranks(graph(Vs,Rs,Es,L), graph(Vs,Rs,Es,L)).
include_ranks(graph(Vs,Es,L), graph(Vs,[],Es,L)).


%! shared_attributes(
%!   +Terms:list(compound),
%!   -SharedAttributes:list(compound),
%!   -NewTerms:list(compound)
%! ) is det.

shared_attributes(Ts1, Shared, Ts2):-
  maplist(term_to_attrs, Ts1, L1),
  extract_shared(L1, Shared),
  maplist(remove_shared_attributes(Shared), L1, L2),
  maplist(term_change_attrs, Ts1, L2, Ts2).

term_to_attrs(edge(_,_,A), A).
term_to_attrs(vertex(_,A), A).

extract_shared([], []):- !.
extract_shared(Argss, Shared):-
  ord_intersection(Argss, Shared).

remove_shared_attributes(Shared, Args1, Args2):-
  ord_subtract(Args1, Shared, Args2).

term_change_attrs(edge(From,To,_), A, edge(From,To,A)).
term_change_attrs(vertex(Id,_), A, vertex(Id,A)).
