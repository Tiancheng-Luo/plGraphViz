:- module(
  gv_dot,
  [
    gv_graph//1 % +Graph:compound
  ]
).

/** <module> GraphViz DOT generator

DCG rules for GraphViz DOT file generation.

Methods for writing to the GraphViz DOT format.

In GraphViz vertices are called 'nodes'.

```abnf
attr_list = "[" [a_list] "]" [attr_list]
a_list = ID "=" ID [","] [a_list]
```

---

@author Wouter Beek
@see http://www.graphviz.org/content/dot-language
@version 2013/07, 2013/09, 2014/03-2014/06, 2014/11-2014/12
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(ordsets)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_arrow)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(dcg_quote)).

:- use_module(plGraphViz(gv_attrs)).
:- use_module(plGraphViz(gv_html)).
:- use_module(plGraphViz(gv_numeral)).





%! gv_attr(
%!   +Context:oneof([edge,graph,node]),
%!   +Attribute:nvpair
%! )// is det.
% A single GraphViz attribute.
% We assume that the attribute has already been validated.

gv_attr(Context, Name=Value) -->
  gv_id(Name),
  "=",
  gv_attr_value(Context, Name=Value),
  ";".



%! gv_compass_pt(+Direction:oneof(['_',c,e,n,ne,nw,s,se,sw,w]))// .
% ```
% compass_pt : (n | ne | e | se | s | sw | w | nw | c | _)
% ```

gv_compass_pt('_') --> "_".
gv_compass_pt(c) --> "c".
gv_compass_pt(e) --> "e".
gv_compass_pt(n) --> "n".
gv_compass_pt(ne) --> "ne".
gv_compass_pt(nw) --> "nw".
gv_compass_pt(s) --> "s".
gv_compass_pt(se) --> "se".
gv_compass_pt(sw) --> "sw".
gv_compass_pt(w) --> "w".



%! gv_edge_operator(+Directed:boolean)// .
% The binary edge operator between two vertices.
% The operator that is used depends on whether the graph is directed or
% undirected.
%
% @arg Directed Whether an edge is directed (operator `->`) or
%                   undirected (operator `--`).

gv_edge_operator(false) --> !, "--".
gv_edge_operator(true) --> arrow(right, 2).



%! gv_edge_statement(
%!   +Indent:nonneg,
%!   +Directed:boolean,
%!   +EdgeTerm:compound
%! )// is det.
% A GraphViz statement describing an edge.
%
% @arg Indent The indentation level at which the edge statement is written.
% @arg Directed Whether the graph is directed or not.
% @arg GraphAttributes The attributes of the graph. Some of these attributes
%      may be used in the edge statement (e.g., the colorscheme).
% @arg EdgeTerm A compound term in the GIFormat, representing an edge.
%
% @tbd Instead of gv_node_id//1 we could have a gv_subgraph//1
%      at the from and/or to location.
% @tbd Add support for multiple, consecutive occurrences of gv_edge_rhs//2.

gv_edge_statement(I, Directed, edge(FromId,ToId,EAttrs)) -->
  indent(I),

  gv_node_id(FromId), " ",
  gv_edge_operator(Directed), " ",
  gv_node_id(ToId), " ",
  
  % We want `colorscheme/1` from the edges and
  % `directionality/1` from the graph.
  bracketed(square, '*'(gv_attr(edge), EAttrs, [])),
  line_feed.



%! gv_generic_attributes_statement(
%!   +Kind:oneof([edge,graph,node]),
%!   +Indent:integer,
%!   +CategoryAttributes:list(nvpair)
%! )//
% A GraphViz statement describing generic attributes for a category of items.
%
% @arg Kind The category of items for to the attributes apply.
%      Possible values: `edge`, `graph`, and `node`.
% @arg Indent An integer indicating the number of tabs.
% @arg GraphAttributes A list of name-value pairs.
% @arg CategoryAttributes A list of name-value pairs.
%
% ```
% attr_stmt = (graph / node / edge) attr_list
% ```

gv_generic_attributes_statement(_, _, []) --> [], !.
gv_generic_attributes_statement(Kind, I, KindAttrs) -->
  indent(I),
  gv_kind(Kind), " ",
  bracketed(square, '*'(gv_attr(Kind), KindAttrs, [])),
  line_feed.



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

gv_graph(graph(VTerms,ETerms,GAttrs)) -->
  gv_graph(graph(VTerms,[],ETerms,GAttrs)).

gv_graph(graph(VTerms,RankedVTerms,ETerms,GAttrs1)) -->
  {
    shared_attributes(VTerms, SharedVAttrs, NewVTerms),
    shared_attributes(ETerms, SharedEAttrs, NewETerms),
    select_nvpair(strict=Strict, GAttrs1, GAttrs2, false),
    select_nvpair(directed=Directed, GAttrs2, GAttrs3, true),
    select_nvpair(name=GName, GAttrs3, GAttrs4, noname),
    add_default_nvpair(GAttrs4, overlap, false, GAttrs5),
    I = 0
  },

  % The first statement in the GraphViz output.
  % States that this file represents a graph according to the GraphViz format.
  indent(I),
  gv_strict(Strict),
  gv_graph_type(Directed), " ",
  gv_id(GName), " ",

  % The body of the DOT file appears between curly braces.
  bracketed(
    curly,
    gv_graph0(
      I,
      NewVTerms, SharedVAttrs, RankedVTerms,
      NewETerms, SharedEAttrs,
      Directed, GAttrs5
    )
  ),
  line_feed.

gv_graph0(
  I,
  NewVTerms, SharedVAttrs, RankedVTerms,
  NewETerms, SharedEAttrs,
  Directed, GAttrs
) -->
  line_feed,

  % The following lines are indented.
  {NewI is I + 1},

  % Attributes that apply to the graph as a whole.
  gv_generic_attributes_statement(graph, NewI, GAttrs),

  % Attributes that are the same for all nodes.
  gv_generic_attributes_statement(node, NewI, SharedVAttrs),

  % Attributes that are the same for all edges.
  gv_generic_attributes_statement(edge, NewI, SharedEAttrs),

  % Only add a line_feed if some content was already written
  % and some content is about to be written.
  (
    {
      % Succeeds if no content was written.
      (GAttrs == [], SharedVAttrs == [], SharedEAttrs == [])
    ;
      % Succeeds if no content is about to be written.
      (NewVTerms == [], RankedVTerms == [])
    }
  ->
    ""
  ;
    line_feed
  ),

  % The list of GraphViz nodes.
  '*'(gv_node_statement(NewI), NewVTerms, []),
  ({NewVTerms == []} -> "" ; line_feed),

  % The ranked GraphViz nodes (displayed at the same height).
  '*'(gv_ranked_node_collection(NewI), RankedVTerms, []),
  ({RankedVTerms == []} -> "" ; line_feed),

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
  '*'(gv_edge_statement(NewI, Directed), RankEdges, []),

  % The non-rank edges.
  '*'(gv_edge_statement(NewI, Directed), NewETerms, []),

  % Note that we do not include a line_feed here.

  % We want to indent the closing curly brace.
  indent(I).



%! gv_graph_type(+Directed:boolean)// .
% The type of graph that is represented.

gv_graph_type(false) --> "graph".
gv_graph_type(true) --> "digraph".



%! gv_id(?Atom:compound)// is det.
% Parse a GraphViz identifier.
% There are 4 variants:
%   1. Any string of alphabetic (`[a-zA-Z'200-'377]`) characters,
%      underscores (`_`) or digits (`[0-9]`), not beginning with a digit.
%   2. A numeral `[-]?(.[0-9]+ | [0-9]+(.[0-9]*)? )`.
%   3. Any double-quoted string (`"..."`) possibly containing
%      escaped quotes (`\"`).
%      In quoted strings in DOT, the only escaped character is
%      double-quote (`"`). That is, in quoted strings, the dyad `\"`
%      is converted to `"`. All other characters are left unchanged.
%      In particular, `\\` remains `\\`.
%      Layout engines may apply additional escape sequences.
%      Represented by a Prolog term of the form `double_quoted_string(ATOM)`.
%   4. An HTML string (`<...>`).
%      Represented by a Prolog term of the form `html_like_label(COMPOUND)`.
%
% @tbd Add support for HTML-like labels:
%      http://www.graphviz.org/doc/info/shapes.html#html
%      This requires an XML grammar!

% HTML strings (assumed to be the same as HTML-like labels).
gv_id(html_like_label(Content)) --> !,
  gv_html_like_label(Content).
% Double-quoted strings.
% The quotes are already part of the given atom.
gv_id(double_quoted_string(Atom)) --> !,
  quoted(atom(Atom)).
% Numerals.
gv_id(N) -->
  {number(N)}, !,
  gv_numeral(N).
% Alpha-numeric strings.
gv_id(Atom) -->
  {atom_codes(Atom, [H|T])},
  gv_id_first(H),
  gv_id_rest(T), !,
  % Variant 1 identifiers should not be (case-variants of) a
  % GraphViz keyword.
  {\+ gv_keyword([H|T])}.

gv_id_first(X) --> ascii_letter(X).
gv_id_first(X) --> underscore(X).

gv_id_rest([]) --> [].
gv_id_rest([H|T]) -->
  (   ascii_alpha_numeric(H)
  ;   underscore(H)
  ),
  gv_id_rest(T).



%! gv_keyword(+Codes:list(code)) is semidet.
% Succeeds if the given codes for a GraphViz reserved keyword.

gv_keyword(Codes):-
  % Obviously, the keywords do not occur on the difference list input.
  % So we must use phrase/[2,3].
  phrase(gv_keyword, Codes).

%! gv_keyword// .
% GraphViz has reserved keywords that cannot be used as identifiers.
% GraphViz keywords are case-insensitive.

gv_keyword --> "digraph".
gv_keyword --> "edge".
gv_keyword --> "graph".
gv_keyword --> "node".
gv_keyword --> "strict".
gv_keyword --> "subgraph".



%! gv_kind(+Kind:oneof([edge,graph,node]))// .

gv_kind(edge) --> "edge".
gv_kind(graph) --> "graph".
gv_kind(node) --> "node".



%! gv_node_id(+NodeId:compound)// .
% GraphViz node identifiers can be of the following two types:
%   1. A GraphViz identifier, see gv_id//1.
%   2. A GraphViz identifier plus a GraphViz port indicator, see gv_port//0.
%
% @tbd Add support for GraphViz port indicators
%      inside GraphViz node identifiers.

gv_node_id(Id) -->
  gv_id(Id).
%gv_node_id(_) -->
%  gv_id(_),
%  gv_port.



%! gv_node_statement(+Indent:nonneg, +VertexTerm:compound)// .
% A GraphViz statement describing a vertex (GraphViz calls vertices 'nodes').

gv_node_statement(I, vertex(Id,VAttrs)) -->
  indent(I),
  gv_node_id(Id), " ",
  bracketed(square, '*'(gv_attr(node), VAttrs, [])),
  line_feed.



gv_port -->
  gv_port_location,
  '?'(gv_port_angle, []).
gv_port -->
  gv_port_angle,
  '?'(gv_port_location, []).
gv_port -->
  ":",
  gv_compass_pt(_).

gv_port_angle -->
  "@",
  gv_id(_).

gv_port_location -->
  ":",
  gv_id(_).
gv_port_location -->
  ":",
  bracketed(
    round,
    (
      gv_id(_),
      ",",
      gv_id(_)
    )
  ).



gv_ranked_node_collection(I, rank(Rank_V_Term,Content_V_Terms)) -->
  indent(I),
  bracketed(curly, (
    line_feed,

    % The rank attribute.
    {NewI is I + 1},
    indent(NewI),
    gv_attr(subgraph, rank=same),
    ";",
    line_feed,

    % Vertice statements.
    '*'(
      gv_node_statement(NewI),
      [Rank_V_Term|Content_V_Terms],
      []
    ),

    % We want to indent the closing curly brace.
    indent(I)
  )),
  line_feed.



%! gv_strict(+Strict:boolean)// is det.
% The keyword denoting that the graph is strict, i.e., has no self-arcs and
% no multi-edges.
% This only applies to directed graphs.

gv_strict(false) --> [].
gv_strict(true) --> "strict ".





% HELPERS

add_default_nvpair(Attrs1, N, Default, Attrs2):-
  add_default_nvpair(Attrs1, N, Default, _, Attrs2).

add_default_nvpair(Attrs, N, _, V, Attrs):-
  memberchk(N=V, Attrs), !.
add_default_nvpair(Attrs1, N, Default, Default, Attrs2):-
  ord_add_element(Attrs1, N=Default, Attrs2).

select_nvpair(N=V, Attrs1, Attrs2, _):-
  memberchk(N=V, Attrs1), !,
  select(N=V, Attrs1, Attrs2).
select_nvpair(_=Default, Attrs, Attrs, Default).


extract_shared([], []):- !.
extract_shared(Argss, Shared):-
  ord_intersection(Argss, Shared).

remove_shared_attributes(Shared, Args1, Args2):-
  ord_subtract(Args1, Shared, Args2).

shared_attributes(Terms1, SharedAs, Terms2):-
  maplist(term_to_attrs, Terms1, As1),
  extract_shared(As1, SharedAs),
  maplist(remove_shared_attributes(SharedAs), As1, As2),
  maplist(term_change_attrs, Terms1, As2, Terms2).

term_change_attrs(edge(From,To,_), A, edge(From,To,A)).
term_change_attrs(vertex(Id,_), A, vertex(Id,A)).

term_to_attrs(edge(_,_,A), A).
term_to_attrs(vertex(_,A), A).

