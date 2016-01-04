:- module(
  gv_graph_comp,
  [
    gv_edge_statement//3, % +Indent:nonneg
                          % +Directed:boolean
                          % +Edge:compound
    gv_generic_attributes_statement//3, % +Kind:oneof([edge,graph,node])
                                        % +Indent:nonneg
                                        % +CategoryAttributes:list(compound)
    gv_id//1, % +Name:compound
    gv_node_statement//2, % +Indent:nonneg
                          % +Vertex:compound
    gv_ranked_node_collection//2 % +Indent:nonneg
                                 % +Rank
  ]
).

/** <module> GraphViz graph components

```abnf
attr_list = "[" [a_list] "]" [attr_list]
a_list = ID "=" ID [","] [a_list]
```

@author Wouter Beek
@see http://www.graphviz.org/content/dot-language
@version 2015/07-2015/08, 2015/10-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(gv/gv_attrs)).
:- use_module(library(gv/gv_html)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).





%! gv_edge_statement(
%!   +Indent:nonneg,
%!   +Directed:boolean,
%!   +Edge:compound
%! )// is det.
% A GraphViz statement describing an edge.
%
% @arg Indent The indentation level at which the edge statement is written.
% @arg Directed Whether the graph is directed or not.
% @arg GraphAttributes The attributes of the graph. Some of these attributes
%      may be used in the edge statement (e.g., the colorscheme).
% @arg Edge A compound term in the GIFormat, representing an edge.
%
% @tbd Instead of gv_node_id//1 we could have a gv_subgraph//1
%      at the from and/or to location.
% @tbd Add support for multiple, consecutive occurrences of gv_edge_rhs//2.

gv_edge_statement(I, Dir, edge(From,To,Attrs)) -->
  tab(I),

  gv_node_id(From), " ",
  gv_edge_operator(Dir), " ",
  gv_node_id(To), " ",

  % We want `colorscheme/1` from the edges and
  % `directionality/1` from the graph.
  gv_attrs(edge, Attrs),
  "\n".

%! gv_edge_operator(+Directed:boolean)// .
% The binary edge operator between two vertices.
% The operator that is used depends on whether the graph is directed or
% undirected.
%
% @arg Directed Whether an edge is directed (operator `->`) or
%      undirected (operator `--`).

gv_edge_operator(Dir) -->
  {must_be(boolean, Dir)},
  ({Dir == false} -> "--" ; {Dir == true} -> "->").



%! gv_generic_attributes_statement(
%!   +Kind:oneof([edge,graph,node]),
%!   +Indent:nonneg,
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

gv_generic_attributes_statement(_, _, []) --> !, "".
gv_generic_attributes_statement(Kind, I, Attrs) -->
  tab(I),
  gv_kind(Kind), " ",
  gv_attrs(Kind, Attrs),
  "\n", !.



%! gv_kind(+Kind:oneof([edge,graph,node]))// .

gv_kind(Kind) --> {must_be(oneof([edge,graph,node]), Kind)}, atom(Kind).



%! gv_node_statement(+Indent:nonneg, +Vertex:compound)// is det.
% A GraphViz statement describing a vertex (GraphViz calls vertices 'nodes').

gv_node_statement(I, vertex(Id,Attrs)) -->
  tab(I),
  gv_node_id(Id),
  gv_attrs(node, Attrs),
  "\n".



%! gv_ranked_node_collection(+Indent:nonneg, Rank:pair)// is det.

gv_ranked_node_collection(I, RankVTerm-VTerms) -->
  tab(I),
  "{\n",

  % The rank attribute.
  {NewI is I + 1},
  tab(NewI),
  gv_attr(subgraph, rank(same)),
  "\n",

  % Vertice statements.
  *(gv_node_statement(NewI), [RankVTerm|VTerms]),

  % We want to indent the closing curly brace.
  tab(I),
  "\n}".





% HELPERS %

%! gv_attrs(
%!   +Kind:oneof([edge,graph,node]),
%!   +Attributes:list(compound)
%! )// is det.

gv_attrs(Kind, L) --> "[", *(gv_attr(Kind), L), "]".


%! gv_attr(+Context:oneof([edge,graph,node]), +Attribute:compound)// is det.
% A single GraphViz attribute.
% We assume that the attribute has already been validated.

gv_attr(Context, Attr) -->
  {Attr =.. [N,V]},
  gv_id(N), "=", gv_attr_value(Context, N=V), ";".



%! gv_id(+Id:compound)// is det.
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
  "\"", atom(Atom), "\"".
% Numerals.
gv_id(N) -->
  gv_numeral(N), !.
% Alpha-numeric strings.
gv_id(Atom) -->
  {atom_codes(Atom, [H|T])},
  gv_id_first(H),
  gv_id_rest(T), !,
  % Variant 1 identifiers should not be (case-variants of) a
  % GraphViz keyword.
  {\+ gv_keyword([H|T])}.


%! gv_id_first(+First:code)// is det.
% Generates the first character of a GraphViz identifier.

gv_id_first(C)   --> alpha(C), !.
gv_id_first(0'_) --> "_".


%! gv_id_rest(+NonFirst:code)// is det.
% Generates a non-first character of a GraphViz identifier.

gv_id_rest([H|T])   --> alphadigit(H), !, gv_id_rest(T).
gv_id_rest([0'_|T]) --> "_",           !, gv_id_rest(T).
gv_id_rest([])      --> "".



%! gv_keyword(+Codes:list(code)) is semidet.
% Succeeds if the given codes for a GraphViz reserved keyword.

gv_keyword(Cs):-
  % Obviously, the keywords do not occur on the difference list input.
  % So we must use phrase/[2,3].
  phrase(gv_keyword, Cs).


%! gv_keyword// .
% GraphViz has reserved keywords that cannot be used as identifiers.
% GraphViz keywords are case-insensitive.

gv_keyword --> "digraph".
gv_keyword --> "edge".
gv_keyword --> "graph".
gv_keyword --> "node".
gv_keyword --> "strict".
gv_keyword --> "subgraph".



%! gv_node_id(+NodeId:compound)// .
% GraphViz node identifiers can be of the following two types:
%   1. A GraphViz identifier, see gv_id//1.
%   2. A GraphViz identifier plus a GraphViz port indicator, see gv_port//0.
%
% @tbd Add support for GraphViz port indicators
%      inside GraphViz node identifiers.

gv_node_id(Id) --> gv_id(Id), !.
%gv_node_id(_) --> gv_id(_), gv_port.
gv_node_id(Id) --> {type_error(gv_node_id, Id)}.



%! gv_numeral(-Number)// is det.
% ```bnf
% ('-')? ( '.' [0-9]+ | [0-9]+ ( '.' [0-9]* )? )
% ```

gv_numeral(N) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  (   "."
  ->  {I = 0}, +(digit, Ds), {pos_frac(Ds, Frac)}
  ;   +(digit, Ds1), {pos_sum(Ds1, I)},
      ("." -> *(digit, Ds2), {pos_frac(Ds2, Frac)} ; {Frac = 0.0})
  ),
  {N is Sg * (I + Frac)}.



%! gv_port// is det.

gv_port --> gv_port_location, (gv_port_angle ; "").
gv_port --> gv_port_angle, (gv_port_location ; "").
gv_port --> ":", gv_compass_pt(_).

gv_port_angle --> "@", gv_id(_).

gv_port_location --> ":", gv_id(_).
gv_port_location --> ":[", gv_id(_), ",", gv_id(_), "]".



%! gv_compass_pt(+Direction:oneof(['_',c,e,n,ne,nw,s,se,sw,w]))// .
% ```
% compass_pt : ( n | ne | e | se | s | sw | w | nw | c | _ )
% ```

gv_compass_pt('_') --> "_".
gv_compass_pt(c)   --> "c".
gv_compass_pt(e)   --> "e".
gv_compass_pt(n)   --> "n".
gv_compass_pt(ne)  --> "ne".
gv_compass_pt(nw)  --> "nw".
gv_compass_pt(s)   --> "s".
gv_compass_pt(se)  --> "se".
gv_compass_pt(sw)  --> "sw".
gv_compass_pt(w)   --> "w".
