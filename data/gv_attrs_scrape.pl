:- module(
  gv_attrs_scrape,
  [
    gv_attrs_scrape/1 % +File
  ]
).

/** <module> GraphViz: Scrape attributes

Writes compound terms of the following form to file:

```prolog
gv_attr(
  ?Name,
  ?UsedBy:list(oneof([cluster,edge,graph,node,subgraph])),
  ?Types:list(atom),
  ?Default,
  ?Minimum,
  ?Notes
) is nondet.
```

@author Wouter Beek
@version 2015/10, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(gv/gv_attr_type)).
:- use_module(library(http/http_download)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(pl_term)).
:- use_module(library(print_ext)).
:- use_module(library(xpath)).
:- use_module(library(xpath/xpath_table)).
:- use_module(library(yall)).





%! gv_attrs_scrape(+File) is det.

gv_attrs_scrape(File):-
  debug(gv, "Updating the GraphViz attributes table.", []),
  call_to_stream(File, [In,Meta,Meta]>>gv_attrs_download(In)).


gv_attrs_download(Write):-
  gv_attrs_iri(Iri),
  html_download(Iri, Dom),
  xpath_chk(Dom, //table(@align=lower_case(center)), TableDom),
  xpath_table(TableDom, _, Rows),
  maplist(write_attr_row(Write), Rows).


write_attr_row(Write, [Name,UsedBy1,Types1,Default1,Minimum,Notes]):-
  atom_phrase(translate_usedby(UsedBy2), UsedBy1),
  once(atom_phrase(translate_type(Types2), Types1)),
  sort(UsedBy2, UsedBy3),
  translate_default(Default1, Default2),
  with_output_to(
    Write,
    write_fact(gv_attr(Name, UsedBy3, Types2, Default2, Minimum, Notes))
  ).

gv_attrs_iri('http://www.graphviz.org/doc/info/attrs.html').





% HELPERS %

%! translate_default(+Default1, -Default2) is det.

% The empty string is represented by the empty atom.
translate_default('""', ''):- !.
% The absence of a default value is represented by an uninstantiated variable.
translate_default('<none>', _):- !.
translate_default(Default, Default).



%! translate_type(-Types:list(atom))// is det.

translate_type([H|T]) -->
  gv_attr_type(H),
  whites,
  translate_type(T).
translate_type([H]) -->
  gv_attr_type(H).
translate_type([]) --> "".



%! translated_usedby(
%!   -UsedBy:list(oneof([cluster,edge,graph,node,subgraph]))
%! )// is det.

translate_usedby([cluster|T]) -->
  "C", !,
  translate_usedby(T).
translate_usedby([edge|T]) -->
  "E", !,
  translate_usedby(T).
translate_usedby([graph|T]) -->
  "G", !,
  translate_usedby(T).
translate_usedby([node|T]) -->
  "N", !,
  translate_usedby(T).
translate_usedby([subgraph|T]) -->
  "S", !,
  translate_usedby(T).
translate_usedby([]) -->
  "".
