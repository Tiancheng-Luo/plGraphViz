:- module(
  gv_attrs_scrape,
  [
    gv_attrs_scrape/1 % +File:atom
  ]
).

/** <module> GraphViz: Scrape attributes

Writes compound terms of the following form to file:

```prolog
gv_attr(
  ?Name:atom,
  ?UsedBy:list(oneof([cluster,edge,graph,node,subgraph])),
  ?Types:list(atom),
  ?Default,
  ?Minimum,
  ?Notes:atom
) is nondet.
```

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(gv/gv_attr_type)).
:- use_module(library(http/http_download)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(msg_ext)).
:- use_module(library(pl/pl_term)).
:- use_module(library(xpath)).
:- use_module(library(xpath/xpath_table)).





%! gv_attrs_scrape(+File:atom) is det.

gv_attrs_scrape(File):-
  setup_call_cleanup(
    open_any(File, write, Write, Close, []),
    verbose(
      gv_attrs_download(Write),
      "Updating the GraphViz attributes table."
    ),
    close_any(Close)
  ).


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

%! translate_default(+Default1:atom, -Default2:atom) is det.

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
