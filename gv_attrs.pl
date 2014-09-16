:- module(
  gv_attrs,
  [
    gv_attr/3 % +Context:oneof([cluster,edge,graph,node,subgraph])
              % +Attr1:nvpair
              % +Attr2:nvpair
  ]
).

/** <module> GraphViz attributes v2

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(persistency)).
:- use_module(library(xpath)).

:- use_module(generics(db_ext)).
:- use_module(os(file_ext)).

:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_table)).

:- use_module(plGraphViz(gv_attr_type)). % DCGs implementing attribute types.

:- db_add_novel(user:prolog_file_type(log, logging)).

%! gv_attr(
%!   ?Name:atom,
%!   ?UsedBy:list(oneof([cluster,edge,graph,node,subgraph])),
%!   ?Types:list(atom),
%!   ?Default,
%!   ?Minimum,
%!   ?Notes:atom
%! ) is nondet.

:- persistent(
  gv_attr(
    name:atom,
    used_by:list(oneof([cluster,edge,graph,node,subgraph])),
    types:list(atom),
    default,
    minimum,
    notes:atom
  )
).

:- initialization(gv_attrs_init).



gv_attr(Context, N=V, N=V):-
  var(V), !,
  gv_attr(N, UsedBy, _, V, _, _),
  memberchk(Context, UsedBy).
gv_attr(Context, N=V1, N=V2):-
  gv_attr(N, UsedBy, Types, _, Minimum, _),
  memberchk(Context, UsedBy),
  member(Type, Types),
  (
    Type == style
  ->
    Dcg =.. [Type,Context,V1]
  ;
    Dcg =.. [Type,V1]
  ),
  once(dcg_phrase(Dcg, V2)),
  check_minimum(V1, Minimum).

check_minimum(_, ''):- !.
check_minimum(V, Min1):-
  atom_number(Min1, Min2),
  Min2 =< V.



% INITIALIZATION

%! assert_gv_attr_row(+Row:list(atom)) is det.

assert_gv_attr_row([Name,UsedBy1,Types1,Default1,Minimum,Notes]):-
  dcg_phrase(translate_usedby(UsedBy2), UsedBy1),
  once(dcg_phrase(translate_type(Types2), Types1)),
  sort(UsedBy2, UsedBy3),
  translate_default(Default1, Default2),
  assert_gv_attr(Name, UsedBy3, Types2, Default2, Minimum, Notes).


%! gv_attrs_downloads is det.
% Downloads the table describing GraphViz attributes from `graphviz.org`.

gv_attrs_download:-
  gv_attrs_url(Url),
  download_html(Url, Dom, [html_dialect(html4),verbose(silent)]),

  xpath(Dom, //table(@align=center), TableDom),
  % @tbd This does not work, since in `record_name(Element, Name)`,
  %      `Element` is a signleton list whereas a compound term is expected.
  %%%%xpath(Dom, /html/body/table, TableDom),

  html_to_table(TableDom, _, Rows),
  maplist(assert_gv_attr_row, Rows).


%! gv_attrs_file(-File:atom) is det.

gv_attrs_file(File):-
  absolute_file_name(
    data(gv_attrs),
    File,
    [access(write),file_type(logging)]
  ).


%! gv_attrs_init is det.

gv_attrs_init:-
  gv_attrs_file(File),
  safe_db_attach(File),
  file_age(File, Age),
  gv_attrs_update(Age).


%! gv_attrs_update(+Age:float) is det.

% The persistent store is still fresh.
gv_attrs_update(Age):-
  once(gv_attr(_, _, _, _, _, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
gv_attrs_update(_):-
  retractall_gv_attr(_, _, _, _, _, _),
  gv_attrs_download.


%! gv_attrs_url(-Url:url) is det.

gv_attrs_url('http://www.graphviz.org/doc/info/attrs.html').


%! safe_db_attach(+File:atom) is det.

safe_db_attach(File):-
  exists_file(File), !,
  db_attach(File, []).
safe_db_attach(File):-
  touch_file(File),
  safe_db_attach(File).


%! translate_default(+Default1:atom, -Default2:atom) is det.

% The empty string is represented by the empty atom.
translate_default('""', ''):- !.
% The absence of a default value is represented by an uninstantiated variable.
translate_default('<none>', _):- !.
translate_default(Default, Default).


%! translate_type(-Types:list(atom))// is det.

translate_type([H|T]) -->
  {gv_attr_type(H)},
  atom(H),
  whites,
  translate_type(T).
translate_type([]) --> !, [].


%! translated_usedby(
%!   -UsedBy:list(oneof([cluster,edge,graph,node,subgraph]))
%! )// is det.

translate_usedby([cluster|T]) --> `C`, !, translate_usedby(T).
translate_usedby([edge|T]) --> `E`, !, translate_usedby(T).
translate_usedby([graph|T]) --> `G`, !, translate_usedby(T).
translate_usedby([node|T]) --> `N`, !, translate_usedby(T).
translate_usedby([subgraph|T]) --> `S`, !, translate_usedby(T).
translate_usedby([]) --> [].

