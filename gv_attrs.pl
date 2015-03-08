:- module(
  gv_attrs,
  [
    gv_attr_value//2 % +Context:oneof([cluster,edge,graph,node,subgraph])
                     % +Attribute:nvpair
  ]
).

/** <module> GraphViz: Attributes

Support for GraphViz attributes.

@author Wouter Beek
@version 2014/06, 2014/11-2014/12, 2015/03
*/

:- use_module(library(apply)).
:- use_module(library(persistency)).
:- use_module(library(xpath)).

:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/db_ext)).
:- use_module(plc(generics/print_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(io/file_gnu)).

:- use_module(plHtml(html)).
:- use_module(plHtml(elements/html_table)).

:- use_module(plGraphViz(gv_attr_type), [gv_attr_type/1]).

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





%! gv_attr_value(
%!   +Context:oneof([cluster,edge,graph,node,subgraph]),
%!   +Attribute:nvpair
%! )// is det.
% Uses the default value in case Value is uninstantiated.
% Otherwise, performs a typecheck and converts the given value.

% Use the default if no value is given.
gv_attr_value(Context, Name=Value) -->
  {
    var(Value), !,
    gv_attr(Name, UsedBy, _, DefaultValue, _, _),
    % Check validity of context.
    memberchk(Context, UsedBy)
  },
  gv_attr_value(Context, Name=DefaultValue).
gv_attr_value(Context, Name=Value) -->
  {
    % Check the validity of the context argument.
    gv_attr(Name, UsedBy, Types, _, Minimum, _),
    memberchk(Context, UsedBy),

    %  Pick a value type non-deterministically.
    member(Type, Types),

    % The `style` type is the only one that requires the context argument.
    (   Type == style
    ->  Dcg =.. [Type,Context]
    ;   Dcg =.. [Type]
    ),

    % Check validity of Value w.r.t. minimum value -- if available.
    check_minimum(Value, Minimum)
  },
  dcg_call(gv_attr_type:Dcg, Value).





% HELPERS %

%! check_minimum(+Value:atom, +Minimum:number) is semidet.
% Trivially succeeds if no minimum value is available for a given attribute.

check_minimum(_, ''):- !.
check_minimum(V, Min1):-
  atom_number(Min1, Min2),
  Min2 =< V.





% INITIALIZATION %

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
  report_on_process(
    'Updating GraphViz attributes table... ',
    (
      gv_attrs_uri(Uri),
      download_html_dom(Uri, Dom, [dialect(html5),silent(true)]),
      xpath(Dom, //table(@align=lower_case(center)), TableDom),
      html_to_table(TableDom, _, Rows),
      maplist(assert_gv_attr_row, Rows)
    )
  ).


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


%! gv_attrs_uri(-Url:url) is det.

gv_attrs_uri('http://www.graphviz.org/doc/info/attrs.html').


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

