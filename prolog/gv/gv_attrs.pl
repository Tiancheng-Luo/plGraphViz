:- module(
  gv_attrs,
  [
    gv_attr_value//2 % +Context:oneof([cluster,edge,graph,node,subgraph])
                     % +Attribute:nvpair
  ]
).
:- ensure_loaded(library('gv/gv_attrs.data')).

/** <module> GraphViz attributes

@author Wouter Beek
@version 2015/07-2015/08, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(gv/gv_attr_type), [gv_attr_type//1]).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).





%! gv_attr_value(
%!   +Context:oneof([cluster,edge,graph,node,subgraph]),
%!   +Attribute:compound
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
