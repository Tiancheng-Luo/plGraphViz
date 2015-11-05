:- module(
  fca_export,
  [
    fca_export/2, % +Context:compound
                  % ?File:atom
    fca_export/3 % +Context:compound
                 % ?File:atom
                 % :Options:list(compound)
  ]
).

/** <module> FCA export

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(fca/fca)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(concept_label(+,1,+,-)).
:- meta_predicate(concept_label(+,1,+,?,?)).
:- meta_predicate(fca_export(+,?,:)).

:- predicate_options(fca_export/3, 3, [
     concept_label(+oneof([attributes,both,objects])),
     object_label(+callable),
     pass_to(gv_export/3, 3)
   ]).

is_meta(object_label).





%! fca_export(+Context:compound, ?File:atom) is det.
% Wrapper around fca_export/3 with default options.

fca_export(Context, File):-
  fca_export(Context, File, []).


%! fca_export(+Context:compound, ?File:atom, :Options:list(compound)) is det.
% The following optios are supported:
%   * concept_label(+oneof([attributes,both,objects]))

fca_export(Context, File, Opts1):-
  fca_hasse(Context, Hasse),
  
  meta_options(is_meta, Opts1, Opts2),
  option(concept_label(Mode), Opts2, both),
  option(object_label(Object_1), Opts2, =),
  merge_options(
    [
      vertex_label(fca_export:concept_label(Mode, Object_1)),
      vertex_rank(fca:concept_cardinality)
    ],
    Opts2,
    Opts3
  ),
  build_export_graph(Hasse, ExportG, Opts3),

  gv_export(ExportG, File, Opts2).



%! concept_label(
%!   +Mode:oneof([attributes,both,objects]),
%!   :Object_1,
%!   +Concept:compound,
%!   -Label:atom
%! ) is det.

concept_label(Mode, Object_1, Concept, Lbl):-
  string_phrase(concept_label(Mode, Object_1, Concept), Lbl).

concept_label(attributes, _, concept(_,As)) -->
  set(As).
concept_label(both, Object_1, Concept) -->
  pair(
    concept_label(objects, Object_1, Concept),
    concept_label(attributes, Object_1, Concept)
  ).
concept_label(objects, Object_1, concept(Os,_)) -->
  set(Object_1, Os).
