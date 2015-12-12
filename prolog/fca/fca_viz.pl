:- module(
  fca_viz,
  [
    fca_export_graph/2, % +Context, -ExportGraph
    fca_export_graph/3, % +Context:compound
                        % -ExportGraph:compound
                        % :Options:list(compound)
    fca_viz/2, % +Context, ?File
    fca_viz/3 % +Context:compound
              % ?File:atom
              % :Options:list(compound)
  ]
).

/** <module> FCA visualization

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(fca/fca)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(concept_label(+,1,1,+,-)).
:- meta_predicate(concept_label(+,1,1,+,?,?)).
:- meta_predicate(fca_export_graph(+,?,:)).
:- meta_predicate(fca_viz(+,?,:)).

:- predicate_options(fca_export_graph/3, 3, [
     attribute_label(+callable),
     concept_label(+oneof([attributes,both,objects])),
     object_label(+callable)
   ]).
:- predicate_options(fca_viz/3, 3, [
     pass_to(fca_export_graph/3, 3),
     pass_to(gv_export/3, 3)
   ]).

is_meta(attribute_label).
is_meta(object_label).




%! fca_export_graph(+Context:compound, -ExportGraph:compound) is det.
% Wrapper around fca_export_graph/3 with default options.

fca_export_graph(Context, ExportG):-
  fca_export_graph(Context, ExportG, []).


%! fca_export_graph(
%!   +Context:compound,
%!   -ExportGraph:compound,
%!   :Options:list(compound)
%! ) is det.
% The following optios are supported:
%   * attribute_label(+callable)
%     DCG writing the labels for individual attributes.
%     Default is pl_term//1.
%   * concept_label(+oneof([attributes,both,objects]))
%     Determines which components of the concepts are displayed
%     in the export graph.
%     Default is `both`.
%   * object_label(+callable)
%     DCG writing the labels for individual objects.
%     Default is pl_term//1.

fca_export_graph(Context, ExportG, Opts1):-
  fca_hasse(Context, Hasse),
  meta_options(is_meta, Opts1, Opts2),
  option(attribute_label(ALbl_1), Opts2, pl_term),
  option(concept_label(Mode), Opts2, both),
  option(object_label(OLbl_1), Opts2, pl_term),
  merge_options(
    [
      vertex_label(fca_viz:concept_label(Mode, OLbl_1, ALbl_1)),
      vertex_rank(fca:concept_cardinality)
    ],
    Opts2,
    Opts3
  ),
  build_export_graph(Hasse, ExportG, Opts3).



%! fca_viz(+Context:compound, ?File:atom) is det.
% Wrapper around fca_viz/3 with default options.

fca_viz(Context, File):-
  fca_viz(Context, File, []).


%! fca_viz(+Context:compound, ?File:atom, :Options:list(compound)) is det.

fca_viz(Context, File, Opts1):-
  meta_options(is_meta, Opts1, Opts2),
  fca_export_graph(Context, ExportG, Opts2),
  gv_export(ExportG, File, Opts2).



%! concept_label(
%!   +Mode:oneof([attributes,both,objects]),
%!   :ObjectLabel_1,
%!   :AttributeLabel_1,
%!   +Concept:compound,
%!   -Label:atom
%! ) is det.

concept_label(Mode, OLbl_1, ALbl_1, Concept, Lbl):-
  string_phrase(concept_label(Mode, OLbl_1, ALbl_1, Concept), Lbl).


%! concept_label(
%!   +Mode:oneof([attributes,both,objects]),
%!   :ObjectLabel_1,
%!   :AttributeLabel_1,
%!   +Concept:compound
%! )// is det.

concept_label(attributes, _, ALbl_1, concept(_,As)) -->
  set(ALbl_1, As).
concept_label(both, OLbl_1, ALbl_1, Concept) -->
  pair(
    concept_label(objects, OLbl_1, ALbl_1, Concept),
    concept_label(attributes, OLbl_1, ALbl_1, Concept)
  ).
concept_label(objects, OLbl_1, _, concept(Os,_)) -->
  set(OLbl_1, Os).
