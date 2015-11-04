:- module(
  fca_export,
  [
    fca_export/2, % +Context:compound
                  % ?File:atom
    fca_export/3, % +Context:compound
                  % ?File:atom
                  % +Options:list(compound)
    fca_label_attributes/2, % +Concept:compound
                            % -Label:atom
    fca_label_concept/2, % +Concept:compound
                         % -Label:atom
    fca_label_objects/2 % +Concept:compound
                        % -Label:atom
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

:- predicate_options(fca_export/3, 3, [
     pass_to(build_export_graph/3, 3),
     pass_to(gv_export/3, 3)
   ]).





%! fca_export(+Context:compound, ?File:atom) is det.
% Wrapper around fca_export/3 with default options.

fca_export(Context, File):-
  fca_export(Context, File, []).


%! fca_export(+Context:compound, ?File:atom, +Options:list(compound)) is det.

fca_export(Context, File, Opts1):-
  fca_lattice(Context, Lattice1),
  %minimize(Lattice1, Lattice2),
  Lattice2 = Lattice1,
  option(concept_label(VLabel_2), Opts1, fca_export:fca_label_concept),
  merge_options(
    [vertex_label(VLabel_2),vertex_rank(fca:concept_cardinality)],
    Opts1,
    Opts2
  ),
  build_export_graph(Lattice2, ExportG, Opts2),
  gv_export(ExportG, File, Opts1).

minimize(G1, G2):-
  s_vertices(G1, Vs),
  aggregate_all(
    set(X-Y),
    (
      s_edge(G1, X-Y),
      maplist(concept_cardinality, [X,Y], [CX,CY]),
      CX < CY
    ),
    Es
  ),
  s_graph_components(G2, Vs, Es).



%! fca_label_attributes(+Concept:compound, -Label:atom) is det.
% Writes a concept label displaying its attributes.

fca_label_attributes(Concept, Lbl):-
  string_phrase(dcg_attributes(Concept), Lbl).

dcg_attributes(concept(_,As)) -->
  set(As).



%! fca_label_attributes(+Concept:compound, -Label:atom) is det.
% Writes a concept label display both its attributes and objects.

fca_label_concept(Concept, Lbl):-
  string_phrase(dcg_concept(Concept), Lbl).

dcg_concept(Concept) -->
  pair(dcg_objects(Concept), dcg_attributes(Concept)).



%! fca_label_objects(+Concept:compound, -Label:atom) is det.
% Writes a concept label displaying its object.

fca_label_objects(Concept, Lbl):-
  string_phrase(dcg_objects(Concept), Lbl).

dcg_objects(concept(Os,_)) -->
  set(Os).
