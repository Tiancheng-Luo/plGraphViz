:- module(
  gv_file,
  [
    gif_to_gv_file/3, % +GraphInterchangeFormat:compound
                      % ?ToFile:atom
                      % +Options:list(nvpair)
    gif_to_svg_dom/3, % +GraphInterchangeFormat:compound
                      % -SvgDom:list(compound)
                      % +Options:list(nvpair)
    open_dot/1 % +File:file
  ]
).

/** <module> GraphViz file

Predicates for converting GIF-formatted terms
into GraphViz output files or SVG DOM structures.

Also converts between GraphViz DOT formatted files
and GraphViz output files or SVG DOM structures.

@author Wouter Beek
@version 2011-2013/09, 2013/11-2014/01, 2014/05, 2014/07
*/

:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(generics(codes_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(error_ext)).
:- use_module(generics(trees)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).
:- use_module(os(safe_file)).
:- use_module(svg(svg_file)).
:- use_module(ugraph(ugraph_export)).

:- use_module(plGraphViz(gv_dot)).

:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).

:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

% Register DOT.
:- db_add_novel(user:prolog_file_type(dot, dot)).
:- db_add_novel(user:prolog_file_type(dot, graphviz)).
:- db_add_novel(user:file_type_program(dot, dotty)).
:- db_add_novel(user:file_type_program(dot, dotx)).
:- db_add_novel(user:module_uses(gv_file, file_type(dot))).

% Register JPG/JPEG.
:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpeg, graphviz_output)).
:- db_add_novel(user:prolog_file_type(jpg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg, graphviz_output)).

% Register PDF.
:- db_add_novel(user:prolog_file_type(pdf, pdf)).
:- db_add_novel(user:prolog_file_type(pdf, graphviz_output)).

% Register PNG.
:- db_add_novel(user:prolog_file_type(png, png)).
:- db_add_novel(user:prolog_file_type(png, graphviz_output)).

% Register SVG.
:- db_add_novel(user:prolog_file_type(svg, graphviz_output)).
:- db_add_novel(user:prolog_file_type(svg, svg)).

% Register XDOT.
:- db_add_novel(user:prolog_file_type(xdot, graphviz_output)).
:- db_add_novel(user:prolog_file_type(xdot, xdot)).

:- predicate_options(gif_to_svg_dom/3, 3, [
     pass_to(gif_to_gv_file/3, 3)
   ]).
:- predicate_options(gif_to_gv_file/3, 3, [
     pass_to(to_gv_file/3, 3)
   ]).
:- predicate_options(to_gv_file/3, 3, [
     pass_to(convert_gv/3, 3)
   ]).
:- predicate_options(convert_gv/3, 3, [
     method(+oneof([dot,sfdp])),
     to_file_type(+oneof([dot,jpeg,pdf,svg,xdot]))
   ]).



%! gif_to_gv_file(+Gif:compound, -ToFile:atom, +Options:list(nvpair)) is det.
% Returns a file containing a GraphViz visualization of the given graph.
%
% The following options are supported:
%   * =|method(+Method:oneof([dot,sfdp])|=
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   * =|to_file_type(+FileType:oneof([dot,jpeg,pdf,svg,xdot])|=
%     The file type of the generated GraphViz file.
%     Default: `pdf`.

gif_to_gv_file(Gif, ToFile, Options):-
  once(phrase(gv_graph(Gif), Codes)),
  to_gv_file(Codes, ToFile, Options).


%! gif_to_svg_dom(
%!   +GraphInterchangeFormat:compound,
%!   -SvgDom:list(compound),
%!   +Options:list(nvpair)
%! ) is det.

gif_to_svg_dom(Gif, SvgDom, Options1):-
  % Make sure the file type of the output file is SvgDom.
  merge_options([to_file_type=svg], Options1, Options2),
  gif_to_gv_file(Gif, ToFile, Options2),
  file_to_svg(ToFile, SvgDom),
  safe_delete_file(ToFile).


%! open_dot(+File:atom) is det.
% Opens the given DOT file.
%
% @tbd Test support on Windows.
% @tbd Test support on OS-X.

open_dot(File):-
  once(find_program_by_file_type(dot, Program)),
  run_program(Program, [File]).



% SUPPORT PREDICATES %

%! convert_gv(+FromFile:atom, ?ToFile:atom, +Options:list(nvpair)) is det.
% Converts a GraphViz DOT file to an image file, using a specific
% visualization method.

convert_gv(FromFile, ToFile, Options):-
  option(to_file_type(dot), Options), !,
  rename_file(FromFile, ToFile).
convert_gv(FromFile, ToFile, Options):-
  % The input file must be readable.
  access_file(FromFile, read),

  % The method option.
  option(method(Method), Options, dot),
  must_be(oneof([dot,sfdp]), Method),

  % The file type option.
  option(to_file_type(ToFileType), Options, pdf),
  prolog_file_type(ToExtension, ToFileType),
  prolog_file_type(ToExtension, graphviz_output), !,

  % The output file is either given or created.
  (
    var(ToFile)
  ->
    absolute_file_name(
      data(export),
      ToFile,
      [access(write),file_type(ToFileType)]
    )
  ;
    is_absolute_file_name(ToFile),
    % The given output file must match a certain file extension.
    file_name_extension(_, ToExtension, ToFile)
  ),
  % Now that we have the output file we can prevent the
  % file type / file extension translation predicates from bakctracking.
  !,

  % Run the GraphViz conversion command in the shell.
  format(atom(OutputType), '-T~w', [ToExtension]),
  process_create(
    path(Method),
    [OutputType,FromFile,'-o',ToFile],
    [process(PID)]
  ),
  process_wait(PID, exit(ShellStatus)),
  exit_code_handler('GraphViz', ShellStatus).


%! to_gv_file(+Codes:list(code), ?ToFile:atom, +Options:list(nvpair)) is det.

to_gv_file(Codes, ToFile, Options):-
  absolute_file_name(
    data(tmp),
    FromFile,
    [access(write),file_type(graphviz)]
  ),
  setup_call_cleanup(
    open(FromFile, write, Out, [encoding(utf8),type(test)]),
    put_codes(Out, Codes),
    close(Out)
  ),
  convert_gv(FromFile, ToFile, Options),

  %%%%% DEB: Store DOT file.
  %%%%ignore((
  %%%%  file_type_alternative(ToFile, graphviz, DOT_File),
  %%%%  safe_copy_file(FromFile, DOT_File)
  %%%%)),

  safe_delete_file(FromFile).

