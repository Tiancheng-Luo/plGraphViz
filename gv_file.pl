:- module(
  gv_file,
  [
    file_to_gv/2, % +File:atom
                  % +Options:list(nvpair)
    file_to_gv/3, % +FromFile:atom
                  % ?ToFile:atom
                  % +Options:list(nvpair)
    gif_to_gv_file/3, % +GraphInterchangeFormat:compound
                      % ?ToFile:atom
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
@version 2013/09, 2013/11-2014/01, 2014/05, 2014/07-2014/08
*/

:- use_module(library(option)).
:- use_module(library(process)).

:- use_module(generics(code_ext)).
:- use_module(generics(db_ext)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).

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

:- predicate_options(codes_to_gv_file/3, 3, [
     pass_to(file_to_gv/3, 3)
   ]).
:- predicate_options(file_to_gv/2, 2, [
     pass_to(file_to_gv/3, 3)
   ]).
:- predicate_options(file_to_gv/3, 3, [
     method(+oneof([dot,sfdp])),
     to_file_type(+oneof([dot,jpeg,pdf,svg,xdot]))
   ]).
:- predicate_options(gif_to_svg_dom/3, 3, [
     pass_to(gif_to_gv_file/3, 3)
   ]).
:- predicate_options(gif_to_gv_file/3, 3, [
     pass_to(codes_to_gv_file/3, 3)
   ]).



%! codes_to_gv_file(
%!   +Codes:list(code),
%!   ?ToFile:atom,
%!   +Options:list(nvpair)
%! ) is det.

codes_to_gv_file(Codes, ToFile, Options):-
  absolute_file_name(data(tmp), TmpFile, [access(write),file_type(dot)]),
  setup_call_cleanup(
    open(TmpFile, write, Write, [encoding(utf8)]),
    put_codes(Write, Codes),
    close(Write)
  ),
  file_to_gv(TmpFile, ToFile, Options).


%! file_to_gv(+FromFile:atom, +Options:list(nvpair)) is det.

file_to_gv(FromFile, Options):-
  file_to_gv(FromFile, _, Options).

%! file_to_gv(+FromFile:atom, ?ToFile:atom, +Options:list(nvpair)) is det.
% Converts a GraphViz DOT file to an image file, using a specific
% visualization method.

file_to_gv(FromFile, ToFile, Options):-
  option(to_file_type(dot), Options), !,
  rename_file(FromFile, ToFile).
file_to_gv(FromFile, ToFile, Options):-
  % The method option.
  option(method(Method), Options, dot),
  must_be(oneof([dot,sfdp]), Method),

  % The file type option.
  option(to_file_type(ToFileType), Options, pdf),
  prolog_file_type(ToExtension, ToFileType),
  prolog_file_type(ToExtension, graphviz_output), !,

  % The output file is either given or created.
  (   var(ToFile)
  ->  user:prolog_file_type(ToExtension, ToFileType),
      file_alternative(FromFile, _, _, ToExtension, ToFile)
  ;   is_absolute_file_name(ToFile),
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
    % @tbd Windows hack:
    %%%%'C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe',
    [OutputType,FromFile,'-o',ToFile],
    [process(PID)]
  ),
  process_wait(PID, exit(ShellStatus)),
  exit_code_handler('GraphViz', ShellStatus).


%! gif_to_gv_file(+Gif:compound, ?ToFile:atom, +Options:list(nvpair)) is det.
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
  codes_to_gv_file(Codes, ToFile, Options).


%! open_dot(+File:atom) is det.
% Opens the given DOT file.
%
% @tbd Test support on Windows.
% @tbd Test support on OS-X.

open_dot(File):-
  once(find_program_by_file_type(dot, Program)),
  run_program(Program, [File]).

