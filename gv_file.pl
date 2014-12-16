:- module(
  gv_file,
  [
    file_to_gv/2, % +InputFile:atom
                  % +Options:list(nvpair)
    file_to_gv/3, % +InputFile:atom
                  % ?OutputFile:atom
                  % +Options:list(nvpair)
    export_graph_to_gv_file/3, % +ExportGraph:compound
                               % ?OutputFile:atom
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
@version 2013/09, 2013/11-2014/01, 2014/05, 2014/07-2014/08, 2014/11-2014/12
*/

:- use_module(library(option)).
:- use_module(library(process)).

:- use_module(generics(code_ext)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).

:- use_module(plGraphViz(gv_dot)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(dot, dot).
user:prolog_file_type(pdf, pdf).

:- predicate_options(codes_to_gv_file/3, 3, [
     pass_to(file_to_gv/3, 3)
   ]).
:- predicate_options(file_to_gv/2, 2, [
     pass_to(file_to_gv/3, 3)
   ]).
:- predicate_options(file_to_gv/3, 3, [
     method(+atom),
     output(+atom)
   ]).
:- predicate_options(export_graph_to_gv_file/3, 3, [
     pass_to(codes_to_gv_file/3, 3)
   ]).





%! codes_to_gv_file(
%!   +Codes:list(code),
%!   ?OutputFile:atom,
%!   +Options:list(nvpair)
%! ) is det.

codes_to_gv_file(Codes, OutputFile, Options):-
  absolute_file_name(data(tmp), TmpFile, [access(write),extensions([dot])]),
  setup_call_cleanup(
    open(TmpFile, write, Write, [encoding(utf8)]),
    put_codes(Write, Codes),
    close(Write)
  ),
  file_to_gv(TmpFile, OutputFile, Options).



%! file_to_gv(+InputFile:atom, +Options:list(nvpair)) is det.

file_to_gv(InputFile, Options):-
  file_to_gv(InputFile, _, Options).

%! file_to_gv(
%!   +InputFile:atom,
%!   ?OutputFile:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Converts a GraphViz DOT file to an image file, using a specific
% visualization method.

file_to_gv(InputFile, OutputFile, Options):-
  option(output(dot), Options), !,
  (   var(OutputFile)
  ->  OutputFile = InputFile
  ;   rename_file(InputFile, OutputFile)
  ).
file_to_gv(InputFile, OutputFile, Options):-
  % Typecheck for `method` option.
  option(method(Method), Options, dot),
  findall(Method0, gv_method(Method0), Methods),
  must_be(oneof(Methods), Method),

  % Typecheck for `output` option.
  option(output(OutputType), Options, pdf),
  findall(OutputType0, gv_output_type(OutputType0), OutputTypes),
  must_be(oneof(OutputTypes), OutputType),

  % The output file is either given or created.
  (   var(OutputFile)
  ->  file_alternative(InputFile, _, _, OutputType, OutputFile)
  ;   is_absolute_file_name(OutputFile)
  ),

  % Run the GraphViz conversion command in the shell.
  format(atom(OutputTypeFlag), '-T~a', [OutputType]),
  format(atom(OutputFileFlag), '-o~a', [OutputFile]),
  process_create(
    path(Method),
    % @tbd Windows hack:
    %%%%'C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe',
    [OutputTypeFlag,file(InputFile),OutputFileFlag],
    [process(PID)]
  ),
  process_wait(PID, exit(ShellStatus)),
  exit_code_handler('GraphViz', ShellStatus).



%! export_graph_to_gv_file(
%!   +ExportGraph:compound,
%!   ?OutputFile:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Returns a file containing a GraphViz visualization of the given graph.
%
% The following options are supported:
%   - `method(+Method:atom`
%     The algorithm used by GraphViz for positioning the tree nodes.
%     Either =dot= (default) or =sfdp=.
%   - `output(+FileType:atom)`
%     The file type of the generated GraphViz file.
%     Default: `pdf`.

export_graph_to_gv_file(ExportGraph, OutputFile, Options):-
  once(phrase(gv_graph(ExportGraph), Codes)),
  codes_to_gv_file(Codes, OutputFile, Options).



%! open_dot(+File:atom) is det.
% Opens the given DOT file.
%
% @tbd Test support on Windows.
% @tbd Test support on OS-X.

open_dot(File):-
  once(find_program_by_file_type(dot, Program)),
  run_program(Program, [File]).





% HELPERS

gv_method(circo).
gv_method(dot).
gv_method(fdp).
gv_method(neato).
gv_method(osage).
gv_method(sfdp).
gv_method(twopi).



gv_output_type(bmp).
gv_output_type(canon).
gv_output_type(dot).
gv_output_type(gv).
gv_output_type(xdot).
gv_output_type('xdot1.2').
gv_output_type('xdot1.4').
gv_output_type(cgimage).
gv_output_type(cmap).
gv_output_type(eps).
gv_output_type(exr).
gv_output_type(fig).
gv_output_type(gd).
gv_output_type(gd2).
gv_output_type(gif).
gv_output_type(gtk).
gv_output_type(ico).
gv_output_type(imap).
gv_output_type(cmapx).
gv_output_type(imap_np).
gv_output_type(cmapx_np).
gv_output_type(ismap).
gv_output_type(jp2).
gv_output_type(jpg).
gv_output_type(jpeg).
gv_output_type(jpe).
gv_output_type(pct).
gv_output_type(pict).
gv_output_type(pdf).
gv_output_type(pic).
gv_output_type(plain).
gv_output_type('plain-ext').
gv_output_type(png).
gv_output_type(pov).
gv_output_type(ps).
gv_output_type(ps2).
gv_output_type(psd).
gv_output_type(sgi).
gv_output_type(svg).
gv_output_type(svgz).
gv_output_type(tga).
gv_output_type(tif).
gv_output_type(tiff).
gv_output_type(tk).
gv_output_type(vml).
gv_output_type(vmlz).
gv_output_type(vrml).
gv_output_type(wbmp).
gv_output_type(webp).
gv_output_type(xlib).
gv_output_type(x11).
