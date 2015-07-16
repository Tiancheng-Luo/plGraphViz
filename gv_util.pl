:- module(
  gv_util,
  [
    ascii_alpha//1, % ?X:code
    atom_phrase/2, % :Goal_1
                   % +Atom:atom
    atom_phrase/3, % :Goal_1
                   % +Atom1:atom
                   % -Atom2:atom
    file_age/2, % +File:atom
                % -Age:float
    html_download/2, % +Uri:atom
                     % -Dom:compound
    indent//1, % +N:nonneg
    persistent_db_attach/2, % +Module:atom
                            % +File:atom
    underscore//1, % ?X:code
    verbose_process/2 % +Process:atom
                      % :Goal_0
  ]
).

/** <module> GraphViz utilities

Utilities that should not be specific to the GraphViz library
but that are considered too small to publish independently.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(ansi_term)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- meta_predicate(atom_phrase(1,+)).
:- meta_predicate(atom_phrase(1,+,-)).
:- meta_predicate(dcg_call(//,?,?)).
:- meta_predicate(dcg_call(1,+,?,?)).
:- meta_predicate(http_get(+,1,+)).
:- meta_predicate(http_stream(+,1,+)).
:- meta_predicate(verbose_process(+,0)).



%! ascii_alpha(?X:code)// .

ascii_alpha(X) -->
  between_code(65, 90, X)
ascii_alpha(X) -->
  between_code(97, 122, X)



%! ascii_alpha_numeric(?X:code)// .

ascii_alpha_numeric(X) -->
  ascii_alpha(X).
ascii_alpha_numeric(X) -->
  ascii_numberic(X).



%! ascii_numeric(?X:code)// .

ascii_numeric(X) -->
  between_code(48, 57, X).



%! atom_phrase(:Goal_1, +Atom:atom) is det.

atom_phrase(Goal_1, A):-
  atom_codes(A, Cs),
  phrase(Goal_1, Cs).



%! atom_phrase(:Goal_1, +Atom1:atom, +Atom2:atom) is det.

atom_phrase(Goal_1, A1, A2):-
  atom_codes(A1, Cs1),
  phrase(Goal_1, Cs1, Cs2),
  atom_codes(A2, Cs2).



%! between_code(+Low:code, +High:code, ?X:code)// .

between_code(Low, High, X) -->
  [X],
  {between(Low, High, X)}.



%! dcg_call(:Dcg_0)// is det.

dcg_call(Dcg, X, Y):-
  call(Dcg, X, Y).

%! dcg_call(:Dcg_1, +Argument1)// is det.

dcg_call(Dcg, A1, X, Y):-
  call(Dcg, A1, X, Y).



%! file_age(+File:atom, -Age:float) is det.

file_age(File, Age):-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! html_download(+Uri:atom, -Dom:compound) is det.

html_download(Uri, Dom):-
  http_get(
    Uri,
    \Stream^load_html(Stream, Dom, [dialect(html5),max_errors(-1)]),
    copy
    []
  ).



%! http_get(+Uri:atom, :Success_1, +Options:list(compound)) is det.

http_get(Uri, Success_1, Opts0):-
  merge_options([method(get),status_code(Status)], Opts0, Opts),
  setup_call_cleanup(
    http_open(Uri, Stream, Opts),
    http_stream(Status, Success_1, Stream),
    close(Stream)
  ).

%! http_stream(+Status:between(100,599), :Success_1, +Read:stream) is det.

http_stream(Status, Success_1, Stream):-
  between(200, 299, Status), !,
  call(Success_1, Stream).
http_stream(Status, _, Stream):-
  format(user_error, '[STATUS ~D] ', [Status]),
  copy_stream_data(Stream, user_error),
  format(user_error, '\n', []).



%! html_table(+Dom:list(compound), -Header:list, -Data:list(list)) is det.

html_table(Dom, Header, Data):-
  ignore(html_table_row(header, Dom, Header)),
  findall(
    Data0,
    html_table_row(data, Table, Data0),
    Data
  ).

%! html_table_row(
%!   +Mode:oneof([data,header]),
%!   +Dom:list(compound),
%!   -Data:list
%! ) is det.

html_table_row(data, Dom, Row):- !,
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //td(normalize_space), Cell),
    Row
  ),
  Row \== [].
html_table_row(header, Dom, Row):-
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //th(normalize_space), Cell),
    Row
  ),
  Row \== [].



%! indent(+N:nonneg)// is det.

indent(0) --> !, "".
indent(N) -->
  " ",
  {succ(M, N)},
  indent(M).



%! persistent_db_attach(+Module:atom, +File:atom) is det.
% Safe attachement of a persistent database dump.
% This first make sure the given file exists.

persistent_db_attach(Mod, File):-
  exists_file(File), !,
  Mod:db_attach(File, []).
persistent_db_attach(Mod, File):-
  touch_file(File),
  persistent_db_attach(Mod, File).

touch_file(File):-
  create_process(path(touch), [file(File)], []).



%! underscore(?C:code)// .

underscore(95) -->
  [95].



%! verbose_process(+Name:atom, :Goal_0) is det.

verbose_process(Name, Goal_0):-
  setup_call_catcher_cleanup(
    start(Name),
    Goal_0,
    E,
    (   E == true
    ->  success(Name)
    ;   failure(Name)
    )
  ).

failure(Name):-
  warning('[FAILURE]'),
  normal(' Process '),
  normal(Name),
  normal(':'),
  nl,
  normal(E).

start(Name):-
  normal('Starting process '),
  normal(Name),
  normal('.').

success(Name):-
  success('[SUCCESS]'),
  normal(' Ending process '),
  normal(Name),
  normal('.').

emphasis(X):- ansi_format([bold], '~a', [X]).
normal(X):- ansi_format([], '~a', [X]).
notification(X):- ansi_format([bold,fg(yellow)], '~a', [X]).
success(X):- ansi_format([bold,fg(green)], '~a', [X]).
warning(X):- ansi_format([bold,fg(red)], '~a', [X]).
