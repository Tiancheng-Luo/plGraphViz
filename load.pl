% Load file for the plGraphViz library.

:- dynamic(user:project/3).
:- multifile(user:project/3).
   user:project(plGraphViz, 'GraphViz support for SWI-Prolog.').

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plDcg,
    plHtml
]).

