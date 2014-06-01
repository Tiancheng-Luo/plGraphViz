% Load file for plGraphViz.

:- multifile(user:prolog/3).
:- dynamic(user:prolog/3).
user:project(plGraphViz, 'GraphViz support for SWI-Prolog.', plGraphViz).

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plHtml
]).

