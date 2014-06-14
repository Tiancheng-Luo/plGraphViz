% Load file for plGraphViz.

:- dynamic(user:prolog/3).
:- multifile(user:prolog/3).
   user:project(plGraphViz, 'GraphViz support for SWI-Prolog.', plGraphViz).

:- use_module(load_project).
:- load_project(plGraphViz, [
    plc-'Prolog-Library-Collection',
    plHtml
]).

