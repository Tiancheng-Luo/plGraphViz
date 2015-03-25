% Standalone execution of the plGraphViz library.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- set_prolog_flag(verbose, silent).
  :- ensure_loaded(load).
:- endif.

:- use_module(plGraphViz(gv_file)).

