:- module(
  gv_numeral,
  [
    gv_numeral//1 % ?Value:number
  ]
).

/** <module> GraphViz numeral

@author Wouter Beek
@version 2014/05-2014/06
*/

:- use_module(math(math_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_cardinal)).



%! gv_numeral(?Value:number)// .
% ~~~{.bnf}
% ('-')? ( '.' [0-9]+ | [0-9]+ ( '.' [0-9]* )? )
% ~~~

gv_numeral(N) -->
  {nonvar(N)},
  {number_sign_parts(N, Sign, Abs)},
  ({Sign =:= -1} -> `-` ; ``),
  gv_numeral_abs(Abs).
gv_numeral(N) -->
  {var(N)},
  'sign?'(Sign),
  gv_numeral_abs(Abs),
  {number_sign_parts(N, Sign, Abs)}.


gv_numeral_abs(N) -->
  {nonvar(N)},
  {number_integer_parts(N, N1, N2)},
  (
    {N2 =:= 0}
  ->
    integer(N1)
  ;
    {N1 =:= 0}
  ->
    `.`,
    integer(N2)
  ;
    integer(N1),
    '?'((`.`, 'integer?'(N2)), [])
  ).
gv_numeral_abs(N) -->
  {var(N)},
  (
    `.`,
    integer(N2)
  ->
    {N1 = 0}
  ;
    integer(N1),
    (
      `.`
    ->
      'integer?'(N2)
    ;
      {N2 = 0}
    )
  ),
  {number_integer_parts(N, N1, N2)}.

