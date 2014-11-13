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
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).



%! gv_numeral(?Value:number)// .
%
% # Syntax
%
% ```bnf
% ('-')? ( '.' [0-9]+ | [0-9]+ ( '.' [0-9]* )? )
% ```
%
% ## Generation
%
% In the generative case, three notations are allowed (also see below):
%   1. (non-zero integer-part)
%   2. (decimal-separator) (non-zero fractional)
%   3. (non-zero integer-part) (decimal-separator) (non-zero fractional)
%
% The following notations are explicitly excluded due to verbosity:
%   - (non-zero integer-part) (decimal-separator)
%   - (non-zero integer-part) (decimal-separator) (zero fractional)
%   - (zero integer-part) (decimal-separator) (non-zero fractional)
%
% The following notations are also excluded, but since they violate
%  well-formedness:
%   - (non-zero integer-part) (non-zero fractional)
%   - (decimal-separator)

gv_numeral(N) -->
  {var(N)}, !,
  sign(Sign),
  (   ".",
      '+'(decimal_digit, Fractional, [convert1(weights_decimal)]),
      {IntegerPart = 0}
  ;   '+'(decimal_digit, IntegerPart, [convert1(weights_decimal)]),
      (   ".",
          '*'(decimal_digit, Fractional, [convert1(weights_decimal)])
      ;   {Fractional = 0}
      )
  ),
  {   number_integer_parts(UnsignedN, IntegerPart, Fractional),
      N is copysign(UnsignedN, Sign)
  }.
gv_numeral(N) -->
  {number_sign_parts(N, Sign, UnsignedN)},
  sign(Sign),
  {number_integer_parts(UnsignedN, IntegerPart, Fractional)},
  (   % [1] The fractional is zero, so only write the integer part
      %     and do not write the decimal separator.
      {Fractional =:= 0}
  ->  integer(IntegerPart)
  ;   % [2] The integer part is zero, so only write the fractional part,
      %     preceded by the decimal separator.
      {IntegerPart =:= 0}
  ->  ".",
      integer(Fractional)
  ;   % [3] Both the integer part and the fractional are non-zero,
      %     so write both of them, with the decimal separator in-between.
      integer(IntegerPart),
      ".",
      integer(Fractional)
  ).
