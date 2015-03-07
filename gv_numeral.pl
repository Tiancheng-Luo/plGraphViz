:- module(
  gv_numeral,
  [
    gv_numeral//1 % ?Value:number
  ]
).

/** <module> GraphViz numeral

@author Wouter Beek
@version 2014/05-2014/06, 2015/01
*/

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_abnf_common)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(math/math_ext)).
:- use_module(plc(math/radix)).
:- use_module(plc(math/rational_ext)).





%! gv_numeral(?Value:compound)// .
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
  (   {ground(N)}
  ->  '[-]?'(N),
      {N0 is abs(N)},
      {rational_parts(N0, I, F)},
      (   % [1] The fractional is zero, so only write the integer part
          %     and do not write the decimal separator.
          {F =:= 0}
      ->  {weights_nonneg(IW, I)},
          '[0-9]+'(IW)
      ;   % [2] The integer part is zero, so only write the fractional part,
          %     preceded by the decimal separator.
          {I =:= 0}
      ->  ".",
          {weights_fraction(FW, F)},
          '[0-9]*'(FW)
      ;   % [3] Both the integer part and the fractional are non-zero,
          %     so write both of them, with the decimal separator in-between.
          {weights_nonneg(IW, I)},
          '[0-9]+'(IW),
          ".",
          {weights_fraction(FW, F)},
          '[0-9]*'(FW)
      )
  ;   '[-]?'(Sg),
      (   ".",
          '[0-9]+'(FW),
          {IW = []}
      ;   '[0-9]+'(IW),
          (   ".",
              '[0-9]*'(FW)
          ;   {FW = []}
          )
      ),
      {rational_parts_weights(N0, IW, FW)},
      {N is copysign(N0, Sg)}
  ).
