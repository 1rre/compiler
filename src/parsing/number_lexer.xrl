Definitions.
FRA = ((([0-9]+)(\.)([0-9]*))|(([0-9]*)(\.)([0-9]+)))(([eE]([+-]?)[0-9]+)?)
EXP = ([eE]([+-]?)[0-9]+)
OCT = [0][0-7]*
DEC = [1-9][0-9]*
HEX = 0[xX][0-9A-Fa-f]+
INS = ([uU][lL]?)|([lL][uU]?)
FLS = [fFlL]

Rules.

{FRA}({FLS}?) : {token, ffloat(TokenChars)}.
[0-9]+{EXP}({FLS}?) : {token, ifloat(TokenChars)}.
{OCT}({INS}?) : {token, oct(TokenChars)}.
{DEC}({INS}?) : {token, dec(TokenChars)}.
{HEX}({INS}?) : {token, hex(TokenChars)}.

Erlang code.

ffloat(Chars) ->
  {Str, Suffix} = lists:splitwith(fun (X) -> (X =/= $f) and (X =/= $F) and (X =/= $l) and (X =/= $L) end, [$0 | Chars]),
  case string:split(Str, "e") of
    [Man, Exp] -> {float_l,list_to_float(Man ++ [$0]) * math:pow(10, list_to_integer(Exp)), Suffix};
    [Man] -> {float_l, list_to_float(Man ++ [$0]), Suffix}
  end.

ifloat(Chars) ->
  {Str, Suffix} = lists:splitwith(fun (X) -> (X =/= $f) and (X =/= $F) and (X =/= $l) and (X =/= $L) end, [$0 | Chars]),
  [Man, Exp] = string:split(Str, "e"),
  {float_l, list_to_integer(Man) * math:pow(10, list_to_integer(Exp)), Suffix}.
oct(Chars) ->
  {Str, Suffix} = lists:splitwith(fun (X) -> (X =/= $u) and (X =/= $U) and (X =/= $l) and (X =/= $L) end, Chars),
  {int_l, list_to_integer(Str, 8), Suffix}.

dec(Chars) ->
  {Str, Suffix} = lists:splitwith(fun (X) -> (X =/= $u) and (X =/= $U) and (X =/= $l) and (X =/= $L) end, Chars),
  {int_l, list_to_integer(Str, 10), Suffix}.

hex([_,_|Chars]) ->
  {Str, Suffix} = lists:splitwith(fun (X) -> (X =/= $u) and (X =/= $U) and (X =/= $l) and (X =/= $L) end, Chars),
  {int_l, list_to_integer(Str, 16), Suffix}.
