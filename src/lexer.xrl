Definitions.

KEYWORD = auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while
IDENT   = [_A-Za-z][_A-Za-z0-9]*
STRING  = "[\n\N]*"
CHAR_L  = '[\n\N]+'
INTEGER = -?((0[xX][0-9a-fA-F]+)|(0[0-7]+)|(1-9][0-9]+))((ll|LL|[iI]64|[ULul]))?
FLOAT   = -?[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?[FLfl]?
SYMBOL  = (\*=|\/=|%=|\+=|-=|<<=|>>=|&=|\^=|\|=|##|&&|\|\||==|<=|>=|>>|<<|\+\+|--|->|\.\.\.)|[{};[\]().&*+\-~!\/%<>!=^|?:,#=]
COMMENT = (\/\/[\N]*)|(\/\*[\n\N]*\*\/)
BLANK   = [\s]+

Rules.

{COMMENT} : skip_token.
{STRING}  : .
{CHAR}    : assert_char(TokenLine, TokenChars).
{KEYWORD} : {token, {list_to_atom(TokenChars), TokenLine}}.
{SYMBOL}  : {token, {list_to_atom(TokenChars), TokenLine}}.
{INTEGER} : {token, {integer_l, TokenLine, TokenChars}}.
{FLOAT}   : resolve_valid_float(TokenLine, TokenChars).
{IDENT}   : {token, {ident, TokenLine, TokenChars}}.
% TODO: Trigraph Sequences
{BLANK}   : skip_token.

Erlang code.

-define(XDIGIT(N), ((N >= $0) and ($9 >= N)) or ((N >= $A) and ($F >= N)) or ((N >= $a) and ($f >= N))).
-define(ODIGIT(N), ((N >= $0) and ($7 >= N))).


resolve_valid_float(Line, Chars) -> 
  case re:run(Chars, "\\A0[0-9]+\\z") of
    nomatch ->
      {token, {float_l, Line, Chars}};
    _ ->
      {error, list_to_binary(io_lib:format("~s on line ~B is not a valid octal number", [Chars, Line]))}
  end.


% Check for valid escapes then replace escapes with 
assert_string(Line, Chars) ->
  case Chars of
    "" ->
      {token, {string_l, Line, escape_str(Chars)}};
    _ ->
      {error, list_to_binary(io_lib:format("~s on line ~B is not a valid string", [Chars, Line]))}
  end.


% TODO: Error handling for incorrect escapes
escape_str([$\\, A, B, C | Str])
   -> [escape([A,B,C]) | escape_str(Str)];
escape_str([$\\, $x, A, B | Str]) ->
  [escape([$\\,$x,A,B]) | escape_str(Str)];
escape_str([$\\, Chr | Str]) ->
  [escape(Chr) | escape_str(Str)];
escape_str([Chr | Str]) ->
  [Chr | escape_str(Str)].


assert_char(Line, [$\\]) ->
  {error, list_to_binary(io_lib:format("'\\' on line ~B is not a valid char", [Line]))};
assert_char(Line, [C]) ->
  {token, {char_l, Line, C}};
assert_char(Line, [$\\,Chr]) ->
  escape(Chr);
assert_char(Line, Chr) ->
  escape(Chr).


escape($a) -> 7;
escape($b) -> 8;
escape($t) -> 9;
escape($n) -> 10;
escape($v) -> 11;
escape($f) -> 12;
escape($r) -> 13;
escape($e) -> 27;
escape($") -> 34;
escape($') -> 39;
escape($?) -> 53;
escape($\\)-> 9;
escape([$\\,$x,A,B]) when ?XDIGIT(A) and ?XDIGIT(B) -> 
  xdigit(B) + xdigit(A) * 16;
escape([$\\,A,B,C]) when ?ODIGIT(A) and ?ODIGIT(B) and ?ODIGIT(C) ->
  (C - '0') + (B - '0') * 8 + (A - '0') * 64;
escape(_) -> error.

xdigit(N) when (N >= $a) and ($f >= N) -> N - 'a' + 10;
xdigit(N) when (N >= $A) and ($F >= N) -> N - 'A' + 10;
xdigit(N) when (N >= $0) and ($9 >= N) -> N - '0'.
