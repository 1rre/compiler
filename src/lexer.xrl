Definitions.

KEYWORD = auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while
IDENT   = [_A-Za-z][_A-Za-z0-9]*
STRING  = "((\\")|[^"])*"
CHAR_L  = '((\\')|[^'])*'
INTEGER = -?((0[xX][0-9a-fA-F]+)|(0[0-7]*)|([1-9][0-9]*))((ll|LL|[iI]64|[ULul])?)
FLOAT   = -?[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?[FLfl]?
SYMBOL  = (\*=|\/=|%=|\+=|-=|<<=|>>=|&=|\^=|\|=|&&|\|\||==|<=|>=|>>|<<|\+\+|--|->|\.\.\.)|[{};[\]().&*+\-~!\/%<>!=^|?:,=]
COMMENT = (\/\/[\N]*)|(\/\*[\n\N]*\*\/)
BLANK   = [\s\v\t\n\r]+
DIRECTIVE = #[\s\v\t]*(include|pragma|define|error|warning|undef|if|ifdef|ifndef|else|elif|endif|line)

Rules.

{COMMENT}   : skip_token.
{STRING}    : escape_str(TokenLine, lists:droplast(tl(TokenChars))).
L{STRING}   : escape_str(TokenLine, lists:droplast(tl(tl(TokenChars)))). % Extended string, not using for now
{BLANK}?{DIRECTIVE}[^_a-zA-Z\n]+[^\n]* : resolve_directive(TokenChars).      % TODO: Ensure that the directives are on their own line
{CHAR}      : assert_char(TokenLine, lists:droplast(tl(TokenChars))).
{KEYWORD}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{SYMBOL}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{INTEGER}   : {token, {integer_l, TokenLine, TokenChars}}.
{FLOAT}     : resolve_valid_float(TokenLine, TokenChars).
{IDENT}     : {token, {ident, TokenLine, TokenChars}}.
% TODO: Trigraph Sequences
{BLANK}     : skip_token.

Erlang code.

-define(XDIGIT(N), ((N >= $0) and ($9 >= N)) or ((N >= $A) and ($F >= N)) or ((N >= $a) and ($f >= N))).
-define(ODIGIT(N), ((N >= $0) and ($7 >= N))).
-define(I_ESCAPE(L,C), {error, list_to_binary(io_lib:format("'\\~s' on line ~B is not a valid escape", [C, L]))}).

resolve_directive(Chars) -> {error, <<"Not Implemented.">>}. % TODO: Implement. It may well be a good idea to use a sub-lexer for this?

resolve_valid_float(Line, Chars) -> 
  case re:run(Chars, "\\A0[0-9]+\\z") of
    nomatch ->
      {token, {float_l, Line, Chars}};
    _ ->
      {error, list_to_binary(io_lib:format("~s on line ~B is not a valid octal number", [Chars, Line]))}
  end.

% TODO: Error handling for incorrect escapes
escape_str(TokenLine, []) -> {token, {string_l, TokenLine, ""}};
escape_str(TokenLine, [$\\,A,B,C | Str]) when ?ODIGIT(A) ->
  case {escape_str(TokenLine, Str), escape([$\\,A,B,C])} of
    {_, error} -> {error, ?I_ESCAPE(TokenLine, [A,B,C])};
    {{token, {string_l, _, S}}, Chr} -> {token, {string_l, TokenLine, [Chr | S]}};
    {Err, _} -> Err
  end;
escape_str(TokenLine, [$\\,$x,A,B | Str])  ->
  case {escape_str(TokenLine, Str), escape([$\\,A,B])} of
    {_, error} -> {error, ?I_ESCAPE(TokenLine, [A,B])};
    {{token, {string_l, _, S}}, Chr} -> {token, {string_l, TokenLine, [Chr | S]}};
    {Err, _} -> Err
  end;
escape_str(TokenLine, [$\\,A | Str])  ->
  case {escape_str(TokenLine, Str), escape(A)} of
    {_, error} -> {error, ?I_ESCAPE(TokenLine, [A])};
    {{token, {string_l, _, S}}, Chr} -> {token, {string_l, TokenLine, [Chr | S]}};
    {Err, _} -> Err
  end;
escape_str(TokenLine, [Chr | Str]) when Chr =/= $\\ ->
  case escape_str(TokenLine, Str) of
    {token, {string_l, _, S}} -> {token, {string_l, TokenLine, [Chr | S]}};
    Err -> Err
  end;
escape_str(Line, Str) ->
  {error, list_to_binary(io_lib:format("~s on line ~B is not a valid substring", [Str, Line]))}.

assert_char(Line, [$\\]) ->
  {error, ?I_ESCAPE(Line, "")};
assert_char(Line, [C]) ->
  {token, {char_l, Line, C}};
assert_char(Line, [$\\,Chr]) ->
  case escape(Chr) of
    error -> ?I_ESCAPE(Line, [Chr]);
    Char  -> {token, {char_l, Line, Char}}
  end;
assert_char(Line, Chr) ->
  case escape(Chr) of
    error -> ?I_ESCAPE(Line, [Chr]);
    Char  -> {token, {char_l, Line, Char}}
  end.


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

