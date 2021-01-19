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
{STRING}  : {token, {string, TokenLine, TokenChars}}.
{CHAR}    : {token, {char_l, TokenLine, TokenChars}}.
{KEYWORD} : {token, {list_to_atom(TokenChars), TokenLine}}.
{SYMBOL}  : {token, {list_to_atom(TokenChars), TokenLine}}.
{INTEGER} : {token, {number, TokenLine, TokenChars}}.
{FLOAT}   : resolve_valid_float(TokenChars, TokenLine).
{IDENT}   : {token, {ident, TokenLine, TokenChars}}.
% TODO: Trigraph Sequences
{BLANK}   : skip_token.

Erlang code.

resolve_valid_float(Chars, Line) -> 
  case re:run(Chars, "\\A0[0-9]+\\z") of
    nomatch -> {token, {number, Line, Chars}};
    _       -> {error, list_to_binary(io_lib:format("~s on line ~B is not a valid octal number~n", [Chars, Line]))}
  end.