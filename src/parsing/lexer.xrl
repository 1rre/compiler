Definitions.

COMMENT = (\/\/[^\n]*)|(\/\*(.|[\n\s])*\*\/)

KEYWORD = auto|double|int|struct|break|else|long|switch|case|enum|register|typedef|char|extern|return|union|const|float|short|unsigned|continue|for|signed|void|default|goto|sizeof|volatile|do|if|static|while
IDENT   = [_a-zA-Z]([_a-zA-Z0-9]*)
NUMBER  = ((\.?[0-9]+)[a-zA-Z_.]*([eE][+\-])?)+
C_CHAR  = [^\n'\\]
S_CHAR  = [^\n"\\]
OCTAL_ESCAPE = \\[0-7][0-7]?[0-7]?
HEX_ESCAPE = \\x[0-9a-fA-F]+
SYMBOL = \<\<\=|\>\>\=|\{|\}|\.\.\.|\<\<|\>\>|\<|\>|\<\=|\>\=|\=\=|\!\=|\^|\||\&\&|\?|\:|\*\=|\/\=|\%\=|\+\=|\-\=|\&\=|\^\=|\|\=|\-\>|\+\+|\-\-|\;|\[|\]|\(|\)|\.|\&|\||\*|\+|\-|\~|\!|\/|\%|\=|\,|\#\#|\#
SIMPLE_ESCAPE = \\[abefnrtv'"\\?]
BLANK = [\s\n]

Rules. %"
{COMMENT} : skip_token.
"({OCTAL_ESCAPE}|{HEX_ESCAPE}|{SIMPLE_ESCAPE}|{S_CHAR})*"  : string_escape(TokenLine, tl(TokenChars)). 
'({OCTAL_ESCAPE}|{HEX_ESCAPE}|{SIMPLE_ESCAPE}|{C_CHAR})+'  : char_to_int(TokenLine, tl(TokenChars)).
{NUMBER}  : parse_num(TokenLine, TokenChars).
{KEYWORD} : {token, {list_to_atom(TokenChars), TokenLine}}.
{IDENT}   : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{SYMBOL}  : {token, {list_to_atom(TokenChars), TokenLine}}.
{BLANK}   : skip_token.

Erlang code.

-define(SIMPLE_ESCAPE(C), (C =:= $a) or (C =:= $b) or (C =:= $e) or (C =:= $f) or
                          (C =:= $n) or (C =:= $r) or (C =:= $t) or (C =:= $v) or
                          (C =:= $') or (C =:= $") or (C =:= $?) or (C =:= $\\)).
-define(HEX_DIGIT(C), (C >= $0) and ($9 >= C) or ((C >= $a) and ($f >= C)) or ((C >= $A) and ($F >= C))).
-define(OCT_DIGIT(C), (C >= $0) and ($7 >= C)).

char_to_int(Line, Chars) ->
  case resolve_escapes(Chars, char) of
    {error, Error} -> {error, Error};
    Esc -> {token, {char_l, Line, lists:foldr(fun (V,A) -> A * 256 + V end, 0, Esc)}}
  end.

string_escape(Line, Chars) ->
  case resolve_escapes(Chars, string) of
    {error, Error} -> {error, Error};
    Esc -> {token, {string_l, Line, Esc}}
  end.

% Simple escape
resolve_escapes([$\\,C | List], Type) when ?SIMPLE_ESCAPE(C) -> 
  case resolve_escapes(List, Type) of
    {error, Error} -> {error, Error};
    Esc -> [simple_escape(C) | Esc]
  end;
resolve_escapes([$\\,$x | List], Type) ->
  case lists:splitwith(fun (X) -> ?HEX_DIGIT(X) end, List) of
    {[], _} -> {error, "Invalid Hex Int"};
    {Hex, Rem} ->
      case resolve_escapes(Rem, Type) of
      {error, Error} -> {error, Error};
      Esc -> [list_to_integer(Hex, 16) | Esc]
    end
  end;
resolve_escapes([$\\,C | List], Type) when ?OCT_DIGIT(C) ->
  if ?OCT_DIGIT(hd(List)) ->
    if ?OCT_DIGIT(hd(tl(List))) ->
      case resolve_escapes(tl(tl(List)), Type) of
        {error, Error} -> {error, Error};
        Esc -> [((C-$0)*8+(hd(List)-$0))*8+(hd(tl(List))-$0) | Esc]
      end;
    true ->
      case resolve_escapes(tl(List), Type) of
        {error, Error} -> {error, Error};
        Esc -> [(C-$0)*8+(hd(List)-$0) | Esc]
      end
    end;
  true -> 
    case resolve_escapes(List, Type) of
      {error, Error} -> {error, Error};
      Esc -> [(C-$0) | Esc]
    end
  end;
resolve_escapes([$\\ | _List], _Type) -> {error, "Invalid Escape"};
resolve_escapes([$'], char) -> [];
resolve_escapes([$"], string) -> [];
resolve_escapes([C | List], Type) -> 
  case resolve_escapes(List, Type) of
    {error, Error} -> {error, Error};
    Esc -> [C | Esc]
  end.


simple_escape($a) -> $\x07;
simple_escape($b) -> $\x08;
simple_escape($e) -> $\x1b;
simple_escape($f) -> $\x0c;
simple_escape($n) -> $\x0a;
simple_escape($r) -> $\x0d;
simple_escape($t) -> $\x09;
simple_escape($v) -> $\x0b;
simple_escape($') -> $\x27;
simple_escape($") -> $\x22;
simple_escape($?) -> $\x3f;
simple_escape($\\)-> $\x5c.

parse_num(Line, Chars) -> 
  case number_lexer:string(Chars) of
    {ok, [{T, N, S}], _} -> {token, {T, Line, N, S}};
    {error, {_,_,M}, _} -> {error, M}
  end.

-undef(OCT_DIGIT).
-undef(HEX_DIGIT).
-undef(SIMPLE_ESCAPE).
