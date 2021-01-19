Nonterminals
  constant
  enum_l
  root.

Terminals 
  '[' ']' '(' ')' '.' '->' '++' '--' '&' '*' '+' '-' '~' '!'
  '/' '%' '<<' '>>' '<' '>' '<=' '>=' '==' '!=' '^' '|' '&&'
  '||' '?' ':' '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&='
  '^=' '|=' ',' '#' '##' '{' '}' ';' '...' sizeof auto break
  int struct double else long switch 'case' enum register do
  typedef char extern return union const float short default
  continue for signed while void goto volatile 'if' unsigned
  static string_l char_l float_l integer_l ident.


Rootsymbol root.

constant -> float_l   : '$1'.
constant -> integer_l : '$1'.
constant -> enum_l    : '$1'.
constant -> char_l    : assert_char('$1').



Erlang code.

assert_char({char_l, })
