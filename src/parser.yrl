Nonterminals
  expression
  primary_expr
  postfix_expr
  assignment_expr
  unary_expr
  cast_expr
  mult_expr
  add_expr
  shift_expr
  relational_expr
  equality_expr
  band_expr
  xor_expr
  bor_expr
  and_expr
  or_expr
  ternary
  const_expr
  arg_expr_list
  unary_operator
  assignment_operator
  type_name
  constant
  enum_l.

Terminals 
  '[' ']' '(' ')' '.' '->' '++' '--' '&' '*' '+' '-' '~' '!'
  '/' '%' '<<' '>>' '<' '>' '<=' '>=' '==' '!=' '^' '|' '&&'
  '||' '?' ':' '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&='
  '^=' '|=' ',' '#' '##' '{' '}' ';' '...' sizeof auto break
  int struct double else long switch 'case' enum register do
  typedef char extern return union const float short default
  continue for signed while void goto volatile 'if' unsigned
  static string_l char_l float_l integer_l ident.


Rootsymbol expression.

primary_expr -> ident : '$1'.
primary_expr -> constant : '$1'.
primary_expr -> string_l : '$1'.
primary_expr -> '(' expression ')' : '$2'.

postfix_expr -> primary_expr : '$1'.
postfix_expr -> postfix_expr '[' expression ']' : {'$1', sqbr, '$3'}.
postfix_expr -> postfix_expr '(' ')' : '$1'.
postfix_expr -> postfix_expr '(' arg_expr_list ')' : {'$1', 'br', '$3'}.
postfix_expr -> postfix_expr '.' ident : {'$1', '.', '$3'}.
postfix_expr -> postfix_expr '->' ident : {'$1', '->', '$3'}.
postfix_expr -> postfix_expr '++' : {'$1', '$2'}.
postfix_expr -> postfix_expr '--' : {'$1', '$2'}.

arg_expr_list -> assignment_expr : ['$1'].
arg_expr_list -> assignment_expr arg_expr_list : ['$1' | '$2'].

% These can all probably be redone as precedence

unary_expr -> postfix_expr : '$1'.
unary_expr -> '++' unary_expr : {'$1', '$2'}.
unary_expr -> '--' unary_expr : {'$1', '$2'}.
unary_expr -> unary_operator cast_expr : {'$1', '$2'}.
unary_expr -> sizeof unary_expr : {'$1', '$2'}.
unary_expr -> sizeof '(' type_name ')' : {'$1', '$3'}.

unary_operator -> '&' : '&'.
unary_operator -> '*' : '*'.
unary_operator -> '+' : '+'.
unary_operator -> '-' : '-'.
unary_operator -> '~' : '~'.
unary_operator -> '!' : '!'.

cast_expr -> unary_expr : '$1'.
cast_expr -> '(' type_name ')' cast_expr : {'$2', '$4'}.

mult_expr -> cast_expr : '$1'.
mult_expr -> mult_expr '*' cast_expr : {'$1', '*', '$3'}.
mult_expr -> mult_expr '/' cast_expr : {'$1', '/', '$3'}.
mult_expr -> mult_expr '%' cast_expr : {'$1', '%', '$3'}.

add_expr -> mult_expr : '$1'.
add_expr -> add_expr '+' mult_expr : {'$1', '+', '$3'}.
add_expr -> add_expr '-' mult_expr : {'$1', '-', '$3'}.

shift_expr -> add_expr : '$1'.
shift_expr -> shift_expr '<<' add_expr : {'$1', '<<', '$3'}.
shift_expr -> shift_expr '>>' add_expr : {'$1', '>>', '$3'}.

relational_expr -> shift_expr : '$1'.
relational_expr -> relational_expr '<=' shift_expr : {'$1', '<=', '$3'}.
relational_expr -> relational_expr '>=' shift_expr : {'$1', '>=', '$3'}.
relational_expr -> relational_expr '<' shift_expr : {'$1', '<', '$3'}.
relational_expr -> relational_expr '>' shift_expr : {'$1', '>', '$3'}.

equality_expr -> relational_expr : '$1'.
equality_expr -> equality_expr '==' relational_expr : {'$1', '==', '$2'}.
equality_expr -> equality_expr '!=' relational_expr : {'$1', '!=', '$2'}.

band_expr -> equality_expr : '$1'.
band_expr -> band_expr '&' equality_expr : {'$1', '&', '$3'}.

xor_expr -> band_expr : '$1'.
xor_expr -> xor_expr '^' and_expr : {'$1', '^', '$3'}.

bor_expr -> xor_expr : '$1'.
bor_expr -> bor_expr '|' xor_expr : {'$1', '|', '$3'}.

and_expr -> bor_expr : '$1'.
and_expr -> and_expr '&&' bor_expr : {'$1', '&&', '$3'}.

or_expr -> and_expr : '$1'.
or_expr -> or_expr '||' and_expr : {'$1', '||', '$3'}.

ternary -> or_expr : '$1'.
ternary -> or_expr '?' expression ':' ternary : {'$1', '?', '$3', '$5'}.

assignment_expr -> ternary : '$1'.
assignment_expr -> unary_expr assignment_operator assignment_expr : {'$1', '$2', '$3'}.

assignment_operator -> '=' : '='.
assignment_operator -> '*=' : '*='.
assignment_operator -> '/=' : '/='.
assignment_operator -> '%=' : '%='.
assignment_operator -> '+=' : '+='.
assignment_operator -> '-=' : '-='.
assignment_operator -> '<<=' : '<<='.
assignment_operator -> '>>=' : '>>='.
assignment_operator -> '&=' : '&='.
assignment_operator -> '^=' : '^='.
assignment_operator -> '|=' : '|='.

const_expr -> ternary : '$1'.

expression -> assignment_expr : '$1'.
expression -> expression ',' assignment_expr.

constant -> float_l   : '$1'.
constant -> integer_l : '$1'.
constant -> ident     : '$1'.
constant -> char_l    : '$1'.




Erlang code.

