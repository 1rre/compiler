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
  operator
  postfix_operator
  unary_operator
  assignment_operator
  mult_operator
  add_operator
  shift_operator
  equality_operator
  relational_operator
  punctuator
  type_name
  constant
  enum_l
% Types (redo?)
  uchar_t
  schar_t
  char_t
  l_double
  ulong_t
  long_t
  uint_t
  int_t
  float_t
  double_t
.

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

primary_expr -> constant : '$1'.
primary_expr -> string_l : '$1'.
primary_expr -> '(' expression ')' : '$2'.

postfix_expr -> primary_expr : '$1'.
postfix_expr -> postfix_expr '[' expression ']' : {'$1', offset, '$3'}.
postfix_expr -> postfix_expr '(' ')' : '$1'.
postfix_expr -> postfix_expr '(' arg_expr_list ')' : {'$1', args, '$3'}.
postfix_expr -> postfix_expr '.' ident : {'$1', '.', '$3'}.
postfix_expr -> postfix_expr '->' ident : {'$1', '->', '$3'}.
postfix_expr -> postfix_expr postfix_operator : {'$1', '$2'}.

postfix_operator -> '++' : '++'.
postfix_operator -> '--' : '--'.

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
mult_expr -> mult_expr mult_operator cast_expr : {'$1', '$2', '$3'}.

mult_operator -> '*' : '*'.
mult_operator -> '/' : '/'.
mult_operator -> '%' : '%'.

add_expr -> mult_expr : '$1'.
add_expr -> add_expr add_operator mult_expr : {'$1', '$2', '$3'}.

add_operator -> '+' : '+'.
add_operator -> '-' : '-'.

shift_expr -> add_expr : '$1'.
shift_expr -> shift_expr shift_operator add_expr : {'$1', '$1', '$3'}.

shift_operator -> '>>' : '>>'.
shift_operator -> '<<' : '<<'.

relational_expr -> shift_expr : '$1'.
relational_expr -> relational_expr relational_operator shift_expr : {'$1', '$2', '$3'}.

relational_operator -> '<=' : '<='.
relational_operator -> '>=' : '<='.
relational_operator -> '<' : '<'.
relational_operator -> '>' : '>'.

equality_expr -> relational_expr : '$1'.
equality_expr -> equality_expr equality_operator relational_expr : {'$1', '$2', '$3'}.

equality_operator -> '==' : '=='.
equality_operator -> '!=' : '!='.

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

operator -> postfix_operator : '$1'      %% ++ --
operator -> unary_operator : '$1'.       %% & * + - ~ !
operator -> relational_operator : '$1'.  %% < > <= >=
operator -> equality_operator : '$1'.    %% == !=
operator -> mult_operator : '$1'.        %% * / %
operator -> add_operator : '$1'.         %% +'-
operator -> shift_operator : '$1'.       %% >> <<
operator -> assignment_operator : '$1'.  %% = *= /= %= += -= <<= >>= &= ^= |=
operator -> sizeof : sizeof.
operator -> '->' : '->'.
operator -> '&&' : '&&'.
operator -> '||' : '||'.
operator -> '##' : '##'.
operator -> '.' : '.'.
operator -> '&' : '&'.
operator -> '|' : '|'.
operator -> '^' : '^'.
operator -> '#' : '#'.
% The following only occur in pairs:
operator -> '[' : '['.
operator -> ']' : ']'.
operator -> '(' : '('.
operator -> ')' : ')'.
operator -> '?' : '?'.
operator -> ':' : ':'.

punctuator -> '[' : '['.
punctuator -> ']' : ']'.
punctuator -> '(' : '('.
punctuator -> ')' : ')'.
punctuator -> '{' : '{'.
punctuator -> '}' : '}'.
punctuator -> '*' : '*'.
punctuator -> ',' : ','.
punctuator -> ':' : ':'.
punctuator -> '=' : '='.
punctuator -> ';' : ';'.
punctuator -> '#' : '#'.
punctuator -> '...' : '...'.

type_name -> uchar_t   : '$1'.
type_name -> schar_t   : '$1'.
type_name -> char_t    : '$1'.
type_name -> l_double  : '$1'.
type_name -> ulong_t   : '$1'.
type_name -> long_t    : '$1'.
type_name -> uint_t    : '$1'.
type_name -> int_t     : '$1'.
type_name -> float_t   : '$1'.
type_name -> double_t  : '$1'.
type_name -> void      : '$1'.

ulong_t -> unsigned long int : {ulong, element(2, '$1')}.
ulong_t -> unsigned long     : {ulong, element(2, '$1')}.

long_t -> signed long int : {long, element(2, '$1')}.
long_t -> signed long     : {long, element(2, '$1')}.
long_t -> long int        : {long, element(2, '$1')}.
long_t -> long            : '$1'.

int_t -> signed int : {int, element(2, '$1')}.
int_t -> signed     : {int, element(2, '$1')}.
int_t -> int        : '$1'.

uint_t -> unsigned int : {uint, element(2, '$1')}.
uint_t -> unsigned     : {uint, element(2, '$1')}.

schar_t -> signed char : {schar, element(2, '$1')}.

uchar_t -> unsigned char : {uchar, element(2, '$1')}.

char_t -> char : '$1'.

l_double -> long double : {ldouble, element(2, '$1')}.

float_t -> float : '$1'.

double_t -> double : '$1'.


Erlang code.

