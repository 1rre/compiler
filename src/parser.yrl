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
  constant
  declaration
  declaration_spec
  declarator_list
  init_declarator
  declarator
  initialiser
  storage_spec
  enum_spec
  typedef_name
  struct_spec
  struct_decn_list
  struct_decn
  spec_qual_list
  type_spec
  type_qual
  struct_decr_list
  struct_decr
  type_name
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

declaration -> declaration_spec ';' : '$1'.
declaration -> declaration_spec declarator_list ';' : '$1'.

declaration_spec -> storage_spec : ['$1'].
declaration_spec -> storage_spec declaration_spec : ['$1' | '$2'].
declaration_spec -> type_spec : ['$1'].
declaration_spec -> type_spec declaration_spec : ['$1' | '$2'].
declaration_spec -> type_qual : ['$1'].
declaration_spec -> type_qual declaration_spec : ['$1' | '$2'].

declarator_list -> init_declarator : ['$1'].
declarator_list -> init_declarator declarator_list : ['$1' | '$2'].

init_declarator -> declarator : '$1'.
init_declarator -> declarator '=' initialiser : {'$1', '=', '$3'}.

storage_spec -> typedef : '$1'.
storage_spec -> extern : '$1'.
storage_spec -> static : '$1'.
storage_spec -> auto : '$1'.
storage_spec -> register : '$1'.

type_spec -> void : '$1'.
type_spec -> char : '$1'.
type_spec -> short : '$1'.
type_spec -> int : '$1'.
type_spec -> long : '$1'.
type_spec -> float : '$1'.
type_spec -> double : '$1'.
type_spec -> signed : '$1'.
type_spec -> unsigned : '$1'.
type_spec -> struct_spec : '$1'.
type_spec -> enum_spec : '$1'.
type_spec -> typedef_name : '$1'.

struct_spec -> struct '{' struct_decn_list '}' : {'$1', '$3'}.
struct_spec -> union '{' struct_decn_list '}' : {'$1', '$3'}.
struct_spec -> struct ident '{' struct_decn_list '}' : {'$1', '$2', '$4'}.
struct_spec -> union ident '{' struct_decn_list '}' : {'$1', '$2', '$4'}.
struct_spec -> struct ident : {'$1', '$2'}.
struct_spec -> union ident : {'$1', '$2'}.

struct_decn_list -> struct_decn : ['$1'].
struct_decn_list -> struct_decn struct_decn_list : ['$1' | '$2'].

struct_decn -> spec_qual_list struct_decr_list ';' : {'$1', '$2'}.

spec_qual_list -> type_spec : ['$1'].
spec_qual_list -> type_spec spec_qual_list : ['$1' | '$2'].
spec_qual_list -> type_qual : ['$1'].
spec_qual_list -> type_qual spec_qual_list : ['$1' | '$2'].

struct_decr_list -> struct_decr : ['$1'].
struct_decr_list -> struct_decr ',' struct_decr_list : ['$1' | '$2'].

struct_decr -> declarator : '$1'.
struct_decr -> ':' const_expr : '$2'.
struct_decr -> declarator ':' const_expr : {'$1', '$2'}.


% so that it will compile:
type_name -> int : '$1'.


Erlang code.

