Nonterminals
expression assignment_operator equality_operator relational_operator
shift_operator addition_operator multiplication_operator cast unary_operator
postfix_operator expression_list constant type_name postfix_list float_l
fractional exponent int_l
% enum_l
.
Terminals
auto double int struct break else long switch case enum register typedef
char extern return union const float short unsigned continue goto signed
void default sizeof volatile do if static while for
float_number raw_exponent raw_exponent_suffix
oct_number hex_number dec_number
oct_number_suffix hex_number_suffix dec_number_suffix oct_number_long dec_number_long
identifier char_l string_l
'{' '}' '...' ';' '[' ']' '(' ')' '.' '++' '--' '&' '|' '*' '+' '-' ','
'~' '!' '/' '%' '<<' '>>' '<' '>' '<=' '>=' '==' '!=' '^' '&&' '||' '?'
':' '=' '*=' '/=' '%=' '+=' '-=' '|=' '<<=' '>>=' '&=' '^=' '#' '##' '->'
.

Rootsymbol expression.

% This seemed to work at 850 (max), but I think it should be 0?
Nonassoc 850 '('.
Left  050 ','.
Left  100 assignment_operator.
Left  150 '?'.
Left  200 '||'.
Left  250 '&&'.
Left  300 '|'.
Left  350 '^'.
Left  400 '&'.
Left  450 equality_operator.
Left  500 relational_operator.
Left  550 shift_operator.
Left  600 addition_operator.
Left  650 multiplication_operator.
Right 700 cast.
Unary 750 unary_operator.
Unary 750 sizeof.
Left 800 postfix_operator.
Right 800 postfix_list.

expression -> expression ',' expression : {'$1', '$2'}.
expression -> expression assignment_operator expression : {'$1', '$2', '$3'}.
expression -> expression '?' expression ':' expression : {'$1', '$2', '$3', '$5'}.
expression -> expression '||' expression : {'$1', '$2', '$3'}.
expression -> expression '&&' expression : {'$1', '$2', '$3'}.
expression -> expression '|' expression : {'$1', '$2', '$3'}.
expression -> expression '^' expression : {'$1', '$2', '$3'}.
expression -> expression '&' expression : {'$1', '$2', '$3'}.
expression -> expression equality_operator expression : {'$1', '$2', '$3'}.
expression -> expression relational_operator expression : {'$1', '$2', '$3'}.
expression -> expression shift_operator expression : {'$1', '$2', '$3'}.
expression -> expression addition_operator expression : {'$1', '$2', '$3'}.
expression -> expression multiplication_operator expression : {'$1', '$2', '$3'}.
expression -> cast expression : {'$1', '$2'}.
expression -> unary_operator expression : {'$1', '$2'}.
expression -> expression postfix_operator : {'$1', '$2'}.
expression -> expression postfix_list : {'$1', '$2'}.
expression -> sizeof '(' type_name ')' : {'$1', '$3'}. % Is the 2nd part a cast?
expression -> identifier : '$1'.
expression -> constant : '$1'.
expression -> string_l : '$1'.
expression -> '(' expression ')' : '$2'.

postfix_list -> '[' expression ']' : {'$1', '$2', '$3'}.
postfix_list -> '(' expression_list ')' : {apply, '$2'}.

assignment_operator -> '='   : '$1'.
assignment_operator -> '*='  : '$1'.
assignment_operator -> '/='  : '$1'.
assignment_operator -> '%='  : '$1'.
assignment_operator -> '+='  : '$1'.
assignment_operator -> '-='  : '$1'.
assignment_operator -> '<<=' : '$1'.
assignment_operator -> '>>=' : '$1'.
assignment_operator -> '&='  : '$1'.
assignment_operator -> '^='  : '$1'.
assignment_operator -> '|='  : '$1'.

equality_operator -> '==' : '$1'.
equality_operator -> '!=' : '$1'.

relational_operator -> '>' : '$1'.
relational_operator -> '<' : '$1'.
relational_operator -> '>=' : '$1'.
relational_operator -> '<=' : '$1'.

shift_operator -> '>>' : '$1'.
shift_operator -> '<<' : '$1'.

addition_operator -> '+' : '$1'.
addition_operator -> '-' : '$1'.

multiplication_operator -> '*' : '$1'.
multiplication_operator -> '/' : '$1'.
multiplication_operator -> '%' : '$1'.

cast -> '(' type_name ')' : '$2'.

unary_operator -> sizeof : '$1'. % This is weird in the spec, maybe different precedence?
unary_operator -> '++' : '$1'.   % This is weird in the spec, maybe different precedence?
unary_operator -> '--' : '$1'.   % This is weird in the spec, maybe different precedence?
unary_operator -> '&' : '$1'.
unary_operator -> '*' : '$1'.
unary_operator -> '+' : '$1'.
unary_operator -> '-' : '$1'.
unary_operator -> '~' : '$1'.
unary_operator -> '!' : '$1'.

postfix_operator -> '++' : '$1'.
postfix_operator -> '--' : '$1'.
postfix_operator -> '.' identifier : {'$1', '$2'}.
postfix_operator -> '->' identifier : {'$1', '$2'}.

expression_list -> expression : ['$1'].
expression_list -> expression ',' expression_list : ['$1' | '$2'].

constant -> int_l : '$1'.
constant -> float_l : '$1'.
constant -> char_l : '$1'.
%constant -> enum_l : '$1'.

% We need to confirm that the numbers (especially decimal) are valid

int_l -> oct_number : '$1'.
int_l -> oct_number_long : '$1'.
int_l -> oct_number_suffix : '$1'.
int_l -> dec_number : '$1'.
int_l -> dec_number_long : '$1'.
int_l -> dec_number_suffix : '$1'.
int_l -> hex_number : '$1'.
int_l -> hex_number_suffix : '$1'.

% This causes reduce conflicts because an expression can be an identfier through constant or directly
% enum_l -> identifier : '$1'.

float_l -> exponent : '$1'.
float_l -> fractional : '$1'.
fractional -> dec_number '.' dec_number : {'$1', '$2', '$3'}.
fractional -> oct_number '.' dec_number : {'$1', '$2', '$3'}.
fractional -> dec_number '.' oct_number : {'$1', '$2', '$3'}.
fractional -> oct_number '.' oct_number : {'$1', '$2', '$3'}.

fractional -> dec_number '.' float_number : {'$1', '$2', '$3'}.
fractional -> oct_number '.' float_number : {'$1', '$2', '$3'}.

fractional -> dec_number '.' oct_number_long : {'$1', '$2', '$3'}.
fractional -> oct_number '.' oct_number_long : {'$1', '$2', '$3'}.

fractional -> dec_number '.' dec_number_long : {'$1', '$2', '$3'}.
fractional -> oct_number '.' dec_number_long : {'$1', '$2', '$3'}.

fractional -> dec_number '.' : {'$1', '$2'}.
fractional -> oct_number '.' : {'$1', '$2'}.

fractional -> dec_number '.' exponent : {'$1', '$2', '$3'}.
fractional -> oct_number '.' exponent : {'$1', '$2', '$3'}.

fractional -> '.' dec_number : {'$1', '$2'}.
fractional -> '.' oct_number : {'$1', '$2'}.

fractional -> '.' float_number : {'$1', '$2'}.
fractional -> '.' exponent : {'$1', '$2'}.

exponent -> raw_exponent_suffix : '$1'.
exponent -> raw_exponent '+' dec_number : {'$1', '$2'}.
exponent -> raw_exponent '+' oct_number : {'$1', '$2'}.
exponent -> raw_exponent '-' dec_number : {'$1', '$2'}.
exponent -> raw_exponent '-' oct_number : {'$1', '$2'}.
exponent -> raw_exponent '+' float_number : {'$1', '$2'}.
exponent -> raw_exponent '-' float_number : {'$1', '$2'}.
exponent -> raw_exponent '+' dec_number_long : {'$1', '$2'}.
exponent -> raw_exponent '-' oct_number_long : {'$1', '$2'}.
% For testing purposes only:

type_name -> int : '$1'.
