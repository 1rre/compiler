Nonterminals
expression assignment_operator equality_operator relational_operator
shift_operator addition_operator multiplication_operator cast unary_operator
postfix_operator expression_list constant type_name postfix_list float_l
fractional exponent int_l initialiser initialiser_list pointer struct_declarator
type_qualifier_list direct_declarator identifier_list declarator struct_union
direct_abstract_declarator abstract_declarator specifier_qualifier_list
enum_specifier parameter_type_list parameter_list type_qualifier type_specifier
enumerator_list declaration declaration_specifiers init_declarator_list
enumerator init_declarator storage_class_specifier struct_union_specifier
struct_declaration_list struct_declaration struct_declarator_list
parameter_declaration 
% typedef_name
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

Rootsymbol declaration.

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

%%%%%%%%%%%%%%%
% EXPRESSIONS %
%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%
% DECLARATIONS %
%%%%%%%%%%%%%%%%

declaration -> declaration_specifiers ';' : {'$1', '$2'}.
declaration -> declaration_specifiers init_declarator_list ';' : {'$1', '$2', '$3'}.

declaration_specifiers -> storage_class_specifier : ['$1'].
declaration_specifiers -> storage_class_specifier declaration_specifiers : ['$1' | '$2'].
declaration_specifiers -> type_specifier : ['$1'].
declaration_specifiers -> type_specifier declaration_specifiers : ['$1' | '$2'].
declaration_specifiers -> type_qualifier : ['$1'].
declaration_specifiers -> type_qualifier declaration_specifiers : ['$1' | '$2'].

init_declarator_list -> init_declarator : ['$1'].
init_declarator_list -> init_declarator ',' init_declarator_list : ['$1' | '$3'].

init_declarator -> declarator '=' initialiser : {'$1', '$2', '$3'}.

storage_class_specifier -> typedef : '$1'.
storage_class_specifier -> extern : '$1'.
storage_class_specifier -> static : '$1'.
storage_class_specifier -> auto : '$1'.
storage_class_specifier -> register : '$1'.

type_specifier -> void : '$1'.
type_specifier -> char : '$1'.
type_specifier -> short : '$1'.
type_specifier -> int : '$1'.
type_specifier -> long : '$1'.
type_specifier -> float : '$1'.
type_specifier -> double : '$1'.
type_specifier -> signed : '$1'.
type_specifier -> unsigned : '$1'.
type_specifier -> struct_union_specifier : '$1'.
type_specifier -> enum_specifier : '$1'.
%type_specifier -> typedef_name : '$1'.

struct_union_specifier -> struct_union identifier '{' struct_declaration_list '}' : {'$1', '$2', '$3', '$4', '$5'}.
struct_union_specifier -> struct_union '{' struct_declaration_list '}' : {'$1', '$2', '$3', '$4'}.
struct_union_specifier -> struct_union identifier : {'$1', '$2'}.

struct_union -> struct : '$1'.
struct_union -> union : '$1'.

struct_declaration_list -> struct_declaration : ['$1'].
struct_declaration_list -> struct_declaration struct_declaration_list : ['$1' | '$2'].

struct_declaration -> specifier_qualifier_list struct_declarator_list ';' : {'$1', '$2', '$3'}.

specifier_qualifier_list -> type_specifier : ['$1'].
specifier_qualifier_list -> type_specifier specifier_qualifier_list : ['$1' | '$2'].
specifier_qualifier_list -> type_qualifier : ['$1'].
specifier_qualifier_list -> type_qualifier specifier_qualifier_list : ['$1' | '$2'].

struct_declarator_list -> struct_declarator : ['$1'].
struct_declarator_list -> struct_declarator ',' struct_declarator_list : ['$1' | '$3'].

struct_declarator -> declarator : '$1'.
struct_declarator -> declarator ':' expression : {'$1', '$2', '$3'}.
struct_declarator -> ':' expression : {'$1', '$2'}.

enum_specifier -> enum identifier '{' enumerator_list '}' : {'$1', '$2', '$3', '$4', '$5'}.
enum_specifier -> enum '{' enumerator_list '}' : {'$1', '$2', '$3', '$4'}.

enumerator_list -> enumerator : ['$1'].
enumerator_list -> enumerator ',' enumerator_list : ['$1' | '$3'].

enumerator -> identifier : '$1'.
enumerator -> identifier '=' expression : {'$1', '$2', '$3'}.

type_qualifier -> const : '$1'.
type_qualifier -> volatile : '$1'.

declarator -> pointer direct_declarator : {'$1', '$2'}.
declarator -> direct_declarator : '$1'.

direct_declarator -> identifier : '$1'.
direct_declarator -> '(' declarator ')' : '$2'.
direct_declarator -> direct_declarator '[' expression ']' : {'$1', '$2', '$3', '$4'}.
direct_declarator -> direct_declarator '[' ']' : {'$1', '$2', '$3'}.
direct_declarator -> direct_declarator '(' parameter_type_list ')' : {'$1', '$2', '$3', '$4'}.
direct_declarator -> direct_declarator '(' identifier_list ')' : {'$1', '$2', '$3', '$4'}.
direct_declarator -> direct_declarator '(' ')' : {'$1', '$2', '$3'}.

pointer -> '*' type_qualifier_list : {'$1', '$2'}.
pointer -> '*' : '$1'.
pointer -> '*' type_qualifier_list pointer : {'$1', '$2', '$3'}.
pointer -> '*' pointer : {'$1', '$2'}.

type_qualifier_list -> type_qualifier : ['$1'].
type_qualifier_list -> type_qualifier type_qualifier_list : ['$1' | '$2'].

parameter_type_list -> parameter_list : '$1'.
parameter_type_list -> parameter_list ',' '...' : {'$1', '$2'}.

parameter_list -> parameter_declaration : ['$1'].
parameter_list -> parameter_declaration ',' parameter_list : ['$1' | '$3'].

parameter_declaration -> declaration_specifiers declarator : {'$1', '$2'}.
parameter_declaration -> declaration_specifiers abstract_declarator : {'$1', '$2'}.
parameter_declaration -> declaration_specifiers : '$1'.

identifier_list -> identifier : ['$1'].
identifier_list -> identifier ',' identifier_list : ['$1' | '$3'].

type_name -> specifier_qualifier_list abstract_declarator : {'$1', '$2'}.
type_name -> specifier_qualifier_list : '$1'.

abstract_declarator -> pointer : '$1'.
abstract_declarator -> pointer direct_abstract_declarator : {'$1', '$2'}.
abstract_declarator -> direct_abstract_declarator : '$1'.

direct_abstract_declarator -> '(' abstract_declarator ')' : {'$2'}.
direct_abstract_declarator -> direct_abstract_declarator '[' expression ']' : {'$1', '$2', '$3', '$4'}.
direct_abstract_declarator -> direct_abstract_declarator '[' ']' : {'$1', '$2', '$3'}.
direct_abstract_declarator ->  '[' expression ']' : {'$1', '$2', '$3'}.
direct_abstract_declarator ->  '[' ']' : {'$1', '$2'}.
direct_abstract_declarator -> direct_abstract_declarator '(' parameter_type_list ')' : {'$1', '$2', '$3', '$4'}.
direct_abstract_declarator -> direct_abstract_declarator '(' ')' : {'$1', '$2', '$3'}.
direct_abstract_declarator ->  '(' parameter_type_list ')' : {'$1', '$2', '$3'}.
direct_abstract_declarator ->  '(' ')' : {'$1', '$2'}.


% causes r/r conflict
% typedef_name -> identifier : '$1'.

initialiser -> expression : '$1'.
initialiser -> '{' initialiser_list '}' : {'$1', '$2', '$3'}.
initialiser -> '{' initialiser_list ',' '}' : {'$1', '$2', '$3', '$4'}.

initialiser_list -> initialiser : ['$1'].
initialiser_list -> initialiser ',' initialiser_list : ['$1' | '$3'].

