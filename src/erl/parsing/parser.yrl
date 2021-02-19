Nonterminals
expression assignment_operator equality_operator relational_operator
shift_operator addition_operator multiplication_operator cast unary_operator
postfix_operator expression_list constant type_name postfix_list initialiser
initialiser_list pointer struct_declarator
type_qualifier_list direct_declarator identifier_list declarator struct_union
direct_abstract_declarator abstract_declarator specifier_qualifier_list
enum_specifier parameter_type_list parameter_list type_qualifier type_specifier
enumerator_list declaration declaration_specifiers init_declarator_list
enumerator init_declarator storage_class_specifier struct_union_specifier
struct_declaration_list struct_declaration struct_declarator_list
parameter_declaration statement labeled_statement compound_statement
iteration_statement statement_list selection_statement jump_statement
expression_statement declaration_list translation_unit external_declaration
function_definition translation_units

% We need advanced fixes for these 2: https://stackoverflow.com/questions/17202409/how-typedef-name-identifier-issue-is-resolved-in-c
.
Terminals
auto double int struct break else long switch case enum register typedef
char extern return union const float short unsigned continue goto signed
void default sizeof volatile do if static while for 
int_l float_l identifier char_l string_l
'{' '}' '...' ';' '[' ']' '(' ')' '.' '++' '--' '&' '|' '*' '+' '-' ','
'~' '!' '/' '%' '<<' '>>' '<' '>' '<=' '>=' '==' '!=' '^' '&&' '||' '?'
':' '=' '*=' '/=' '%=' '+=' '-=' '|=' '<<=' '&=' '^=' '#' '##' '>>=' '->'
typedef_name
enum_l
.

Rootsymbol translation_unit.

% This seemed to work at 850, but I think it should be 0?
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
Left  800 postfix_operator.
Right 800 postfix_list.
Right 900 declaration_list.
Unary 950 external_declaration.
Unary 950 else.


%%%%%%%%%%%%%%%
% EXPRESSIONS %
%%%%%%%%%%%%%%%

expression -> expression ',' expression : {'$1','$2'}. % I don't think this is tested
expression -> expression assignment_operator expression : {move,'$2',['$1','$3']}.
expression -> expression '?' expression ':' expression : {'?','$1', ['$3','$5']}.
expression -> expression '||' expression : {bif,'||',['$1','$3']}.
expression -> expression '&&' expression : {bif,'&&',['$1','$3']}.
expression -> expression '|' expression : {bif,'|',['$1','$3']}.
expression -> expression '^' expression : {bif,'^',['$1','$3']}.
expression -> expression '&' expression : {bif,'&',['$1','$3']}.
expression -> expression equality_operator expression : {bif,element(1,'$2'),['$1','$3']}.
expression -> expression relational_operator expression : {bif,element(1,'$2'),['$1','$3']}.
expression -> expression shift_operator expression : {bif,element(1,'$2'),['$1','$3']}.
expression -> expression addition_operator expression : {bif,element(1,'$2'),['$1','$3']}.
expression -> expression multiplication_operator expression : {bif, element(1,'$2'),['$1','$3']}.
expression -> cast expression : {cast,'$1','$2'}.
expression -> unary_operator expression : {element(1,'$1'),'$2'}.
expression -> expression postfix_operator : {'$1',element(1,'$2')}.
expression -> expression postfix_list : {'$1','$2'}.
expression -> sizeof '(' type_name ')' : {sizeof,'$3'}. % Is the 2nd part a cast?
expression -> identifier : '$1'.
expression -> constant : '$1'.
expression -> string_l : '$1'.
expression -> '(' expression ')' : '$2'.

postfix_list -> '[' expression ']' : {offset, '$2'}.
postfix_list -> '(' expression_list ')' : {apply,'$2'}.

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
postfix_operator -> '.' identifier : {'$1','$2'}.
postfix_operator -> '->' identifier : {'$1','$2'}.

expression_list -> expression : ['$1'].
expression_list -> expression ',' expression_list : ['$1' | '$2'].

constant -> int_l : '$1'.
constant -> float_l : '$1'.
constant -> char_l : '$1'.
constant -> enum_l : '$1'.


%enum_l -> identifier : '$1'.


%%%%%%%%%%%%%%%%
% DECLARATIONS %
%%%%%%%%%%%%%%%%

declaration -> declaration_specifiers ';' : {'$1'}.
declaration -> declaration_specifiers init_declarator_list ';' : {'$1','$2'}.

declaration_specifiers -> storage_class_specifier : ['$1'].
declaration_specifiers -> storage_class_specifier declaration_specifiers : ['$1' | '$2'].
declaration_specifiers -> type_specifier : ['$1'].
declaration_specifiers -> type_specifier declaration_specifiers : ['$1' | '$2'].
declaration_specifiers -> type_qualifier : ['$1'].
declaration_specifiers -> type_qualifier declaration_specifiers : ['$1' | '$2'].

init_declarator_list -> init_declarator : ['$1'].
init_declarator_list -> init_declarator ',' init_declarator_list : ['$1' | '$3'].

init_declarator -> declarator : '$1'.
init_declarator -> declarator '=' initialiser : {'$1','$2','$3'}.

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
type_specifier -> typedef_name : '$1'.

struct_union_specifier -> struct_union identifier '{' struct_declaration_list '}' : {'$1','$2','$4'}.
struct_union_specifier -> struct_union '{' struct_declaration_list '}' : {'$1','$3'}.
struct_union_specifier -> struct_union identifier : {'$1','$2'}.

struct_union -> struct : '$1'.
struct_union -> union : '$1'.

struct_declaration_list -> struct_declaration : ['$1'].
struct_declaration_list -> struct_declaration struct_declaration_list : ['$1' | '$2'].

struct_declaration -> specifier_qualifier_list struct_declarator_list ';' : {'$1','$2'}.

specifier_qualifier_list -> type_specifier : ['$1'].
specifier_qualifier_list -> type_specifier specifier_qualifier_list : ['$1' | '$2'].
specifier_qualifier_list -> type_qualifier : ['$1'].
specifier_qualifier_list -> type_qualifier specifier_qualifier_list : ['$1' | '$2'].

struct_declarator_list -> struct_declarator : ['$1'].
struct_declarator_list -> struct_declarator ',' struct_declarator_list : ['$1' | '$3'].

struct_declarator -> declarator : '$1'.
struct_declarator -> declarator ':' expression : {'$1','$2','$3'}.
struct_declarator -> ':' expression : {'$1','$2'}.

enum_specifier -> enum identifier '{' enumerator_list '}' : {'$1','$2','$4'}.
enum_specifier -> enum '{' enumerator_list '}' : {'$1','$3'}.
enum_specifier -> enum identifier : {'$1','$2'}.

enumerator_list -> enumerator : ['$1'].
enumerator_list -> enumerator ',' enumerator_list : ['$1' | '$3'].

enumerator -> identifier : '$1'.
enumerator -> identifier '=' expression : {'$1','$2','$3'}.

type_qualifier -> const : '$1'.
type_qualifier -> volatile : '$1'.

declarator -> pointer direct_declarator : {'$1','$2'}.
declarator -> direct_declarator : '$1'.

direct_declarator -> identifier : '$1'.
direct_declarator -> '(' declarator ')' : '$2'.
direct_declarator -> direct_declarator '[' expression ']' : {'$1','$2','$3','$4'}.
direct_declarator -> direct_declarator '[' ']' : {'$1','$2','$3'}.
direct_declarator -> direct_declarator '(' parameter_type_list ')' : {'$1','$3'}.
direct_declarator -> direct_declarator '(' identifier_list ')' : {'$1','$3'}.
direct_declarator -> direct_declarator '(' ')' : {'$1'}.

pointer -> '*' type_qualifier_list : {'$1','$2'}.
pointer -> '*' : '$1'.
pointer -> '*' type_qualifier_list pointer : {'$1','$2','$3'}.
pointer -> '*' pointer : {'$1','$2'}.

type_qualifier_list -> type_qualifier : ['$1'].
type_qualifier_list -> type_qualifier type_qualifier_list : ['$1' | '$2'].

parameter_type_list -> parameter_list : '$1'.
parameter_type_list -> parameter_list ',' '...' : {'$1','$2'}.

parameter_list -> parameter_declaration : ['$1'].
parameter_list -> parameter_declaration ',' parameter_list : ['$1' | '$3'].

parameter_declaration -> declaration_specifiers declarator : {'$1','$2'}.
parameter_declaration -> declaration_specifiers abstract_declarator : {'$1','$2'}.
parameter_declaration -> declaration_specifiers : '$1'.

identifier_list -> identifier : ['$1'].
identifier_list -> identifier ',' identifier_list : ['$1' | '$3'].

type_name -> specifier_qualifier_list abstract_declarator : {'$1','$2'}.
type_name -> specifier_qualifier_list : '$1'.

abstract_declarator -> pointer : '$1'.
abstract_declarator -> pointer direct_abstract_declarator : {'$1','$2'}.
abstract_declarator -> direct_abstract_declarator : '$1'.

direct_abstract_declarator -> '(' abstract_declarator ')' : {'$2'}.
direct_abstract_declarator -> direct_abstract_declarator '[' expression ']' : {'$1','$2','$3','$4'}.
direct_abstract_declarator -> direct_abstract_declarator '[' ']' : {'$1','$2','$3'}.
direct_abstract_declarator ->  '[' expression ']' : {'$1','$2','$3'}.
direct_abstract_declarator ->  '[' ']' : {'$1','$2'}.
direct_abstract_declarator -> direct_abstract_declarator '(' parameter_type_list ')' : {'$1','$2','$3','$4'}.
direct_abstract_declarator -> direct_abstract_declarator '(' ')' : {'$1','$2','$3'}.
direct_abstract_declarator ->  '(' parameter_type_list ')' : {'$1','$2','$3'}.
direct_abstract_declarator ->  '(' ')' : {'$1','$2'}.

% typedef_name -> identifier : '$1'.

initialiser -> expression : '$1'.
initialiser -> '{' initialiser_list '}' : {'$2'}.
initialiser -> '{' initialiser_list ',' '}' : {'$2'}.

initialiser_list -> initialiser : ['$1'].
initialiser_list -> initialiser ',' initialiser_list : ['$1' | '$3'].

%%%%%%%%%%%%%%
% STATEMENTS %
%%%%%%%%%%%%%%

statement -> labeled_statement : '$1'.
statement -> compound_statement : '$1'.
statement -> expression_statement : '$1'.
statement -> selection_statement : '$1'.
statement -> iteration_statement : '$1'.
statement -> jump_statement : '$1'.

labeled_statement -> identifier ':' statement : {'$1','$2','$3'}.
labeled_statement -> case expression ':' statement : {'$1','$2','$3','$4'}.
labeled_statement -> default ':' statement : {'$1','$2','$3'}.

compound_statement -> '{' declaration_list statement_list '}' : {'$2','$3'}.
compound_statement -> '{' declaration_list '}' : {'$2'}.
compound_statement -> '{' statement_list '}' : {'$2'}.

declaration_list -> declaration : ['$1'].
declaration_list -> declaration declaration_list : ['$1' | '$2'].

statement_list -> statement : ['$1'].
statement_list -> statement statement_list : ['$1' | '$2'].

expression_statement -> expression ';' : {'$1'}.
expression_statement -> ';' : {nil}.

selection_statement -> if '(' expression ')' statement else statement : {'$1','$3','$5','$7'}.
selection_statement -> if '(' expression ')' statement : {'$1','$3','$5'}.
selection_statement -> switch '(' expression ')' statement : {'$1','$3','$5'}.

iteration_statement -> while '(' expression ')' statement : {'$1','$3','$5'}.
iteration_statement -> do statement while '(' expression ')' ';' : {'$1','$2','$5'}.
iteration_statement -> for '(' expression ';' expression ';' expression ')' statement : {'$1',{'$3','$5','$7'},'$9'}.
iteration_statement -> for '(' expression ';' expression ';' ')' statement : {'$1',{'$3','$5',nil},'$8'}.
iteration_statement -> for '(' expression ';' ';' expression ')' statement : {'$1',{'$3',nil,'$6'},'$8'}.
iteration_statement -> for '(' ';' expression ';' expression ')' statement : {'$1',{nil,'$4','$6'},'$8'}.
iteration_statement -> for '(' expression ';' ';' ')' statement : {'$1',{'$3',nil,nil},'$7'}.
iteration_statement -> for '(' ';' expression ';' ')' statement : {'$1',{nil,'$4',nil},'$7'}.
iteration_statement -> for '(' ';' ';' expression ')' statement : {'$1',{nil,nil,'$5'},'$7'}.
iteration_statement -> for '(' ';' ';' ')' statement : {'$1',{nil,nil,nil},'$6'}.

jump_statement -> goto identifier ';' : {'$1', '$2'}.
jump_statement -> continue ';' : {'$1'}.
jump_statement -> break ';' : {'$1'}.
jump_statement -> return expression ';' : {'$1', '$2'}.
jump_statement -> return ';' : {'$1', nil}.

translation_unit -> external_declaration : ['$1'].
translation_unit -> external_declaration translation_unit : ['$1' | '$2'].

external_declaration -> function_definition : '$1'.
external_declaration -> declaration : '$1'.

function_definition -> declaration_specifiers declarator : {'$1', '$2'}.
function_definition -> declarator : '$1'.
function_definition -> declaration_list compound_statement : {'$1', '$2'}.
function_definition -> compound_statement : '$1'.

% Out of spec but probs useful:

translation_units -> translation_unit : ['$1'].
translation_units -> translation_unit translation_units : ['$1' | '$2'].





