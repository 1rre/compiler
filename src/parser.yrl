Nonterminals root type long_t ulong_t int_t uint_t char_t uchar_t schar_t
             float_t double_t l_double void_t typedef_t struct_t enum_t
             enum_l enum_c.


Terminals '[' ']' '(' ')' '.' '->' '++' '--' '&' '*' '+' '-' '~' '!'
          '/' '%' '<<' '>>' '<' '>' '<=' '>=' '==' '!=' '^' '|' '&&'
          '||' '?' ':' '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&='
          '^=' '|=' ',' '#' '##' '{' '}' ';' '...' sizeof auto break
          int struct double else long switch 'case' enum register do
          typedef char extern return union const float short default
          continue for signed while void goto volatile 'if' unsigned
          static string char_l number ident.

Rootsymbol root.

type -> uchar_t   : '$1'.
type -> schar_t   : '$1'.
type -> char_t    : '$1'.
type -> l_double  : '$1'.
type -> ulong_t   : '$1'.
type -> long_t    : '$1'.
type -> uint_t    : '$1'.
type -> int_t     : '$1'.
type -> float_t   : '$1'.
type -> double_t  : '$1'.
type -> void      : '$1'.
type -> typedef_t : '$1'.
type -> struct_t  : '$1'.
type -> enum_t    : '$1'.

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

typedef_t -> ident : {typedef_t, element(2, '$1')}.

enum_t -> enum ident '{' enum_l '}' : {enum_t, element(2, '$1'), element(3, '$2'), {enum_l, '$4'}}.
enum_t -> enum ident : {enum_t, element(2, '$1'), element(3, '$2')}.

enum_l -> enum_c enum_l : ['$1' | '$2'].
enum_l -> enum_c        : ['$1'].

enum_c -> ident : '$1'.
