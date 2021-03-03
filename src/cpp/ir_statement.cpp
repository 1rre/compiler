
#include "ir_statement.hpp"
#include "ir_arg.hpp"
#include <erl_nif.h>
#include <stdio.h>

namespace ir::statement {

char get_atom_char(ErlNifEnv* Env,const ERL_NIF_TERM Term) {
  char* Buf = (char*)malloc(2);
  if (!enif_get_atom(Env,Term,Buf,2,ERL_NIF_LATIN1)) {
    fprintf(stderr,"Couldn't get atom char\n\r");
    exit(1);
  }
  char Rtn = *Buf;
  free(Buf);
  return Rtn;
}

void data_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::data** Data) {
  const ERL_NIF_TERM* R_Tuple;
  int R_Num;
  double R_Dbl;
  int Arity;
  if (!enif_get_tuple(Env,Elem,&Arity,&R_Tuple)) exit(1);
  char R_Name = get_atom_char(Env,R_Tuple[0]);
  switch (R_Name) {
    case 'x':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = new arg::reg(R_Num);
    break;
    case 'y':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = new arg::stack(R_Num);
    break;
    case 'f':
      if (!enif_get_double(Env,R_Tuple[1],&R_Dbl)) exit(1);
      *Data = new arg::floating(R_Dbl);
    break;
    case 'i':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = new arg::integer(R_Num);
    break;
    default:
      exit(1);
    }
}

void mem_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::memory** Mem) {
  const ERL_NIF_TERM* R_Tuple;
  int R_Num;
  int Arity;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,&Arity,&R_Tuple) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = get_atom_char(Env,R_Tuple[0]);
  switch (R_Name) {
    case 'x':
      *Mem = new arg::reg(R_Num);
    break;
    case 'y':
      *Mem = new arg::stack(R_Num);
    break;
    default:
      exit(1);
  }
}
void reg_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::reg** Reg) {
  const ERL_NIF_TERM* R_Tuple;
  int R_Num;
  int Arity;
  if (!enif_get_tuple(Env,Elem,&Arity,&R_Tuple) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = get_atom_char(Env,R_Tuple[0]);
  if (R_Name != 'x') exit(1);
  *Reg = new arg::reg(R_Num);
}

void lbl_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::label** Lbl) {
  const ERL_NIF_TERM* R_Tuple;
  int R_Num;
  int Arity;
  if (!enif_get_tuple(Env,Elem,&Arity,&R_Tuple) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = get_atom_char(Env,R_Tuple[0]);
  if (R_Name != 'l') {
    fprintf(stderr,"Syntax error, expected label\n\r");
    exit(1);
  }
  *Lbl = new arg::label(R_Num);
}

void type_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::type** Type) {
  const ERL_NIF_TERM* T_Tuple;
  int P,S;
  char T;
  int Arity;
  if (!enif_get_tuple(Env,Elem,&Arity,&T_Tuple) ||
      !enif_get_int(Env,T_Tuple[0],&P) ||
      !enif_get_int(Env,T_Tuple[2],&S)) {
    fprintf(stderr,"Error getting type tuple\n\r");
    exit(1);
  }
  T = get_atom_char(Env,T_Tuple[1]);
  *Type = new arg::type(P,T,S);
}

address::address(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  mem_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);

  fprintf(stderr,"Address: {<unknown>,%d} -> {y,%d}\n\r",src->number,dest->number);
}
allocate::allocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&bits)) {
    fprintf(stderr,"Error getting bits to allocate\n\r");
    exit(1);
  }
  fprintf(stderr,"Allocate: %d\n\r",bits);
}
call::call(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  fprintf(stderr,"Call: <unknown>\n\r");
  // TODO: Get function call details
}
cast::cast(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&reg);
  const ERL_NIF_TERM* Type;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elems[1],NULL,&Type)) exit(1);
  // TODO: Decode & type
  fprintf(stderr,"Cast: {x,%d} -> <unknown>\n\r", reg->number);
}
deallocate::deallocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&bits)) exit(1);
  fprintf(stderr,"Allocate: %d\n\r",bits);
}
function::function(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  type_factory(Env,Elems[0],&type);
  unsigned Length;
  if (!enif_get_atom_length(Env,Elems[1],&Length,ERL_NIF_LATIN1)) {
    fprintf(stderr,"Error getting function atom length\n\r");
    exit(1);
  }
  char* Buf = (char*)malloc(Length+1);
  if (!enif_get_atom(Env,Elems[1],Buf,Length+1,ERL_NIF_LATIN1)) {
    fprintf(stderr,"Error getting function name\n\r");
    exit(1);
  }
  name = std::string(Buf);
  if (!enif_get_int(Env,Elems[2],&arity)) {
    fprintf(stderr,"Error getting function arity\n\r");
    exit(1);
  }
  std::vector<statement*> Terms;
  if (!enif_get_list_length(Env,Elems[3],&Length)) exit(1);
  ERL_NIF_TERM Head;
  ERL_NIF_TERM Tail = Elems[3];
  for (unsigned I = 0; I < Length; I++) {
    if (!enif_get_list_cell(Env,Tail,&Head,&Tail)) exit(1);
    else Terms.push_back(factory(Env,Head));
  }
  fprintf(stderr,"Function: %s/%d -> {%d,%c,%d}\n\r",Buf,arity,
                                                     type->ref_level,
                                                     type->data_type,
                                                     type->width);
}
jump::jump(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  lbl_factory(Env,Elems[0],&lbl);
  fprintf(stderr,"Jump: {l,%d}\n\r",lbl->number);
}
label::label(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&number)) exit(1);
  fprintf(stderr,"Label: %d\n\r",number);
}
load::load(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  mem_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);

  if (src->code == arg::REG)
    fprintf(stderr,"Load: {x,%d} -> {y,%d}\n\r",src->number,dest->number);
  else if (src->code == arg::STACK)
    fprintf(stderr,"Load: {y,%d} -> {y,%d}\n\r",src->number,dest->number);
}
move::move(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  data_factory(Env,Elems[0],&src);
  mem_factory(Env,Elems[1],&dest);
  if (src->code == arg::REG)
    fprintf(stderr,"Move: {x,%d}",dynamic_cast<ir::arg::reg*>(src)->number);
  else if (src->code == arg::STACK)
    fprintf(stderr,"Move: {y,%d}",dynamic_cast<ir::arg::stack*>(src)->number);
  else if (src->code == arg::FLOAT)
    fprintf(stderr,"Move: {float,%f}",dynamic_cast<ir::arg::floating*>(src)->value);
  else if (src->code == arg::INT)
    fprintf(stderr,"Move: {integer,%d}",dynamic_cast<ir::arg::integer*>(src)->value);

  if (dest->code == arg::REG)
    fprintf(stderr," -> {x,%d}\n\r",dest->number);
  else
    fprintf(stderr," -> {y,%d}\n\r",dest->number);
}
// Empty constructor as return acts as end of statement only.
rtn::rtn() {}
store::store(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);
  fprintf(stderr,"Store: {x,%d} -> {x,%d}\n\r",src->number,dest->number);
}
test::test(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&reg);
  lbl_factory(Env,Elems[1],&lbl);
  fprintf(stderr,"Test: {x,%d}? {l,%d}\n\r",reg->number,lbl->number);
}
bif::bif(ErlNifEnv* Env,const ERL_NIF_TERM* Elems,char* Operator) {
  fprintf(stderr,"Bif...\n\r");
  // This is nasty but what can you do :)
  switch (Operator[0]) {
    case '|': switch (Operator[1]) {
      case '\0':
        code=BOR;
      break;
      case '|':
        code=LOR;
      break;
      default:
        code=ERR;
    } break;
    case '&': switch (Operator[1]) {
      case '\0':
        code=BND;
        break;
      case '&':
        code=LND;
        break;
      default:
        code=ERR;
    } break;
    case '^':
      code=BXR;
      break;
    case '=':
      code=EEQ;
      break;
    case '!':
      code=IEQ;
      break;
    case '<': switch (Operator[1]) {
      case '\0':
        code=LES;
        break;
      case '<':
        code=SHL;
        break;
      case '=':
        code=LEQ;
        break;
      default:
        code=ERR;
    } break;
    case '>': switch (Operator[1]) {
      case '\0':
        code=GRT;
        break;
      case '>':
        code=SHR;
        break;
      case '=':
        code=GEQ;
        break;
      default:
        code=ERR;
    } break;
    case '+':
      code=ADD;
      break;
    case '-':
      code=SUB;
      break;
    case '*':
      code=MUL;
      break;
    case '/':
      code=DIV;
      break;
    case '%':
      code=REM;
      break;
    default:
      code=ERR;
      break;
  }

  reg_factory(Env,Elems[0],&dest);
  ERL_NIF_TERM Head;
  ERL_NIF_TERM* Tail;
  enif_get_list_cell(Env, Elems[1], &Head, Tail);
  reg_factory(Env,Head,&a);
  enif_get_list_cell(Env, *Tail, &Head, Tail);
  reg_factory(Env,Head,&b);
}


statement* factory(ErlNifEnv* Env, ERL_NIF_TERM St) {
  if (enif_is_atom(Env,St)) return new rtn();
  int Arity;
  const ERL_NIF_TERM* Elems;
  if (!enif_get_tuple(Env,St,&Arity,&Elems)) return nullptr;
  unsigned Length;
  if (!enif_get_atom_length(Env,Elems[0],&Length,ERL_NIF_LATIN1)) return nullptr;
  char* Atom = (char*)malloc(Length+1);
  if (!enif_get_atom(Env,Elems[0],Atom,Length+1,ERL_NIF_LATIN1)) return nullptr;
  char Hd = Atom[0];
  char Third = Atom[2];
  free(Atom);
  // Ignore this I know it's nasty sorry
  switch (Hd) {
    case 'a': switch (Third) {
      case 'd': return new address(Env,Elems+1);
      case 'l': return new allocate(Env,Elems+1);
      default: return nullptr;
    }
    case 'c': switch (Third) {
      case 'l': return new call(Env,Elems+1);
      case 's': return new cast(Env,Elems+1);
      default: return nullptr;
    }
    case 'd': return new deallocate(Env,Elems+1);
    case 'f': return new function(Env,Elems+1);
    case 'j': return new jump(Env,Elems+1);
    case 'l': switch (Third) {
      case 'b': return new label(Env,Elems+1);
      case 'a': return new load(Env,Elems+1);
      default: return nullptr;
    }
    case 'm': return new move(Env,Elems+1);
    case 's': return new store(Env,Elems+1);
    case 't': return new test(Env,Elems+1);
    default: return new bif(Env,Elems+1,Atom);
  }
}
}
