
#include "ir_statement.hpp"
#include "ir_arg.hpp"
#include <erl_nif.h>
#include <stdio.h>

namespace ir::statement {

void data_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::data** Data) {
  const ERL_NIF_TERM* R_Tuple;
  char* Buf = (char*)malloc(2);
  int R_Num;
  double R_Dbl;
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],Buf,1,ERL_NIF_LATIN1)) exit(1);
  char R_Name = *Buf;
  free(Buf);
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
  char* Buf = (char*)malloc(2);
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],Buf,1,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = *Buf;
  free(Buf);
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
void reg_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::reg** reg) {
  const ERL_NIF_TERM* R_Tuple;
  char* Buf = (char*)malloc(2);
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],Buf,1,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = *Buf;
  free(Buf);
  if (R_Name != 'x') exit(1);
  *reg = new arg::reg(R_Num);
}

void lbl_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::label** Lbl) {
  const ERL_NIF_TERM* R_Tuple;
  char* Buf = (char*)malloc(2);
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],Buf,2,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  char R_Name = *Buf;
  free(Buf);
  if (R_Name != 'l') exit(1);
  *Lbl = new arg::label(R_Num);
}

address::address(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  mem_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);

  printf("Address: {<unknown>,%d} -> {y,%d}\n\r",src->number,dest->number);
}
allocate::allocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&bits)) exit(1);
  printf("Allocate: %d\n\r",bits);
}
call::call(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  printf("Call: <unknown>\n\r");
  // TODO: Get function call details
}
cast::cast(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&reg);
  const ERL_NIF_TERM* Type;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elems[1],NULL,&Type)) exit(1);
  // TODO: Decode & type
  printf("Cast: {x,%d} -> <unknown>\n\r", reg->number);
}
deallocate::deallocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&bits)) exit(1);
  printf("Allocate: %d\n\r",bits);
}
function::function(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  printf("Function: <unknown>\n\r");
  // TODO: Get function details
}
jump::jump(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  lbl_factory(Env,Elems[0],&lbl);
  printf("Jump: {l,%d}\n\r",lbl->number);
}
label::label(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&number)) exit(1);
  printf("Label: %d\n\r",number);
}
load::load(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  mem_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);

  if (src->code == arg::REG)
    printf("Load: {x,%d} -> {y,%d}\n\r",src->number,dest->number);
  else if (src->code == arg::STACK)
    printf("Load: {y,%d} -> {y,%d}\n\r",src->number,dest->number);
}
move::move(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  data_factory(Env,Elems[0],&src);
  mem_factory(Env,Elems[1],&dest);
  if (src->code == arg::REG)
    printf("Move: {x,%d}",dynamic_cast<ir::arg::reg*>(src)->number);
  else if (src->code == arg::STACK)
    printf("Move: {y,%d}",dynamic_cast<ir::arg::stack*>(src)->number);
  else if (src->code == arg::FLOAT)
    printf("Move: {float,%f}",dynamic_cast<ir::arg::floating*>(src)->value);
  else if (src->code == arg::INT)
    printf("Move: {integer,%d}",dynamic_cast<ir::arg::integer*>(src)->value);

  if (dest->code == arg::REG)
    printf(" -> {x,%d}\n\r",dest->number);
  else
    printf(" -> {y,%d}\n\r",dest->number);
}
// Empty constructor as return acts as end of statement only.
rtn::rtn() {}
store::store(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&src);
  reg_factory(Env,Elems[1],&dest);
  printf("Store: {x,%d} -> {x,%d}\n\r",src->number,dest->number);
}
test::test(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&reg);
  lbl_factory(Env,Elems[1],&lbl);
  printf("Test: {x,%d}? {l,%d}\n\r",reg->number,lbl->number);
}
bif::bif(ErlNifEnv* Env,const ERL_NIF_TERM* Elems,char* Operator) {
  printf("Bif...\n\r");
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
