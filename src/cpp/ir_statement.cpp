
#include "ir_statement.hpp"
#include <erl_nif.h>
#include <stdio.h>

namespace ir::statement {

void data_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::data* Data) {
  const ERL_NIF_TERM* R_Tuple;
  char R_Name;
  int R_Num;
  double R_Dbl;
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],&R_Name,1,ERL_NIF_LATIN1)) exit(1);
  switch (R_Name) {
    case 'x':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = arg::reg(R_Num);
    break;
    case 'y':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = arg::stack(R_Num);
    break;
    case 'f':
      if (!enif_get_double(Env,R_Tuple[1],&R_Dbl)) exit(1);
      *Data = arg::floating(R_Dbl);
    break;
    case 'i':
      if (!enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
      *Data = arg::integer(R_Num);
    break;
    default:
      exit(1);
    }
}

void mem_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::memory* Mem) {
  const ERL_NIF_TERM* R_Tuple;
  char R_Name;
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],&R_Name,1,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  switch (R_Name) {
    case 'x':
      *Mem = arg::reg(R_Num);
    break;
    case 'y':
      *Mem = arg::stack(R_Num);
    break;
    default:
      exit(1);
  }
}
void reg_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::reg* Reg) {
  const ERL_NIF_TERM* R_Tuple;
  char R_Name;
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],&R_Name,1,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  if (R_Name != 'x') exit(1);
  *Reg = arg::reg(R_Num);
}

void lbl_factory(ErlNifEnv* Env,const ERL_NIF_TERM Elem,arg::label* Lbl) {
  const ERL_NIF_TERM* R_Tuple;
  char R_Name;
  int R_Num;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elem,NULL,&R_Tuple) ||
      !enif_get_atom(Env,R_Tuple[0],&R_Name,1,ERL_NIF_LATIN1) ||
      !enif_get_int(Env,R_Tuple[1],&R_Num)) exit(1);
  if (R_Name != 'l') exit(1);
  *Lbl = arg::label(R_Num);
}

address::address(ErlNifEnv* Env,const ERL_NIF_TERM* Elems):statement() {
  mem_factory(Env,Elems[0],&value.mem_reg.src);
  reg_factory(Env,Elems[1],&value.mem_reg.dest);

  printf("Address: {<unknown>,%d} -> {y,%d}\n",value.mem_reg.src.value.number,
                                               value.mem_reg.dest.value.number);
}
allocate::allocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&value.bits)) exit(1);
  printf("Allocate: %d\n",value.bits);
}
call::call(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  printf("Call: <unknown>\n");
  // TODO: Get function call details
}
cast::cast(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&value.cast.reg);
  const ERL_NIF_TERM* Type;
  // TODO: Better error code
  if (!enif_get_tuple(Env,Elems[1],NULL,&Type)) exit(1);
  // TODO: Decode & type
  printf("Cast: {x,%d} -> <unknown>\n", value.cast.reg.value.number);
}
deallocate::deallocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&value.bits)) exit(1);
  printf("Allocate: %d\n",value.bits);
}
function::function(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  printf("Function: <unknown>\n");
  // TODO: Get function details
}
jump::jump(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  lbl_factory(Env,Elems[0],&value.label.value);
  printf("Jump: {l,%d}\n",value.label.value.number);
}
label::label(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  // TODO: Better error code
  if(!enif_get_int(Env,Elems[0],&value.number)) exit(1);
  printf("Label: %d\n",value.number);
}
load::load(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  mem_factory(Env,Elems[0],&value.mem_reg.src);
  reg_factory(Env,Elems[1],&value.mem_reg.dest);

  if (value.mem_reg.src.code == arg::REG)
    printf("Load: {x,%d} -> {y,%d}\n",value.mem_reg.src.value.number,
                                      value.mem_reg.dest.value.number);
  else if (value.mem_reg.src.code == arg::STACK)
    printf("Load: {y,%d} -> {y,%d}\n",value.mem_reg.src.value.number,
                                      value.mem_reg.dest.value.number);
}
move::move(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  data_factory(Env,Elems[0],&value.data_mem.src);
  mem_factory(Env,Elems[1],&value.data_mem.dest);
  if (Src->Code == arg::REG)
    printf("Address: {x,%d} -> {y,%d}\n",value.data_mem.src.value.number,
                                         value.data_mem.dest.value.number);
  else if (Src->Code == arg::STACK)
    printf("Address: {x,%d} -> {y,%d}\n",value.data_mem.src.value.number,
                                         value.data_mem.dest.value.number);

}
// Empty constructor as return acts as end of statement only.
rtn::rtn() {}
store::store(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&Src);
  reg_factory(Env,Elems[1],&Dest);
}
test::test(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {
  reg_factory(Env,Elems[0],&Reg);
  lbl_factory(Env,Elems[1],&Label);
}
bif::bif(ErlNifEnv* Env,const ERL_NIF_TERM* Elems,char* Operator) {
  // This is nasty but what can you do :)
  switch (Operator[0]) {
    case '|': switch (Operator[1]) {
      case '\0':
        Code=BOR;
      break;
      case '|':
        Code=LOR;
      break;
      default:
        Code=ERR;
    } break;
    case '&': switch (Operator[1]) {
      case '\0':
        Code=BND;
        break;
      case '&':
        Code=LND;
        break;
      default:
        Code=ERR;
    } break;
    case '^':
      Code=BXR;
      break;
    case '=':
      Code=EEQ;
      break;
    case '!':
      Code=IEQ;
      break;
    case '<': switch (Operator[1]) {
      case '\0':
        Code=LES;
        break;
      case '<':
        Code=SHL;
        break;
      case '=':
        Code=LEQ;
        break;
      default:
        Code=ERR;
    } break;
    case '>': switch (Operator[1]) {
      case '\0':
        Code=GRT;
        break;
      case '>':
        Code=SHR;
        break;
      case '=':
        Code=GEQ;
        break;
      default:
        Code=ERR;
    } break;
    case '+':
      Code=ADD;
      break;
    case '-':
      Code=SUB;
      break;
    case '*':
      Code=MUL;
      break;
    case '/':
      Code=DIV;
      break;
    case '%':
      Code=REM;
      break;
    default:
      Code=ERR;
      break;
  }

  reg_factory(Env,Elems[0],&Dest);
  ERL_NIF_TERM Head;
  ERL_NIF_TERM* Tail;
  enif_get_list_cell(Env, Elems[1], &Head, Tail);
  reg_factory(Env,Head,&A);
  enif_get_list_cell(Env, *Tail, &Head, Tail);
  reg_factory(Env,Head,&B);
}


statement* factory(ErlNifEnv* Env, ERL_NIF_TERM St) {
  if (enif_is_atom(Env,St)) return new rtn();
  int Arity;
  const ERL_NIF_TERM* Elems;
  if (!enif_get_tuple(Env,St,&Arity,&Elems)) return nullptr;
  unsigned Length;
  char* Atom;
  if (!enif_get_atom_length(Env,Elems[0],&Length,ERL_NIF_LATIN1) ||
      !enif_get_atom(Env,Elems[0],Atom,Length,ERL_NIF_LATIN1)) return nullptr;

  // Ignore this I know it's nasty sorry
  switch (Atom[0]) {
    case 'a': switch (Atom[1]) {
      case 'd': return new address(Env,Elems+1);
      case 'l': return new allocate(Env,Elems+1);
      default: return nullptr;
    }
    case 'c': switch (Atom[2]) {
      case 'l': return new call(Env,Elems+1);
      case 's': return new cast(Env,Elems+1);
      default: return nullptr;
    }
    case 'd': return new deallocate(Env,Elems+1);
    case 'f': return new function(Env,Elems+1);
    case 'j': return new jump(Env,Elems+1);
    case 'l': switch (Atom[1]) {
      case 'a': return new label(Env,Elems+1);
      case 'o': return new load(Env,Elems+1);
      default: return nullptr;
    }
    case 'm': return new move(Env,Elems+1);
    case 's': return new store(Env,Elems+1);
    case 't': return new test(Env,Elems+1);
    default: return new bif(Env,Elems+1,Atom);
  }
}
}
