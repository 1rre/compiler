
#include "ir_statement.hpp"
#include <erl_nif.h>

namespace ir::statement {
address::address(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
allocate::allocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
call::call(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
cast::cast(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
deallocate::deallocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
function::function(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
jump::jump(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
label::label(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
load::load(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
move::move(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
rtn::rtn() {}
store::store(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
test::test(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
bif::bif(ErlNifEnv* Env,const ERL_NIF_TERM* Elems,char* Operator) {
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
