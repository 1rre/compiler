
#include "ir_statement.hpp"
#include <erl_nif.h>

namespace ir::statement {
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
      case 'd':
        return new address(Env,Elems+1);
      case 'l':
        return new allocate(Env,Elems+1);
    }
    case 'c': switch (Atom[2]) {
      case 'l':
        return new call(Env,Elems+1);
      case 's':
        return new cast(Env,Elems+1);
    }
    case 'd':
      return new deallocate(Env,Elems+1);
    case 'f':
      return new function(Env,Elems+1);
    case 'j':
      return new jump(Env,Elems+1);
    case 'l': switch (Atom[1]) {
      case 'a':
        return new label(Env,Elems+1);
      case 'o':
        return new load(Env,Elems+1);
    }
    case 'm':
      return new move(Env,Elems+1);
    case 's':
      return new store(Env,Elems+1);
    case 't':
      return new test(Env,Elems+1);
    default:
      return new bif(Env,Elems+1,Atom);
  }
}
}
