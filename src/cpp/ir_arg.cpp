#include <erl_nif.h>
#include <vector>
#include "ir_arg.hpp"

namespace ir::arg {

  floating::floating(double Val):
    value(Val) {}
  integer::integer(int Val):
    value(Val) {}
  memory::memory(int Num):
    number(Num) {}
  reg::reg(int Num):
    memory(Num) {}
  stack::stack(int Num):
    memory(Num) {}
  label::label(int Num):
    number(Num) {}
  type::type(int P, char T, int S):
    ref_level(P),
    data_type(T),
    width(S) {}

  arg* factory(ErlNifEnv* Env, ERL_NIF_TERM Arg) {
    int Arity;
    const ERL_NIF_TERM* Elems;

    if (!enif_get_tuple(Env,Arg,&Arity,&Elems)) return nullptr;

    switch (Arity) {
      case 2:
        char Arg_Type;
        if(!enif_get_atom(Env,Elems[0],&Arg_Type,1u,ERL_NIF_LATIN1)) return nullptr;
        int Int;
        switch (Arg_Type) {
          case 'x':
            if(!enif_get_int(Env,Elems[1],&Int)) return nullptr;
            return new reg(Int);
          case 'y':
            if(!enif_get_int(Env,Elems[1],&Int)) return nullptr;
            return new stack(Int);
          case 'l':
            if(!enif_get_int(Env,Elems[1],&Int)) return nullptr;
            return new label(Int);
          case 'i':
            if(!enif_get_int(Env,Elems[1],&Int)) return nullptr;
            return new integer(Int);
          case 'f':
            double Double;
            if(!enif_get_double(Env,Elems[1],&Double)) return nullptr;
            return new floating(Double);
          default:
            return nullptr;
        }
      case 3:
        int Ref_Level,Width;
        char Type_code;
        if (!enif_get_int(Env,Elems[0],&Ref_Level) ||
            !enif_get_atom(Env,Elems[1],&Type_code,1u,ERL_NIF_LATIN1) ||
            !enif_get_int(Env,Elems[2],&Width)) return nullptr;
        return new type(Ref_Level,Type_code,Width);
      default:
        return nullptr;
    }
  }
}
