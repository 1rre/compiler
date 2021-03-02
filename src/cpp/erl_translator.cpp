#include "erl_translator.hpp"

namespace ir {

std::vector<statement::statement*> get_ir(ErlNifEnv* Env, ERL_NIF_TERM Ir) {
  unsigned Length;
  std::vector<statement::statement*> Rtn;

  if (!enif_get_list_length(Env,Ir,&Length)) return {};

  ERL_NIF_TERM Head;

  for (unsigned I = 0; I < Length; I++)
    if (!enif_get_list_cell(Env,Ir,&Head,&Ir)) return {};
    else Rtn.push_back(statement::factory(Env,Head));

  return Rtn;
}

}
