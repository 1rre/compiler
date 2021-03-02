#include "erl_translator.hpp"

namespace ir {

std::vector<statement::statement*> get_ir(ErlNifEnv* Env, ERL_NIF_TERM Ir) {
  unsigned Length;
  std::vector<statement::statement*> Rtn;
  //TODO: return error
  if (!enif_get_list_length(Env,Ir,&Length)) return Rtn;
  
}

}
