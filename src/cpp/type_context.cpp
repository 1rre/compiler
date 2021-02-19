#include <erl_nif.h>
#include "type_context.hpp"
#include "erl_translator.hpp"

static ERL_NIF_TERM get_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 1) return enif_make_badarg(Env);
  ast::atom Var_Name(Env, *Argv);
  context::Var[Var_Name];
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM set_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 2) return enif_make_badarg(Env);
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM make_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 2) return enif_make_badarg(Env);
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM get_type_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv){
  if (Argc != 1) return enif_make_badarg(Env);
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM set_type_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv){
  if (Argc != 2) return enif_make_badarg(Env);
  return enif_make_atom(Env, "ok");
}


static ErlNifFunc nif_funcs[] = {
  {"get_var_context", 1, get_var_context},
  {"set_var_context", 2, set_var_context},
  {"make_var_context", 2, make_var_context},
  {"get_type_context", 1, get_type_context},
  {"set_type_context", 2, set_type_context}
};

ERL_NIF_INIT(ast2beam,nif_funcs,NULL,NULL,NULL,NULL);

