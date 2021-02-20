#include <erl_nif.h>
#include <map>
#include "erl_translator.hpp"

namespace context {
  std::map<ast::term*, ast::term*> Typedef;
  std::map<ast::term*, ast::term*> Enum;
  std::map<ast::term*, ast::term*> Var;
  std::map<ast::term*, ast::term*> Fun;
}

static ERL_NIF_TERM get_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 2) return enif_make_badarg(Env);
  ast::term* Expected_Type = ast::translate(Env, Argv[0]);
  ast::term* Identifier = ast::translate(Env, Argv[1]);
  if (context::Var.find(Identifier) == context::Var.end())
    return enif_make_tuple2(Env, enif_make_atom(Env, "Error"),
                            enif_make_string(Env, "Error", ERL_NIF_LATIN1));
  ast::term* Value = context::Var[Identifier];
  return Value->to_erl(Env);
}

static ERL_NIF_TERM set_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 3) return enif_make_badarg(Env);
  ast::term* Type = ast::translate(Env, Argv[0]);
  ast::term* Identifier = ast::translate(Env, Argv[0]);
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM make_var_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv) {
  if (Argc != 3) return enif_make_badarg(Env);
  return enif_make_atom(Env, "ok");
}

static ERL_NIF_TERM get_type_context(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM* Argv){
  if (Argc != 2) return enif_make_badarg(Env);
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
