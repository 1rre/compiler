#include <erl_nif.h>
#include <vector>
#include <stdio.h>
#include "erl_translator.hpp"


int X = 0;

static ERL_NIF_TERM get_ast(ErlNifEnv* Env, int Argc, const ERL_NIF_TERM Argv[]) {
  // Arity 1 function expects a single argument
  if (Argc != 1) return enif_make_badarg(Env);
  printf("X is %d\n", X);
  X++;
  ast::term* Ast = ast::translate(Env, Argv[0]);
  return enif_make_string(Env, Ast -> to_string().c_str(), ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"cpp_get_ast", 1, get_ast}
};

ERL_NIF_INIT(ast_nif, nif_funcs, NULL, NULL, NULL, NULL);
