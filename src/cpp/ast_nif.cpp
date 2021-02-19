#include <erl_nif.h>
#include <vector>
#include <stdio.h>
#include "erl_translator.hpp"
#include "ast_2_beam.hpp"


static ERL_NIF_TERM get_ast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  // Arity 1 function expects a single argument
  if (argc != 1) return enif_make_badarg(env);
  ast::term* ast = ast::translate(env, argv[0]);
  gen(ast);
  return enif_make_string(env, ast -> to_string().c_str(), ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"cpp_get_ast", 1, get_ast}
};

ERL_NIF_INIT(ast_nif, nif_funcs, NULL, NULL, NULL, NULL);

