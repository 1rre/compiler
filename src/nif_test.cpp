#include <erl_nif.h>
#include <vector>
#include <iostream>


static ERL_NIF_TERM get_ast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  std::vector<ERL_NIF_TERM> terms;
  ERL_NIF_TERM head, tail = argv[0];
  unsigned enif_length;
  if (!enif_get_list_length(env, tail, &enif_length)) exit(1);
  for (unsigned i = 0; i < enif_length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) exit(1);
    terms.push_back(head);
  }
  const ERL_NIF_TERM* tuple;
  int arity;

  if (!enif_get_tuple(env, terms[0], &arity, &tuple)) exit(1);

  printf("arity of first tuple is: %d\n", arity);  

  return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"cpp_get_ast", 1, get_ast}
};

ERL_NIF_INIT(nif_test, nif_funcs, NULL, NULL, NULL, NULL);
