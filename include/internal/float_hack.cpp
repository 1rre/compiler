#include <iostream>
#include "erl_nif.h"

#define LOG_2_10 3.321928094887362



static ERL_NIF_TERM float_hack(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  double left;
  long right;
  if (!enif_get_double(env, argv[0], &left) || !enif_get_long(env, argv[1], &right)) {
    return enif_make_badarg(env);
  }
  right *= LOG_2_10;
  unsigned long left_d = *(long*)(&left);
  unsigned long man = 18442240474082181120ul&left_d;
  left_d = (4503599627370496ul & (left_d*right)) | man;
  return(enif_make_double(env, *(double*)(&left_d)));

    
}

static ErlNifFunc nif_funcs[] = {{"float_hack", 2, float_hack}};

ERL_NIF_INIT(floaty, nif_funcs, NULL, NULL, NULL, NULL)
