-module(nif_test).
-export([send/1, init/0, cpp_get_ast/1]).
-on_load(init/0).

init() -> 
  io:fwrite(os:get_env_var("NIF_DIR")),
  Fileloc = filename:join(code:priv_dir(c89_compiler),"nif_test"),
  erlang:load_nif(Fileloc, 0).

cpp_get_ast(_List) ->
  error("Nif not loaded!").

send(Output) ->
  io:fwrite("~p~n", [cpp_get_ast(Output)]),
  halt(0).

