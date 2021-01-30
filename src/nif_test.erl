-module(nif_test).
-export([send/1, init/0, cpp_get_ast/1]).
-on_load(init/0).

init() -> 
  erlang:load_nif(".build/nif_test", 0).

cpp_get_ast(_List) ->
  error("Nif not loaded!").

send(Output) ->
  io:fwrite("~p~n", [cpp_get_ast(Output)]),
  halt(0).

