-module(ast_nif).
-export([send/1, init/0, cpp_get_ast/1]).
-on_load(init/0).

init() -> 
  erlang:load_nif(".build/ast_nif", 0).

cpp_get_ast(_List) ->
  error("Nif not loaded!").

send(Output) ->
  io:fwrite("Sending~n"),
  io:fwrite("~s~n", [cpp_get_ast(Output)]).

