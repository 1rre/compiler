-module(c89_compiler).
-export([main/1]).

main(Args) ->
  io:fwrite("~p~n", [Args]),
  halt(0).
