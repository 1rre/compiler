-module(c89_compiler).
-export([start/0, init/0, tokenise/1]).

-record(state, {port, node}).

start() ->
  register(c89_compiler, spawn(?MODULE, init, [])).
init() ->
  loop(#state{}).

loop(State) ->
  receive
    Info ->
      io:fwrite("Recieved: ~w~n", [Info]),
      loop(State)
  end.

tokenise(C89_Code) ->
  {ok, Tokens, _} = lexer:string(C89_Code),
  {ok, Result} = parser:parse(Tokens),
  Result.    
