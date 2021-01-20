-module(c89_compiler).
-behaviour(application).
-export([start/2, stop/1, start/0, init/0]).

-record(state, {port, node}).

start() ->
  register(c89_compiler, spawn(?MODULE, init, [])).

start(_,_) -> start().

stop(_) -> ok.

init() ->
  loop(#state{}).

loop(State) ->
  receive
    Info ->
      io:fwrite("Recieved: ~w~n", [Info]),
      loop(State)
  end.
    
