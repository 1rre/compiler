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

tokenise(Source) ->
  Code = trigraph(Source),
  {ok, Tokens, _} = lexer:string(Code),
  {ok, Result} = parser:parse(Tokens),
  Result.    

trigraph([]) -> [];
trigraph([$?,$?,$= | Source]) -> [$# | trigraph(Source)];
trigraph([$?,$?,$( | Source]) -> [$[ | trigraph(Source)];
trigraph([$?,$?,$) | Source]) -> [$] | trigraph(Source)];
trigraph([$?,$?,$/ | Source]) -> [$\\ | trigraph(Source)];
trigraph([$?,$?,$' | Source]) -> [$^ | trigraph(Source)];
trigraph([$?,$?,$< | Source]) -> [${ | trigraph(Source)];
trigraph([$?,$?,$> | Source]) -> [$} | trigraph(Source)];
trigraph([$?,$?,$! | Source]) -> [$| | trigraph(Source)];
trigraph([$?,$?,$- | Source]) -> [$~ | trigraph(Source)];
trigraph([Ch       | Source]) -> [Ch | trigraph(Source)].

