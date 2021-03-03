-module(ir2mips).
-export([init/0,translate/1]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif(".build/erl_nif", 0).

translate(_) ->
  error(nif_not_loaded).
  
