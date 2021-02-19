-module(type_check).
-export([convert/1]).
-on_load(init/0).
-export([get_var_context/1,
         set_var_context/2,
         make_var_context/2,
         get_type_context/1,
         set_type_context/2]).

init() ->
  ok = erlang:load_nif(".build/type_context",0).

get_var_context(_)      -> error("Nif not loaded").
set_var_context(_,_)    -> error("Nif not loaded").
make_var_context(_,_) -> error("Nif not loaded").
get_type_context(_)     -> error("Nif not loaded").
set_type_context(_,_)   -> error("Nif not loaded").

convert(Ast) ->
  Statements = [gen_beam(St) || St <- Ast],
  string:join("\n", Statements).

gen_beam(St) ->
  ok.


