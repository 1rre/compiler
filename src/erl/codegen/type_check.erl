-module(type_check).
-export([check/1]).
-on_load(init/0).
-export([get_var_context/2,
         set_var_context/3,
         make_var_context/3,
         get_type_context/2,
         set_type_context/2]).

init() ->
  ok = erlang:load_nif(".build/type_context",0).

-type var_type() atom().
-type ident() atom().
-type value() any().
-type type() typedef | enum | struct | union | any.
-type context() global | ident().

-spec get_var_context(var_type(),ident()) -> {error, term()} | {cast, var_type()} | ok.
get_var_context(_,_) -> error("Nif not loaded").

-spec set_var_context(var_type(),ident(),value()) -> {error, term()} | {cast, var_type()} | ok.
set_var_context(_,_,_) -> error("Nif not loaded").

-spec make_var_context(var_type(),ident(),context()) -> {error, term()} | ok.
make_var_context(_,_,_) -> error("Nif not loaded").

-spec get_type_context(type(),ident()) -> {error, term()} | {ok, var_type()}.
get_type_context(_,_) -> error("Nif not loaded").

-spec set_type_context(type(),ident()) -> {error, term()} | ok.
set_type_context(_,_) -> error("Nif not loaded").

check(Ast) -> [check_type(St) || St <- Ast].

check_type(St) -> ok.
