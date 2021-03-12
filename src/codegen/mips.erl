-module(mips).
-export([generate/2]).

-include("arch_type_consts.hrl").

-record(context,{fn=#{},types=#{},fp=0,s_reg=[],reg=[],labels=[],stack_size=0}).

generate(Ir,{file,_File}) ->
  Context = lists:foldl(fun
    (Cxt,{function,Type,Name,Args,_}) ->
      Cxt#context{fn=maps:put(Name,{Type,Args},Cxt#context.fn)};
    (Cxt,{global,Type,Name,_}) ->
      Cxt#context{types=maps:put({g,Name},Type,Cxt#context.types)}
    end, #context{}, Ir),
  {Result,_} = lists:foldl(fun
    (St,Asm) ->
      {ok,Generated} = global(St,Context),
      Asm++Generated
  end,{[],#context{}},Ir),
  {ok,Result}.

global({function,Type,Name,Args,St},Context) ->
  resolve_args.
