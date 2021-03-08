-module(mips).
-export([generate/1]).

-include("arch_type_consts.hrl").

%% TODO: Replace this with a more relevant one
-record(context,{fn=main,global=#{},types=#{},reg=[],debug=false,
                 args=[],stack= <<>>,s_reg=[],fp=nil}).

generate(Ir) ->
  generate(Ir,#context{}).

generate(Ir,Context) ->
  io:fwrite("~p~n~n~p~n",[Ir,Context]).
