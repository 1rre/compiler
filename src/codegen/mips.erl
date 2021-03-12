-module(mips).
-export([generate/2]).

-include("arch_type_consts.hrl").

-record(context,{fn=#{},types=#{},fp=0,s_reg=#{},reg=#{},labels=[],stack_size=0}).

generate(Ir,{file,_File}) ->
  Context = lists:foldl(fun
    ({function,Type,Name,Args,_},Cxt) ->
      Cxt#context{fn=maps:put(Name,{Type,Args},Cxt#context.fn)};
    ({global,Type,Name,_},Cxt) ->
      Cxt#context{types=maps:put({g,Name},Type,Cxt#context.types)}
    end, #context{}, Ir),

  {Data,Text} = lists:foldl(fun
    ({function,Type,Name,Args,St},{Data,Text}) ->
      {ok,Args_Asm,Args_Context} = gen_function_args(Args,Context),
      {ok,Scope_Asm,Scope_Context} = gen_scoped(St,Args_Context),
      Asm = Args_Asm ++ Scope_Asm,
      {Data,Text++Asm};
    ({global,Type,Name,Frame},{Data,Text}) ->
      % .bgnb Name
      Asm = gen_global(Frame,Name),
      %% TODO: Label for global
      %% Reversed order such that functions
      {Data++Asm,Text}
  end,{[],[]},lists:sort(Ir)),
  io:fwrite("~p~n~n~p~n",[Data,Text]),
  {ok,{['.data'|Data],['.text'|Text]}}.


%% TODO: get register positions for call
gen_function_args(Args,Context) ->
  {ok,[],Context}.

%% TODO: Translate statements
gen_scoped(St,Context) ->
  {ok,[],Context}.


gen_global(Frame,Name) ->
  Frame_Asm = gen_global(Frame,Name,[1]),
  case Frame_Asm of
    [{'.word',_}|_] ->
      [{'.globl',Name},{Name,[]}|Frame_Asm];
    _ ->
      [{'.globl',Name},{'.bgnb',Name}|Frame_Asm] ++
      gen_global_ptr(Frame,Name,[1]) ++
      [{Name,[]},{'.word',{Name,[1]}},'.endb']
  end.

gen_global({local,Frame},Name,Depth) ->
  lists:flatten(case hd(Frame) of
    {data,_,_} ->
      Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
      [{Name,Depth} | [gen_global(Data,Name,[N|Depth]) || {Data,N} <- Indices]];
    {local,_} ->
      Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
      [gen_global(Local,Name,[N|Depth]) || {Local,N} <- Indices]
  end);


gen_global({data,Type,Value},Name,Depth) ->
  {ok,St} = gen_data(Type,Value),
  St.

%% Char
gen_data({0,i,8},{i,Val}) ->
  {ok,[{'.byte',Val}]};
%% Short
gen_data({0,i,16},{i,Val}) ->
  {ok,[{'.half',Val}]};
%% Int or Long
gen_data({0,i,32},{i,Val}) ->
  {ok,[{'.word',Val}]};
%% Float
gen_data({0,f,32},{i,Val}) ->
  <<Word:32>> = <<Val:32/float>>,
  {ok,[{'.word',Word}]};
%% Double
gen_data({0,f,64},{i,Val}) ->
  <<Upper:32,Lower:32>> = <<Val:64/float>>,
  {ok,[{'.word',Upper},{'.word',Lower}]};
%% Pointer
gen_data({_,_,_},{i,Val}) ->
  {ok,[{'.word',Val}]}.

gen_global_ptr({local,Frame},Name,Depth) ->
  Local = case hd(Frame) of
    {local,_} ->
      Words = [{'.word',{Name,[N|Depth]}} || N <- lists:seq(1,length(Frame))],
      [{Name,Depth}|Words];
    _ -> []
  end,
  Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
  lists:flatten([gen_global_ptr(Sub_Frame,Name,[N|Depth]) || {Sub_Frame,N} <- Indices] ++ Local);

gen_global_ptr(_,_,_) -> [].
