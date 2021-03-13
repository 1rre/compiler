-module(mips).
-export([generate/2]).

-include("arch_type_consts.hrl").

-record(context,{fn=#{},types=#{},sp=0,s_reg=#{},reg=#{},i_reg,f_reg,labels=[],stack_size=0}).

generate(Ir,{file,_File}) ->
  Reg_Pref = ['$2','$3','$8','$9','$10','$11','$12','$13','$14','$15','$24','$25',
              {s,'$16'},{s,'$17'},{s,'$18'},{s,'$19'},{s,'$20'},{s,'$21'},{s,'$22'},{s,'$23'}],

  Float_Reg = ['$f0','$f1','$f2','$f3','$f4','$f5','$f6','$f7','$f8','$f9','$f10','$f11','$f12',
               '$f13','$f14','$f15','$f16','$f17','$f18','$f19','$f20','$f21','$f22','$f23','$f24',
               '$f25','$f26','$f27','$f28','$f29','$f30','$f31'],
  Context = lists:foldl(fun
    ({function,Type,Name,Args,_},Cxt) ->
      Cxt#context{fn=maps:put(Name,{Type,Args},Cxt#context.fn)};
    ({global,Type,Name,_},Cxt) ->
      Cxt#context{types=maps:put({g,Name},Type,Cxt#context.types)}
    end, #context{i_reg=Reg_Pref,f_reg=Float_Reg}, Ir),

  {Data,Text} = lists:foldl(fun
    ({function,Type,Name,Args,St},{Data,Text}) ->
      {ok,Args_Context} = gen_arg_types(Args,Context,0),
      {ok,Scope_Asm,Scope_Context} = gen_scoped(St,Args_Context),
      Asm = Scope_Asm,
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
gen_arg_types([Hd|Args],Context,N) ->
  Types = maps:put({z,N},Hd,Context#context.types),
  gen_arg_types(Args,Context#context{types=Types},N+1);
gen_arg_types([],Context,_) -> {ok,Context}.


%% Stack Resize
% I know this isn't technically necessary for C
% but it's built into the IR now and it'd be difficult to change
gen_scoped([{allocate,Size}|Rest],Context) ->
  N = Context#context.stack_size,
  Sp = Context#context.sp,
  N_S_Reg = maps:put({y,N},Sp,Context#context.s_reg),
  N_Types = maps:put({y,N},{0,n,Size},Context#context.types),
  N_Context = Context#context{s_reg=N_S_Reg,
                              sp=Sp-(Size div 8),
                              stack_size=N+1,
                              types=N_Types},
  [{addiu,'$sp','$sp',-(Size div 8)}|gen_scoped(Rest,N_Context)];

%% Moving a 32 bit (maximum) constant to a register
gen_scoped([{move,{i,Val},{x,N}}|Rest],Context) when 16#7FFFFFFF >= Val
                                                andalso Val >= -16#8000000 ->
  {ok,Reg,Reg_Context} = get_reg({x,N},{0,i,32},Context),
  [{li,Reg,Val}|gen_scoped(Rest,Context)];


gen_scoped([],Context) -> [];
gen_scoped([Other|_],Context) -> error({not_implemented,Other}).

get_reg(Reg,{0,_,64},Context) ->
  error({not_impl,long});
get_reg(Reg,{0,f,32},Context) ->
  % Check if the register has been previously assigned
  case {maps:get(Reg,Context#context.types,nil),Context#context.f_reg} of
    % It has not
    {nil,[Dest|Rest]} ->
      N_Types = maps:put(Reg,{0,f,32},Context#context.types),
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=Rest,reg=N_Reg,types=N_Types}};
    % It has and is already a float
    {{0,f,32},_} ->
      Dest = maps:get(Reg,Context#context.reg),
      {ok,Dest,Context};
    %% TODO: Implement other float registers
    _ -> error
  end;
get_reg(Reg,Type,Context) ->
  case {maps:get(Reg,Context#context.types,nil),Context#context.i_reg} of
    {{0,f,32},_} ->
      error(float_reg_to_int);
    {{0,_,64},_} ->
      error(resize_reg);
    {_,[Dest|Rest]} ->
      N_Types = maps:put(Reg,Type,Context#context.types),
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{i_reg=Rest,reg=N_Reg,types=N_Types}}
  end.

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
