-module(ir_vm).
-export([run/3,run/4]).

-define(STACK_PTR,16#7fffffff).
-define(GLOBL_PTR,16#10008000).

-include("arch_type_consts.hrl").

-record(context,{fn=main,global=#{},types=#{},reg=[],debug=false,s_count=0,
                 addr_buf=[],stack= <<>>,s_bounds=[?STACK_PTR]}).

run(Ir,Args,Flags) ->
  case lists:search(fun ({function,_,_,_,_}) -> true; (_) -> false end, Ir) of
    {value, {function,_,Fn,_,_}} -> run(Ir,Fn,Args,Flags);
    _ -> error({no_fn,Ir})
  end.

run(Ir,Fn,Args,Flags) ->
  {Init_Stack,Init_S_Bounds} = lists:foldl(fun
    (Arg,{Stack,[C1|Ch]}) ->
      {<<Arg:32,Stack/bits>>,[C1-4,C1|Ch]}
    end, {<<>>,[?STACK_PTR]}, Args),
  Init_Global = maps:from_list([{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir]),
  Init_Types = maps:from_list([{{y,N},{0,i,32}}||N<-lists:seq(0,length(Args)-1)]),
  Init_Buf = [get_literal_type(Arg)||Arg<-Args]++[{0,nil,0}],
  Context = #context{fn=Fn,
                     debug=lists:member(debug,Flags),
                     global=Init_Global,
                     types=Init_Types,
                     addr_buf=Init_Buf,
                     stack=Init_Stack,
                     s_bounds=Init_S_Bounds},
  if Context#context.debug ->
    io:fwrite("~p~n~n~n",[Ir]);
    true -> ok
  end,
  debug_print([{call,Fn,length(Args),{y,0}}],Context),
  {ok, End_Context} = call_fn(Fn,Context,Ir),
  if Context#context.debug ->
    io:fwrite("End Reg:~n~p~n",[End_Context#context.reg]);
    true -> ok
  end,
  lists:last(End_Context#context.reg).

call_fn(Fn,Context,Ir) ->
  case lists:search(fun ({function,_Type,Fun,_Arity,_St}) -> Fun =:= Fn end, Ir) of
    {value, {function,_Type,Fn,_Arity,St}} ->
      debug_print(St,Context),
      Result = run_st(St,Context,Ir),
      Result;
    _ -> error({not_found, Fn})
  end.

run_st([],Context,_Ir) ->
  {ok,Context};

run_st([return|_],Context,_Ir) ->
  {ok,Context};

%% TODO: N/A
%        Update local type register using cast
run_st([{cast,Reg,Type}|Rest],Context,Ir) ->
  Types = Context#context.types,
  N_Types = maps:put(Reg,Type,Types),
  {ok,Data} = get_data(Reg,Context),
  io:fwrite("~p~n",[cast(Data,Type)]),
  {ok,Cast_Context} = set_data(Type,Reg,cast(Data,Type),Context),
  Rtn_Context=Cast_Context#context{types=N_Types},
  debug_print(Rest,Cast_Context),
  run_st(Rest,Rtn_Context,Ir);

run_st([{allocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  S_Bounds = Context#context.s_bounds,
  N_Stack = <<Stack/bits,0:N>>,
  N_S_Bounds = [hd(S_Bounds) - N div 8 | S_Bounds],
  S_Count = Context#context.s_count,
  N_Types = maps:put({y,S_Count},{0,nil,N},Context#context.types),
  Buf = Context#context.addr_buf,
  N_Buf = [{0,nil,N} | Buf],
  N_Context = Context#context{stack=N_Stack,s_count=S_Count+1,types=N_Types,
                              addr_buf=N_Buf,s_bounds=N_S_Bounds},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{deallocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  Size = bit_size(Stack) - N,
  S_Bounds = Context#context.s_bounds,
  N_S_Bounds = rm_chunks(?STACK_PTR - Size div 8, S_Bounds),
  <<N_Stack:Size/bits,_/bits>> = Stack,
  Buf = Context#context.addr_buf,
  N_Buf = lists:nthtail(length(S_Bounds)-length(N_S_Bounds),Buf),
  N_Context = Context#context{stack=N_Stack,s_count=Context#context.s_count-1,
                              addr_buf=N_Buf,s_bounds=N_S_Bounds},
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

run_st([{address,Src,Dest}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {P,T,S} = maps:get(Src,Types,{0,nil,0}),
  {ok,Address} = get_address(Src,Context),
  Types = Context#context.types,
  N_Types = maps:put(Dest,{P+1,T,S},Types),
  {ok,N_Context} = set_data({P+1,T,S},Dest,Address,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

%% These 2 are fairly buggy (maybe due to inconsistencies in instr form?)
run_st([{load,{y,N},Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address({y,N},Context),
  {ok,Ptr} = get_data(Address,Context),
  {ok,Value} = get_data(Ptr,Context),
  Types = Context#context.types,
  {P,T,S} = maps:get({y,N},Types,{0,nil,0}),
  N_Types = maps:put(Dest,{P,T,S},Types),
  {ok,N_Context} = set_data({P,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{load,{x,N},Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address({y,N},Context),
  {ok,Value} = get_data(Address,Context),
  Types = Context#context.types,
  {P,T,S} = maps:get({x,N},Types,{0,nil,0}),
  N_Types = maps:put(Dest,{P,T,S},Types),
  {ok,N_Context} = set_data({P,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{store,Src,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Src,Context),
  {ok,Address} = get_address(Dest,Context),
  Types = Context#context.types,
  Type = maps:get(Src,Types,{0,nil,0}),
  {ok,N_Context} = set_data(Type,Address,Value,Context),
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

run_st([{move,Data,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Data,Context),
  Types = Context#context.types,
  {P,T,S} = case Data of
    {float,_} -> {0,f,64};
    {integer,_} -> {0,i,32};
    _ -> maps:get(Data,Types,{0,nil,0})
  end,
  N_Types = maps:put(Dest,{P,T,S},Types),
  {ok,N_Context} = set_data({P,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{label,_}|Rest],Context,Ir) ->
  debug_print(Rest,Context),
  run_st(Rest,Context,Ir);

run_st([{jump,{f,Lb}}|_],Context,Ir) ->
  jump(Lb,Context,Ir);

%% TODO: N/A
%        Add call function support to new type system
run_st([{call,Fn,Arity,{y,First}}|Rest],Context,Ir) ->
  Buf = Context#context.addr_buf,
  {Args,Other} = lists:split(length(Buf)-First-1,Buf),
  Fn_Context = Context#context{addr_buf=Args++[{0,nil,0}],fn=Fn},
  {ok,Fn_End} = call_fn(Fn,Fn_Context,Ir),
  N_Context = Fn_End#context{addr_buf=Other,fn=Context#context.fn},
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

run_st([{test,Data,{f,Lb}}|Rest],Context,Ir) ->
  {ok,Data_Val} = get_data(Data,Context),
  if
    Data_Val == 0 -> jump(Lb,Context,Ir);
    true ->
      debug_print(Rest,Context),
      run_st(Rest,Context,Ir)
  end;

%% / is a special case as erlang treats float & integer division differently
run_st([{'/',Dest,[A,B]}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {0,TA,SA} = maps:get(A,Types,{0,nil,0}),
  {0,TB,SB} = maps:get(B,Types,{0,nil,0}),
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  {ok,N_Context} = if
    (TA =:= f) or (TB =:= f) -> set_data({0,f,64},Dest,A_Val / B_Val,Context);
    true -> set_data({0,i,max(SA,SB)},Dest,A_Val div B_Val,Context)
  end,
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

%% TODO: N/A
%        Add pointer arithmetic here (Probably?)
run_st([{Op,Dest,[A,B]}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {PA,TA,SA} = maps:get(A,Types,{0,nil,0}),
  {PB,TB,SB} = maps:get(B,Types,{0,nil,0}),
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  {ok,N_Context} = if
    (TA =:= f) or (TB =:= f) -> set_data({0,f,64},Dest,do_op(Op,A_Val,B_Val),Context);
    true -> set_data({max(PA,PB),i,max(SA,SB)},Dest,do_op(Op,A_Val,B_Val),Context)
  end,
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

run_st(St,Context,_Ir) ->
  error({{unknown,St},{context,Context}}).


%% TODO: N/A
%        Floats need to be differentiated here
%        as they are currently stored as integers once being removed from the stack
get_data({integer,N},_Context) -> {ok,N};
get_data({float,N},_Context) -> {ok,N};
get_data({x,N},Context) ->
  Reg = Context#context.reg,
  {ok,lists:nth(length(Reg) - N, Reg)};
get_data({y,N},Context) ->
  Buf = Context#context.addr_buf,
  S_Bounds = Context#context.s_bounds,
  Nth = length(Buf)-N,
  get_data(lists:nth(Nth,Buf),lists:nth(Nth,S_Bounds),Context);
get_data(nil,_Context) ->
  {ok,nil};
get_data(Data,_Context) ->
error({unknown,Data}).

get_data({0,f,Size},Address,Context) when is_integer(Address) ->
  Offset = (?STACK_PTR - Address)*8,
  io:fwrite("~p, ~p~n",[Offset,Size]),
  <<_:Offset,Data:Size/float,_/bits>> = Context#context.stack,
  {ok,Data};
get_data({0,_,Size},Address,Context) when is_integer(Address) ->
  Offset = (?STACK_PTR - Address)*8,
  io:fwrite("~p, ~p~n",[Offset,Size]),
  <<_:Offset,Data:Size,_/bits>> = Context#context.stack,
  {ok,Data};
get_data(_,Address,Context) when is_integer(Address) ->
  Size = ?SIZEOF_POINTER,
  Offset = (?STACK_PTR - Address)*8,
  io:fwrite("~p, ~p~n",[Offset,Size]),
  <<_:Offset,Data:Size,_/bits>> = Context#context.stack,
  {ok,Data};
get_data(Type,Address,_Context) ->
  error({unknown,{type,Type},{address,Address}}).

set_data(Type,Dest,true,Context) -> set_data(Type,Dest,1,Context);
set_data(Type,Dest,false,Context) -> set_data(Type,Dest,0,Context);
set_data(_Type,{x,N},Data,Context) ->
  {ok,Context#context{reg=set_reg(Context#context.reg,N,Data)}};
set_data(Type,{y,N},Data,Context) ->
  Buf = Context#context.addr_buf,
  S_Bounds = Context#context.s_bounds,
  Nth = length(Buf)-N,
  N_Context = replace_buf_type(Nth,get_literal_type(Data),Context),
  set_data(Type,lists:nth(Nth, S_Bounds),Data,N_Context);

set_data({0,f,Size},Address,Data,Context) when is_integer(Address) ->
  Offset = (?STACK_PTR - Address)*8,
  <<Init:Offset,_:Size,Rest/bits>> = Context#context.stack,
  N_Stack = <<Init:Offset,Data:Size/float,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
  {ok,N_Context};
set_data({P,_,Raw_Size},Address,Data,Context) when is_integer(Address) ->
  Size = if P =:= 0 -> Raw_Size; true -> ?SIZEOF_POINTER end,
  Offset = (?STACK_PTR - Address)*8,
  <<Init:Offset,_:Size,Rest/bits>> = Context#context.stack,
  N_Stack = <<Init:Offset,Data:Size,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
  {ok,N_Context};
set_data(_Type,Dest,_N,_Data) -> error({no_loc,Dest}).

set_reg([_|Reg],N,Data) when length(Reg) =:= N -> [Data | Reg];
set_reg(Reg,N,Data) when length(Reg) =:= N -> [Data | Reg];
set_reg([],0,Data) -> [Data];
set_reg([Hd|Reg],N,Data) -> [Hd|set_reg(Reg,N,Data)].

get_address({y,N},Context) ->
  Buf = Context#context.addr_buf,
  S_Bounds = Context#context.s_bounds,
  Addr = lists:nth(length(Buf)-N, S_Bounds),
  {ok, Addr};
get_address({x,N},Context) ->
  get_data({x,N},Context);
get_address(_,_) -> error("").

%% TODO: N/A
%        Replace this function with inline/less buggy functions
%        We shouldn't have to exclusively access memory at boundaries
get_mem_size(C1,[C2,C1|_]) ->
  abs(C1-C2);
get_mem_size(C1,[C2,C3|Bounds]) when (C2 > C1) /= (C3 > C1) ->
  error({boundary_get,{C1,{C2,C3}}});
get_mem_size(C1,[_|Bounds]) ->
  get_mem_size(C1,Bounds).

jump(Lb,Context,Ir) ->
  Fn = Context#context.fn,
  case lists:search(fun ({function,_,Fun,_,_}) -> Fun =:= Fn end, Ir) of
    {value, {function,_,_,_,St}} ->
      Rest = find_lb(St,Lb),
      debug_print(Rest,Context),
      run_st(Rest,Context,Ir);
    _ -> error({not_found, Fn})
  end.

find_lb([{label,_Lb}|St],_Lb) -> St;
find_lb([_|St],Lb) -> find_lb(St,Lb);
find_lb(_,Lb) -> error({no_label,Lb}).

rm_chunks(C1,[C1|Bounds]) ->
  [C1|Bounds];
rm_chunks(C1,[C2,C3|Bounds]) when (C2 > C1) /= (C3 > C1) ->
  error({boundary_rm,{C1,{C2,C3}}});
rm_chunks(C1,[_|Bounds]) ->
  rm_chunks(C1,Bounds).

replace_buf_type(2,Type,[_|Buf]) -> [Type|Buf];
replace_buf_type(N,Type,[Hd|Buf]) -> [Hd|replace_buf_type(N-1,Type,Buf)];
replace_buf_type(N,Type,Context) ->
  N_Buf = replace_buf_type(N,Type,Context#context.addr_buf),
  Context#context{addr_buf=N_Buf}.

cast(Data,{0,i,Size}) ->
  io:fwrite("Old: ~p~n",[Data]),
  <<N_Data:Size>> = <<(trunc(Data)):Size>>,
  io:fwrite("New: ~p~n",[N_Data]),
  N_Data;
%% 32 bit floats are tricky so for now I'm going to just use 64 bit for everything
cast(Data,{0,f,Size}) ->
  <<N_Data:64/float>> = <<(float(Data)):64/float>>,
  N_Data;
cast(Data,{0,_,Size}) ->
    <<N_Data:Size>> = <<Data:Size>>,
    N_Data;
cast(Data,{_,_,_}) -> Data.


do_op('+',A,B) -> A+B;
do_op('-',A,B) -> A-B;
do_op('*',A,B) -> A*B;
do_op('%',A,B) -> A rem B;
do_op('==',A,B) -> A==B;
do_op('!=',A,B) -> A/=B;
do_op('>=',A,B) -> A>=B;
do_op('<=',A,B) -> B>=A;
do_op('>',A,B) -> A>B;
do_op('<',A,B) -> B>A;
do_op(Op,_,_) -> error({bif_not_recognised,Op}).

get_literal_type(_Val) when is_float(_Val) -> {0,f,64};
get_literal_type(_Val) when is_integer(_Val) -> {0,i,32};
get_literal_type(Val) -> error({{expected,{'float|int'}},{got,Val}}).

debug_print([Hd|_],Context) when Context#context.debug ->
  io:fwrite("Reg: ~p~nType Buffer: ~p~nTypes: ~p~nStack: ~p~nNext St: ~p~n~n",
            [Context#context.reg,Context#context.addr_buf,Context#context.types,Context#context.stack,Hd]);
debug_print(_,_) -> ok.
