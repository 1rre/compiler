-module(ir_vm).
-export([run/3,run/4]).

-define(STACK_PTR,16#7fffffff).
-define(GLOBL_PTR,16#10008000).

-include("arch_type_consts.hrl").

-record(context,{fn=main,global=#{},types=#{},reg=[],debug=false,
                 args=[],stack= <<>>,s_reg=[],fp=?STACK_PTR}).

run(Ir,Args,Flags) ->
  case lists:search(fun ({function,_,_,_,_}) -> true; (_) -> false end, Ir) of
    {value, {function,_,Fn,_,_}} -> run(Ir,Fn,Args,Flags);
    _ -> error({no_fn,Ir})
  end.

run(Ir,Fn,Args,Flags) ->
  Init_Args = Args,
  Init_Global = maps:from_list([{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir]),
  Init_Types = maps:from_list([{{z,N},{0,i,?SIZEOF_INT}}||N<-lists:seq(0,length(Args)-1)]),
  Context = #context{fn=Fn,
                     debug=lists:member(debug,Flags),
                     global=Init_Global,
                     types=Init_Types,
                     args=Args},
  if Context#context.debug ->
    io:fwrite("~100p~n~n~n",[Ir]);
    true -> ok
  end,
  debug_print([{call,Fn,length(Args)}],Context),
  {ok, End_Context} = call_fn(Fn,Context,Ir),
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
  debug_print(['end'],Context),
  {ok,Context};

%% TODO: N/A
%        Update local type register using cast
run_st([{cast,{x,N},Type}|Rest],Context,Ir) ->
  Reg = {x,N},
  Types = Context#context.types,
  N_Types = maps:put(Reg,Type,Types),
  {ok,Data} = get_data(Reg,Context),
  {ok,Cast_Context} = set_data(Type,Reg,cast(Data,Type),Context),
  Rtn_Context=Cast_Context#context{types=N_Types},
  debug_print(Rest,Cast_Context),
  run_st(Rest,Rtn_Context,Ir);

run_st([{cast,{y,N},Type}|Rest],Context,Ir) ->
  Reg = {y,N},
  Types = Context#context.types,
  N_Types = maps:put(Reg,Type,Types),
  Rtn_Context=Context#context{types=N_Types},
  debug_print(Rest,Rtn_Context),
  run_st(Rest,Rtn_Context,Ir);

run_st([{allocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  S_Reg = Context#context.s_reg,
  N_Stack = <<0:N,Stack/bits>>,
  Types = Context#context.types,
  N_S_Reg = [?STACK_PTR - byte_size(Stack) - N div 8 | S_Reg],
  S_Count = length(S_Reg),
  N_Types = maps:put({y,S_Count},{0,n,N},Types),
  N_Context = Context#context{stack=N_Stack,types=N_Types,s_reg=N_S_Reg},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{gc,0}|Rest],Context,Ir) ->
  Fp = Context#context.fp,
  Stack = Context#context.stack,
  Offset = (byte_size(Stack) - ?STACK_PTR + Fp) * 8,
  <<_:Offset,N_Stack/bits>> = Stack,
  N_S_Reg = [],
  N_Context = Context#context{stack=N_Stack,s_reg=N_S_Reg},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{gc,N}|Rest],Context,Ir) when N >= length(Context#context.s_reg) ->
  run_st(Rest,Context,Ir);
run_st([{gc,N}|Rest],Context,Ir) ->
  S_Reg = Context#context.s_reg,
  Address = lists:nth(length(S_Reg)-N+1,S_Reg),
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address) * 8,
  <<_:Offset,N_Stack/bits>> = Stack,
  N_S_Reg = lists:nthtail(length(S_Reg)-N,S_Reg),
  N_Context = Context#context{stack=N_Stack,s_reg=N_S_Reg},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{test_heap,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  N_Stack = <<0:N,Stack/bits>>,
  %Old_Reg = Context#context.reg,
  %Reg = set_reg(Old_Reg,Rn,?STACK_PTR-byte_size(Stack)),
  N_Context = Context#context{stack=N_Stack},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{address,Src,Dest}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {P,T,S} = maps:get(Src,Types,{0,n,0}),
  {ok,Address} = get_address(Src,Context),
  Types = Context#context.types,
  N_Types = maps:put(Dest,{P+1,T,S},Types),
  {ok,N_Context} = set_data({P+1,T,S},Dest,Address,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

%% These 2 are fairly buggy (maybe due to inconsistencies in instr form?)
run_st([{load,{y,N},Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address({y,N},Context),
  Types = Context#context.types,
  {P,T,S} = maps:get({y,N},Types,{0,n,0}),
  {ok,Ptr} = get_data({P,T,S},Address,Context),
  {ok,Value} = get_data({P-1,T,S},Ptr,Context),
  N_Types = maps:put(Dest,{P-1,T,S},Types),
  {ok,N_Context} = set_data({P-1,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{load,{x,N},Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address({x,N},Context),
  Types = Context#context.types,
  {P,T,S} = maps:get({x,N},Types,{0,n,0}),
  {ok,Value} = get_data({P-1,T,S},Address,Context),
  N_Types = maps:put(Dest,{P-1,T,S},Types),
  {ok,N_Context} = set_data({P-1,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{store,Src,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Src,Context),
  {ok,Address} = get_address(Dest,Context),
  Types = Context#context.types,
  Type = maps:get(Src,Types,{0,n,0}),
  {ok,N_Context} = set_data(Type,Address,Value,Context),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{move,Data,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Data,Context),
  Types = Context#context.types,
  {P,T,S} = case Data of
    {f,_} -> {0,f,?SIZEOF_FLOAT};
    {i,_} -> {0,i,?SIZEOF_INT};
    _ -> maps:get(Data,Types,{0,n,0})
  end,
  N_Types = maps:put(Dest,{P,T,S},Types),
  {ok,N_Context} = set_data({P,T,S},Dest,Value,Context#context{types=N_Types}),
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{label,_}|Rest],Context,Ir) ->
  debug_print(Rest,Context),
  run_st(Rest,Context,Ir);

run_st([{jump,{l,Lb}}|_],Context,Ir) ->
  jump(Lb,Context,Ir);

run_st([{call,N_Fn,Arity}|Rest],Context,Ir) ->
  Types = Context#context.types,
  Args = maps:filter(fun ({R,_},_) -> R =:= z end, Types),
  Stack = Context#context.stack,
  N_Fp = ?STACK_PTR-byte_size(Stack),
  Fn_Context = Context#context{types=Args,fn=N_Fn,fp=N_Fp,s_reg=[]},
  {ok,Fn_End} = call_fn(N_Fn,Fn_Context,Ir),
  N_Types = maps:filter(fun ({R,_},_) -> R =/= z end,Types),
  Fn = Context#context.fn,
  Fp = Context#context.fp,
  S_Reg = Context#context.s_reg,
  N_Context = Fn_End#context{types=N_Types,fn=Fn,fp=Fp,s_reg=S_Reg},
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st([{test,Data,{l,Lb}}|Rest],Context,Ir) ->
  {ok,Data_Val} = get_data(Data,Context),
  if
    Data_Val == 0 -> jump(Lb,Context,Ir);
    true ->
      debug_print(Rest,Context),
      run_st(Rest,Context,Ir)
  end;

%% / is a special case as erlang treats float & integer division differently
run_st([{'/',[A,B],Dest}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {0,TA,SA} = maps:get(A,Types,{0,n,0}),
  {0,TB,SB} = maps:get(B,Types,{0,n,0}),
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  {ok,N_Context} = if
    (TA =:= f) or (TB =:= f) -> set_data({0,f,?SIZEOF_FLOAT},Dest,A_Val / B_Val,Context);
    true -> set_data({0,i,max(SA,SB)},Dest,A_Val div B_Val,Context)
  end,
  debug_print(Rest,Context),
  run_st(Rest,N_Context,Ir);

%% TODO: N/A
%        Add pointer arithmetic here (Probably?)
run_st([{Op,[A,B],Dest}|Rest],Context,Ir) ->
  Types = Context#context.types,
  {PA,TA,SA} = maps:get(A,Types,{0,n,0}),
  {PB,TB,SB} = maps:get(B,Types,{0,n,0}),
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  {ok,N_Context} = if
    (TA =:= f) or (TB =:= f) ->
      set_data({0,f,?SIZEOF_FLOAT},Dest,do_op(Op,A_Val,B_Val,PA,PB),Context);
    PA =/= 0 ->
      set_data({PA,i,SA},Dest,do_op(Op,A_Val,B_Val*SA,PA,PB),Context);
    PB =/= 0 ->
      set_data({PB,i,SB},Dest,do_op(Op,A_Val,B_Val*SB,PA,PB),Context);
    true ->
      set_data({0,i,max(SA,SB)},Dest,do_op(Op,A_Val,B_Val,PA,PB),Context)
  end,
  debug_print(Rest,N_Context),
  run_st(Rest,N_Context,Ir);

run_st(St,Context,_Ir) ->
  error({{unknown,St},{context,Context}}).


%% TODO: N/A
%        Floats need to be differentiated here
%        as they are currently stored as integers once being removed from the stack
get_data({i,N},_Context) -> {ok,N};
get_data({f,N},_Context) -> {ok,N};
get_data({x,N},Context) ->
  Reg = Context#context.reg,
  {ok,lists:nth(length(Reg) - N, Reg)};
get_data({z,N},Context) ->
    Args = Context#context.args,
    {ok,lists:nth(length(Args) - N, Args)};
get_data({y,N},Context) ->
  Types = Context#context.types,
  S_Reg = Context#context.s_reg,
  get_data(maps:get({y,N},Types,{0,n,0}),lists:nth(length(S_Reg)-N,S_Reg),Context);
get_data(n,_Context) ->
  {ok,n};
get_data(Data,_Context) ->
error({unknown,Data}).

get_data({0,f,Size},Address,Context) when is_integer(Address) ->
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address)*8,
  <<_:Offset,Data:Size/float,_/bits>> = Stack,
  {ok,Data};
get_data({0,_,Size},Address,Context) when is_integer(Address) ->
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address)*8,
  <<_:Offset,Data:Size,_/bits>> = Stack,
  {ok,Data};
get_data(_,Address,Context) when is_integer(Address) ->
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address)*8,
  <<_:Offset,Data:?SIZEOF_POINTER,_/bits>> = Stack,
  {ok,Data};
get_data(Type,Address,_Context) ->
  error({unknown,{type,Type},{address,Address}}).

set_data(Type,Dest,true,Context) -> set_data(Type,Dest,1,Context);
set_data(Type,Dest,false,Context) -> set_data(Type,Dest,0,Context);
set_data(Type,{x,N},Data,Context) ->
  Types = Context#context.types,
  Reg = Context#context.reg,
  N_Types = maps:put({x,N},Type,Types),
  N_Reg = set_reg(Context#context.reg,N,Data),
  {ok,Context#context{reg=N_Reg,types=N_Types}};
  set_data(_Type,{z,N},Data,Context) ->
    {ok,Context#context{args=set_reg(Context#context.args,N,Data)}};
set_data(Type,{y,N},Data,Context) ->
  Types = Context#context.types,
  S_Reg = Context#context.s_reg,
  % TODO: Update
  N_Types=Types,
  N_Context = Context#context{types=N_Types},
  set_data(Type,lists:nth(length(S_Reg)-N,S_Reg),Data,N_Context);

set_data({0,f,Size},Address,Data,Context) when is_integer(Address) ->
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address)*8,
  <<Init:Offset,_:Size,Rest/bits>> = Stack,
  N_Stack = <<Init:Offset,Data:Size/float,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
  {ok,N_Context};
set_data({P,_,Raw_Size},Address,Data,Context) when is_integer(Address) ->
  Size = if P =:= 0 -> Raw_Size; true -> ?SIZEOF_POINTER end,
  Stack = Context#context.stack,
  Fp = Context#context.fp,
  Offset = bit_size(Stack) - (?STACK_PTR - Address)*8,
  <<Init:Offset,_:Size,Rest/bits>> = Stack,
  N_Stack = <<Init:Offset,Data:Size,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
  {ok,N_Context};
set_data(_Type,Dest,_N,_Data) -> error({no_loc,Dest}).

set_reg([_|Reg],N,Data) when length(Reg) =:= N -> [Data | Reg];
set_reg(Reg,N,Data) when length(Reg) =:= N -> [Data | Reg];
set_reg([],0,Data) -> [Data];
set_reg([Hd|Reg],N,Data) -> [Hd|set_reg(Reg,N,Data)].

get_address({y,N},Context) ->
  S_Reg = Context#context.s_reg,
  Addr = lists:nth(length(S_Reg)-N, S_Reg),
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

cast(Data,{0,i,Size}) ->
  <<N_Data:Size>> = <<(trunc(Data)):Size>>,
  N_Data;
%% 32 bit floats are tricky so for now I'm going to just use 64 bit for everything
cast(Data,{0,f,Size}) ->
  <<N_Data:?SIZEOF_FLOAT/float>> = <<(float(Data)):?SIZEOF_FLOAT/float>>,
  N_Data;
cast(Data,{0,_,Size}) ->
    <<N_Data:Size>> = <<Data:Size>>,
    N_Data;
cast(Data,{_,_,_}) -> Data.


do_op('+',A,B,0,0) -> A+B;
do_op('+',A,B,_,0) -> A+B div 8;
do_op('+',A,B,0,_) -> B+A div 8;
do_op('-',A,B,0,0) -> A-B;
do_op('-',A,B,_,0) -> A-B div 8;
do_op('*',A,B,0,0) -> A*B;
do_op('%',A,B,0,0) -> A rem B;
do_op('==',A,B,0,0) -> A==B;
do_op('!=',A,B,0,0) -> A/=B;
do_op('>=',A,B,0,0) -> A>=B;
do_op('<=',A,B,0,0) -> B>=A;
do_op('>',A,B,0,0) -> A>B;
do_op('<',A,B,0,0) -> B>A;
do_op('&&',A,B,0,0) -> (A /= 0) and (B /= 0);
do_op('||',A,B,0,0) -> (A /= 0) or (B /= 0);
do_op(Op,_,_,0,0) -> error({bif_not_recognised,Op}).

get_literal_type(_Val) when is_float(_Val) -> {0,f,?SIZEOF_FLOAT};
get_literal_type(_Val) when is_integer(_Val) -> {0,i,?SIZEOF_FLOAT};
get_literal_type(Val) -> error({{expected,{'float|int'}},{got,Val}}).

debug_print([Hd|_],Context) when Context#context.debug ->
  io:fwrite("Reg: ~p~nFp:~p~nSReg:~p~nTypes: ~p~nStack: ~p~n~nNext St: ~p~n",
            [Context#context.reg,Context#context.fp,
             Context#context.s_reg,Context#context.types,
             Context#context.stack,Hd]);
debug_print(_,_) -> ok.
