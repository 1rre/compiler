-module(ir_vm).
-export([run/1,run/2,run/3]).

-define(STACK_PTR,16#7fffffff).
-define(GLOBL_PTR,16#10008000).

-record(context,{fn=main,global=[],addr_buf=[],reg=[],
                 stack= <<>>,s_bounds=[?STACK_PTR],heap= <<>>, h_bounds=[?GLOBL_PTR]}).

run(Ir) -> run(Ir,[]).
run(Ir,Args) ->
  case lists:search(fun ({function,_,_,_,_}) -> true; (_) -> false end, Ir) of
    {value, {function,_,Fn,_,_}} -> run(Ir,Fn,Args);
    _ -> error({no_fn,Ir})
  end.
run(Ir,Fn,Args) ->
  io:fwrite("~p~n~nRunning: ~s~n",[Ir,Fn]),
  {Init_Stack,Init_S_Bounds} = lists:foldl(fun
    (Arg,{Stack,[C1|Ch]}) ->
      {<<Arg:32,Stack/bits>>,[C1-4,C1|Ch]}
    end, {<<>>,[?STACK_PTR]}, Args),
  Init_Global = [{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir],
  Context = #context{fn=Fn,
                     global=Init_Global,
                     addr_buf=Init_S_Bounds,
                     stack=Init_Stack,
                     s_bounds=Init_S_Bounds},
  {ok, End_Context} = call_fn(Fn,Context,Ir),
  io:fwrite("End Stack:~n~p~n~nEnd Heap:~n~p~n",[End_Context#context.stack,End_Context#context.heap]),
  lists:last(End_Context#context.reg).

call_fn(Fn,Context,Ir) ->
  case lists:search(fun ({function,_Type,Fun,_Arity,_St}) -> Fun =:= Fn end, Ir) of
    {value, {function,_Type,Fn,_Arity,St}} ->
      Result = run_st(St,Context,Ir),
      Result;
    _ -> error({not_found, Fn})
  end.

run_st([return|_],Context,_Ir) ->
  {ok,Context};

run_st([{allocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  S_Bounds = Context#context.s_bounds,
  N_Stack = <<Stack/bits,0:N>>,
  N_S_Bounds = [hd(S_Bounds) - N div 8 | S_Bounds],
  Buf = Context#context.addr_buf,
  N_Buf = [hd(N_S_Bounds) | Buf],
  N_Context = Context#context{stack=N_Stack,addr_buf=N_Buf,s_bounds=N_S_Bounds},
  run_st(Rest,N_Context,Ir);

run_st([{deallocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  Size = bit_size(Stack) - N,
  S_Bounds = Context#context.s_bounds,
  N_S_Bounds = rm_chunks(S_Bounds,?STACK_PTR - Size div 8),
  <<N_Stack:Size/bits,_/bits>> = Stack,
  Buf = Context#context.addr_buf,
  N_Buf = lists:nthtail(length(S_Bounds)-length(N_S_Bounds),Buf),
  N_Context = Context#context{stack=N_Stack,addr_buf=N_Buf,s_bounds=N_S_Bounds},
  run_st(Rest,N_Context,Ir);

run_st([{address,Src,Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address(Src,Context),
  {ok,N_Context} = set_data(Dest,Address,Context),
  run_st(Rest,N_Context,Ir);

run_st([{load,Src,Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address(Src,Context),
  {ok,Value} = get_data(Address,Context),
  {ok,N_Context} = set_data(Dest,Value,Context),
  run_st(Rest,N_Context,Ir);

run_st([{store,Src,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Src,Context),
  {ok,Address} = get_data(Dest,Context),
  {ok,N_Context} = set_data(Address,Value,Context),
  run_st(Rest,N_Context,Ir);

run_st([{move,Data,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Data,Context),
  {ok,N_Context} = set_data(Dest,Value,Context),
  run_st(Rest,N_Context,Ir);

run_st([{label,_}|Rest],Context,Ir) -> run_st(Rest,Context,Ir);

run_st([{jump,{f,Lb}}|_],Context,Ir) -> jump(Lb,Context,Ir);

run_st([{call,Fn,Arity,{y,First}}|Rest],Context,Ir) ->
  Buf = Context#context.addr_buf,
  {Args,Other} = lists:split(length(Buf)-First,Buf),
  Fn_Context = Context#context{addr_buf=Args,fn=Fn},
  {ok,Fn_End} = call_fn(Fn,Fn_Context,Ir),
  N_Context = Fn_End#context{addr_buf=Other,fn=Context#context.fn},
  io:fwrite("~p~n",[N_Context#context.addr_buf]),
  run_st(Rest,N_Context,Ir);

run_st([{test,Data,{f,Lb}}|St],Context,Ir) ->
  {ok,Data_Val} = get_data(Data,Context),
  if
    Data_Val == 0 -> jump(Lb,Context,Ir);
    true -> run_st(St,Context,Ir)
  end;

% / is a special case as erlang treats float & integer division differently
run_st([{'/',Dest,[A,B]}|Rest],Context,Ir) ->
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  N_Context = if
    is_float(A_Val) or is_float(B_Val) -> set_data(Dest,A_Val / B_Val,Context);
    true -> set_data(Dest,A_Val div B_Val,Context)
  end,
  run_st(Rest,N_Context,Ir);

run_st([{Op,Dest,[A,B]}|Rest],Context,Ir) ->
  {ok,A_Val} = get_data(A, Context),
  {ok,B_Val} = get_data(B, Context),
  {ok,N_Context} = set_data(Dest,do_op(Op,A_Val,B_Val),Context),
  run_st(Rest,N_Context,Ir);

run_st(St,Context,_Ir) ->
  error({{unknown,St},{context,Context}}).

get_data({integer,N},_Context) -> {ok,N};
get_data({x,N},Context) ->
  Reg = Context#context.reg,
  {ok,lists:nth(length(Reg) - N, Reg)};
get_data({y,N},Context) ->
  Buf = Context#context.addr_buf,
  get_data(lists:nth(length(Buf)-N, Buf),Context);
get_data(nil,_Context) ->
  {ok,nil};
get_data(Address,Context) when Address >= 16#7000000 ->
  Size = get_mem_size(Address,Context#context.s_bounds)*8,
  Offset = (?STACK_PTR - Address)*8 - Size,
  <<_:Offset,Data:Size,_/bits>> = Context#context.stack,
  {ok,Data};
get_data(Address,Context) when is_integer(Address) ->
  Size = get_mem_size(Address,Context#context.s_bounds)*8,
  Offset = (Address-?GLOBL_PTR)*8 - Size,
  <<_:Offset,Data:Size,_/bits>> = Context#context.heap,
  {ok,Data};
get_data(Data,_Context) ->
  error({unknown,Data}).

set_data(Dest,true,Context) -> set_data(Dest,1,Context);
set_data(Dest,false,Context) -> set_data(Dest,0,Context);
set_data({x,N},Data,Context) ->
  {ok,Context#context{reg=set_data(Context#context.reg,N,Data)}};
set_data({y,N},Data,Context) ->
  Buf = Context#context.addr_buf,
  set_data(lists:nth(length(Buf)-N, Buf),Data,Context);
set_data(Address,Data,Context) when is_integer(Address) and (Address >= 16#7000000) ->
  Size = get_mem_size(Address,Context#context.s_bounds) * 8,
  Offset = (?STACK_PTR - Address)*8 - Size,
  <<Init:Offset,_:Size,Rest/bits>> = Context#context.stack,
  N_Stack = <<Init:Offset,Data:Size,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
  {ok,N_Context};
set_data(Address,Data,Context) when is_integer(Address) ->
  Size = get_mem_size(Address,Context#context.s_bounds) * 8,
  Offset = (Address - ?GLOBL_PTR)*8 - Size,
  <<Init:Offset,_:Size,Rest/bits>> = Context#context.heap,
  N_Heap = <<Init:Offset,Data:Size,Rest/bits>>,
  N_Context = Context#context{heap=N_Heap},
  {ok,N_Context};
set_data([_|Reg],N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data(Reg,N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data([],0,Data) -> [Data];
set_data([Hd|Reg],N,Data) -> [Hd|set_data(Reg,N,Data)];
set_data(Dest,_N,_Data) -> error({no_loc,Dest}).

get_address({y,N},Context) ->
  Buf = Context#context.addr_buf,
  Addr = lists:nth(length(Buf)-N, Buf),
  {ok, Addr};
get_address({x,N},Context) ->
  get_data({x,N},Context);
get_address(_,_) -> error("").

get_mem_size(C1,[C1,C2|_]) ->
  abs(C2-C1);
get_mem_size(C1,[C2,C3|S_Bounds]) when (C2 > C1) /= (C1 > C3) ->
  error({stack_boundary,{C1,[C2,C3|S_Bounds]}});
get_mem_size(C1,[_|S_Bounds]) ->
  get_mem_size(C1,S_Bounds).

jump(Lb,Context,Ir) ->
  Fn = Context#context.fn,
  case lists:search(fun ({function,_,Fun,_,_}) -> Fun =:= Fn end, Ir) of
    {value, {function,_,_,_,St}} ->
      run_st(find_lb(St,Lb),Context,Ir);
    _ -> error({not_found, Fn})
  end.

find_lb([{label,_Lb}|St],_Lb) -> St;
find_lb([_|St],Lb) -> find_lb(St,Lb);
find_lb(_,Lb) -> error({no_label,Lb}).

rm_chunks([Chunk|S_Bounds],Size) when Chunk =:= Size ->
  [Chunk|S_Bounds];
rm_chunks([C1,C2|S_Bounds],Size) when (C1>Size) and (C2 < Size) ->
  error(stack_boundary,{[C1,C2|S_Bounds],Size});
rm_chunks([_|S_Bounds],Size) ->
  rm_chunks(S_Bounds,Size).

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
do_op(Op,_,_) -> error({unknown,Op}).
