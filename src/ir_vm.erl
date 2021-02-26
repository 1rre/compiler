-module(ir_vm).
-export([run/1,run/2,run/3]).

-record(context,{global = [], stack = [], reg = [], heap = <<>>, fn = main, chunks = [0]}).

run(Ir) -> run(Ir,[]).
run(Ir,Args) ->
  case lists:search(fun ({function,_,_,_,_}) -> true; (_) -> false end, Ir) of
    {value, {function,_,Fn,_,_}} -> run(Ir,Fn,Args);
    _ -> error({no_fn,Ir})
  end.
run(Ir,Fn,Args) ->
  {Init_Heap,Init_Chunks} = lists:foldl(fun
    (Arg,{Heap,[C1|Ch]}) ->
      {<<Arg:32,Heap/bits>>,[C1+32,C1|Ch]}
    end, {<<>>,[0]}, Args),
  Init_Stack = [32*N||N<-lists:seq(length(Args)-1,0,-1)],
  Init_Global = [{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir],
  Context = #context{global=Init_Global,fn=Fn,stack=Init_Stack,heap=Init_Heap,chunks=Init_Chunks},
  {ok, End_Context} = call_fn(Fn,Context,Ir),
  io:fwrite("End Heap:~n~p~n",[End_Context#context.heap]),
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
  Heap = Context#context.heap,
  Chunks = Context#context.chunks,
  N_Heap = <<Heap/bits,0:N>>,
  N_Chunks = [N+bit_size(Heap)|Chunks],
  N_Stack = [bit_size(Heap)|Context#context.stack],
  N_Context = Context#context{heap=N_Heap,stack=N_Stack,chunks=N_Chunks},
  run_st(Rest,N_Context,Ir);

run_st([{deallocate,N}|Rest],Context,Ir) ->
  Heap = Context#context.heap,
  Size = bit_size(Heap) - N,
  Chunks = Context#context.chunks,
  N_Chunks = rm_chunks(Chunks,Size),
  <<N_Heap:Size/bits,_/bits>> = Heap,
  Stack = Context#context.stack,
  N_Stack = lists:nthtail(length(Chunks)-length(N_Chunks),Stack),
  N_Context = Context#context{heap=N_Heap,stack=N_Stack,chunks=N_Chunks},
  run_st(Rest,N_Context,Ir);

run_st([{address,Src,Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address(Src,Context),
  {ok,N_Context} = set_data(Dest,Address,Context),
  run_st(Rest,N_Context,Ir);

run_st([{get_heap,Src,Dest}|Rest],Context,Ir) ->
  {ok,Address} = get_address(Src,Context),
  {ok,Value} = get_data(Address,Context),
  {ok,N_Context} = set_data(Dest,Value,Context),
  run_st(Rest,N_Context,Ir);

run_st([{put_heap,Src,Dest}|Rest],Context,Ir) ->
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
  Stack = Context#context.stack,
  {Args,Other} = lists:split(length(Stack)-First,Stack),
  %io:fwrite("Calling: ~s with~nargs: ~p~nheap: ~p~n",[Fn,Args,Context#context.heap]),
  Fn_Context = Context#context{stack=Args,fn=Fn},
  {ok,Fn_End} = call_fn(Fn,Fn_Context,Ir),
  N_Context = Fn_End#context{stack=Other,fn=Context#context.fn},
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
  Stack = Context#context.stack,
  get_data(lists:nth(length(Stack)-N, Stack),Context);
get_data(nil,_Context) ->
  {ok,nil};
get_data(Address,Context) when is_integer(Address) ->
  Size = get_heap_size(Address,Context#context.chunks),
  <<_:Address,Data:Size,_/bits>> = Context#context.heap,
  {ok,Data};
get_data(Data,_Context) ->
  error({unknown,Data}).

set_data(Dest,true,Context) -> set_data(Dest,1,Context);
set_data(Dest,false,Context) -> set_data(Dest,0,Context);
set_data({x,N},Data,Context) ->
  {ok,Context#context{reg=set_data(Context#context.reg,N,Data)}};
set_data({y,N},Data,Context) ->
  Stack = Context#context.stack,
  set_data(lists:nth(length(Stack)-N, Stack),Data,Context);
set_data(Address,Data,Context) when is_integer(Address) ->
  Size = get_heap_size(Address,Context#context.chunks),
  <<Init:Address,_:Size,Rest/bits>> = Context#context.heap,
  N_Heap = <<Init:Address,Data:Size,Rest/bits>>,
  N_Context = Context#context{heap=N_Heap},
  {ok,N_Context};
set_data([_|Reg],N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data(Reg,N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data([],0,Data) -> [Data];
set_data([Hd|Reg],N,Data) -> [Hd|set_data(Reg,N,Data)];
set_data(Dest,_N,_Data) -> error({no_loc,Dest}).

get_address({y,N},Context) ->
  Stack = Context#context.stack,
  Addr = lists:nth(length(Stack)-N, Stack),
  {ok, Addr};
get_address({x,N},Context) ->
  get_data({x,N},Context);
get_address(_,_) -> error("").

get_heap_size(C1,[C2,C1|_]) ->
  C2-C1;
get_heap_size(C1,[C2,C3|Chunks]) when (C2 > C1) and (C1 > C3) ->
  error({heap_boundary,{C1,[C2,C3|Chunks]}});
get_heap_size(C1,[_|Chunks]) ->
  get_heap_size(C1,Chunks).

gc_list(_,_,[]) -> {[],[]};
gc_list(First,Arity,[_|Reg]) when length(Reg) >= First + Arity -> gc_list(First,Arity,Reg);
gc_list(First,Arity,[Hd|Reg]) when length(Reg) >= First ->
  {Args, Other} = gc_list(First,Arity,Reg),
  {[Hd|Args],Other};
gc_list(First,Arity,[Hd|Reg]) ->
  {_,Tl} = gc_list(First,Arity,Reg),
  {[],[Hd|Tl]}.

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

rm_chunks([Chunk|Chunks],Size) when Chunk =:= Size ->
  [Chunk|Chunks];
rm_chunks([C1,C2|Chunks],Size) when (C1>Size) and (C2 < Size) ->
  error(heap_boundary,{[C1,C2|Chunks],Size});
rm_chunks([_|Chunks],Size) ->
  rm_chunks(Chunks,Size).

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
