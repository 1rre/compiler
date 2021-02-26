-module(ir_vm).
-export([run/1,run/2,run/3]).

-define(GLOBL_PTR,16#10008000).
-define(STACK_PTR,16#7FFFFFFF).

-record(context,{reg,addr_buf,global,stack,s_bounds,heap,h_bounds,fn}).

run(Ir) -> run(Ir,[]).
run(Ir,Args) ->
  case lists:search(fun ({function,_,_,_,_}) -> true; (_) -> false end, Ir) of
    {value, {function,_,Fn,_,_}} -> run(Ir,Fn,Args);
    _ -> error({no_fn,Ir})
  end.
run(Ir,Fn,Args) ->
  {Init_Stack,Init_S_Bounds} = lists:foldl(fun
    (Arg,{Stack,[C1|Ch]}) ->
      {<<Arg:32,Stack/bits>>,[C1+32,C1|Ch]}
    end, {<<>>,[0]}, Args),
  Init_Global = [{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir],
  Context = #context{reg = [],                 % List (runtime -> registers & memory)
                     addr_buf = Init_S_Bounds, % List (compile-time only)
                     global = Init_Global,     % List (runtime -> labels)
                     stack = Init_Stack,       % Bits (runtime -> memory)
                     s_bounds = Init_S_Bounds, % List (compile-time only)
                     heap = <<>>,              % Bits (runtime -> memory)
                     h_bounds = [0],           % List (compile-time only)
                     fn = Fn},                 % Atom (debugging only)
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
  N_Stack = <<Stack/bits,0:N>>,
  S_Bounds = Context#context.s_bounds,
  N_S_Bounds = [N+bit_size(Stack)|S_Bounds],
  N_Buf = [bit_size(Stack)|Context#context.addr_buf],
  N_Context = Context#context{stack=N_Stack,addr_buf=N_Buf,s_bounds=N_S_Bounds},
  run_st(Rest,N_Context,Ir);

run_st([{deallocate,N}|Rest],Context,Ir) ->
  Stack = Context#context.stack,
  Size = bit_size(Stack) - N,
  S_Bounds = Context#context.s_bounds,
  N_S_Bounds = rm_chunks(S_Bounds,Size),
  <<N_Stack:Size/bits,_/bits>> = Stack,
  Buf = Context#context.addr_buf,
  N_Buf = lists:nthtail(length(S_Bounds)-length(N_S_Bounds),Buf),
  N_Context = Context#context{stack=N_Stack,addr_buf=N_Buf,s_bounds=N_S_Bounds},
  run_st(Rest,N_Context,Ir);

run_st([{heap,Data_Size,Src,Dest}|Rest],Context,Ir) ->
  Heap = Context#context.heap,
  N = get_data(Src,Context),
  Size = Data_Size * N,
  N_Heap = <<Heap/bits,0:Size>>,
  New_Mem = [bit_size(Heap) + X * Data_Size || X <- lists:seq(N,1,-1)],
  H_Bounds = Context#context.h_bounds,
  N_H_Bounds = New_Mem ++ H_Bounds,
  N_Context = Context#context{heap=N_Heap,h_bounds=N_H_Bounds},
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
  %io:fwrite("Calling: ~s with~nargs: ~p~nstack: ~p~n",[Fn,Args,Context#context.stack]),
  Fn_Context = Context#context{addr_buf=Args,fn=Fn},
  {ok,Fn_End} = call_fn(Fn,Fn_Context,Ir),
  N_Context = Fn_End#context{addr_buf=Other,fn=Context#context.fn},
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
get_data(Address,Context) when is_integer(Address) ->
  Size = get_stack_size(Address,Context#context.s_bounds),
  <<_:Address,Data:Size,_/bits>> = Context#context.stack,
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
set_data(Address,Data,Context) when is_integer(Address) ->
  Size = get_stack_size(Address,Context#context.s_bounds),
  <<Init:Address,_:Size,Rest/bits>> = Context#context.stack,
  N_Stack = <<Init:Address,Data:Size,Rest/bits>>,
  N_Context = Context#context{stack=N_Stack},
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

get_stack_size(C1,[C2,C1|_]) ->
  C2-C1;
get_stack_size(C1,[C2,C3|S_Bounds]) when (C2 > C1) and (C1 > C3) ->
  error({stack_boundary,{C1,[C2,C3|S_Bounds]}});
get_stack_size(C1,[_|S_Bounds]) ->
  get_stack_size(C1,S_Bounds).

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

rm_chunks([Chunk|Bounds],Size) when Chunk =:= Size ->
  [Chunk|Bounds];
rm_chunks([C1,C2|Bounds],Size) when (C1>Size) and (C2 < Size) ->
  error({boundary_violation,{request,Size},{bounds,{C2,C1}}});
rm_chunks([_|Bounds],Size) ->
  rm_chunks(Bounds,Size).

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
