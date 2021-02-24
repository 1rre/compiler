-module(ir_vm).
-export([run/1,run/3]).

-record(context,{global=[],stack=[],heap=[],reg=[],fn=main}).

run(Ir) -> run(Ir,main,[]).
run(Ir,Fn,Args) ->
  io:fwrite("Running ~s:~n~p~n~n",[Fn,Ir]),
  Context = #context{global=[{Ident,{Type,Value}} || {global,Type,Ident,Value} <- Ir],fn=Fn,stack=Args},
  {ok, End_Context} = call_fn(Fn,Context,Ir),
  lists:last(End_Context#context.reg).

call_fn(Fn,Context,Ir) ->
  case lists:search(fun ({function,_Type,Fun,_Arity,_St}) -> Fun =:= Fn end, Ir) of
    {value, {function,_Type,Fn,_Arity,St}} ->
      Result = run_st(St,Context,Ir),
      Result;
    _ -> error({not_found, Fn})
  end.

run_st([return|_],Context,_Ir) -> {ok,Context};

run_st([{move,Data,Dest}|Rest],Context,Ir) ->
  {ok,Value} = get_data(Data,Context),
  {ok,N_Context} = set_data(Dest,Value,Context),
  run_st(Rest,N_Context,Ir);

run_st([{label,_}|Rest],Context,Ir) -> run_st(Rest,Context,Ir);

run_st([{jump,{f,Lb}}|_],Context,Ir) -> jump(Lb,Context,Ir);

run_st([{call,Fn,Arity,{x,First}}|Rest],Context,Ir) ->
  {Args,Other} = gc_list(First,Arity,Context#context.reg),
  Fn_Context = #context{global=Context#context.global,
                        stack=Args,
                        heap=Context#context.heap,
                        fn=Fn},
  {ok,Fn_End} = call_fn(Fn,Fn_Context,Ir),
  N_Context = Fn_End#context{stack=Context#context.stack,
                             fn=Context#context.fn,
                             reg=[lists:last(Fn_End#context.reg)|Other]},
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

run_st(St,_Context,_Ir) ->
  error({unknown,St}).

get_data({integer,N},_Context) -> {ok,N};
get_data({x,N},Context) ->
  Reg = Context#context.reg,
  {ok,lists:nth(length(Reg) - N, Reg)};
get_data({y,N},Context) ->
  Stack = Context#context.stack,
  {ok,lists:nth(length(Stack) - N, Stack)};
get_data(nil,_Context) ->
  {ok,nil};
get_data(Data,_Context) ->
  error({unknown,Data}).

set_data(Dest,true,Context) -> set_data(Dest,1,Context);
set_data(Dest,false,Context) -> set_data(Dest,0,Context);
set_data({x,N},Data,Context) ->
  {ok,Context#context{reg=set_data(Context#context.reg,N,Data)}};
set_data({y,N},Data,Context) ->
  {ok,Context#context{stack=set_data(Context#context.stack,N,Data)}};
set_data([_|Reg],N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data(Reg,N,Data) when length(Reg) =:= N -> [Data | Reg];
set_data([],0,Data) -> [Data];
set_data([Hd|Reg],N,Data) -> [Hd|set_data(Reg,N,Data)];
set_data(Dest,_N,_Data) -> error({no_loc,Dest}).


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
