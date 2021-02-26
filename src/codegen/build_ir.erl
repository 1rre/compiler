-module(build_ir).
-export([process/1]).

-record(state,{lbcnt=0,hpcnt=0,rvcnt=0,lvcnt=0,fn=#{},var=#{},typedef=#{},struct=#{},enum=#{}}).

process(Ast) ->
  process(Ast, #state{}).

process([], State) -> {ok, State, []};
process([Hd|Tl], State) ->
  {ok, Hd_State, Hd_St} = process(Hd,State),
  {ok, Tl_State, Tl_St} = process(Tl,Hd_State),
  {ok,Tl_State,Hd_St++Tl_St};

process({int_l,_Line,Val,_Suffix}, State) ->
  Lv_Cnt = State#state.lvcnt,
  {ok, State, [{move,{integer,Val},{x,Lv_Cnt}}]};

process({identifier,Ln,Ident}, State) ->
  case maps:get(Ident,State#state.var,undefined) of
    {_Type, X} ->
      Lv_Cnt = State#state.lvcnt,
      {ok,State,[{move,X,{x,Lv_Cnt}}]};
    Other -> error({Other,Ident,{line,Ln},{state,State}})
  end;

%TODO: Process function calls

process({{identifier,Ln,Ident},{apply,Args}}, State) ->
  {ok,Arg_State,Arg_St} = lists:foldl(fun
    (Arg,{ok,Acc_State,Acc_St}) ->
      {ok,Acc_Rtn_State,Acc_Rtn_St} = process(Arg,Acc_State),
      Lv_Cnt = Acc_State#state.lvcnt,
      {ok,copy_lbcnt(Acc_Rtn_State,Acc_State#state{lvcnt=Lv_Cnt+1}),Acc_St++Acc_Rtn_St}
  end, {ok,State,[]}, Args),
  case maps:get(Ident,Arg_State#state.fn, undefined) of
    {_Type, Arity} when Arity =:= length(Args) ->
      {ok,copy_lbcnt(Arg_State,State),Arg_St++[{call,Ident,Arity,{x,State#state.lvcnt}}]};
    Other ->
      error({Other, Ident, {args, Args}, {line, Ln}, {state, State}})
  end;

process({function,Fn_Specs}, State) ->
  get_fn_specs(Fn_Specs, State);

process({declaration,Raw_Type,Raw_St}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  get_decl_specs(Type,Raw_St,State);

process({assign,{Op,_Ln},Raw_Specs}, State) ->
  get_assign_specs(Op,Raw_Specs,State);

process({{return,_},Raw_St}, State) ->
  {ok, Rtn_State, Rtn_St} = process(Raw_St, State),
  {ok,Dealloc} = deallocate_mem(#{},Rtn_State#state.var),
  {ok, Rtn_State#state{lvcnt=0,rvcnt=0}, Rtn_St++[Dealloc,return]};

process({{'if',_},Predicate,True,False}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Lv_Cnt = State#state.lvcnt,
  {ok,If_State,If_St} = process(Predicate,State#state{lbcnt=Lb_Cnt+2}),
  Test_State = If_State#state{lvcnt=Lv_Cnt},
  Test_Jump = {test,{x,Lv_Cnt},{f,Lb_Cnt+1}},
  {ok,True_State,True_St} = process(True,Test_State),
  {ok,True_Dealloc} = deallocate_mem(Test_State#state.var,True_State#state.var),
  False_Label = {label,Lb_Cnt+1},
  {ok,False_State,False_St} = process(False,copy_lbcnt(True_State,Test_State)),
  {ok,False_Dealloc} = deallocate_mem(Test_State#state.var,False_State#state.var),
  Rtn_State = copy_lbcnt(False_State,State),
  if
    False_St =:= [] ->
      Rtn_St = If_St++[Test_Jump|True_St]++True_Dealloc++[False_Label],
      {ok,Rtn_State,Rtn_St};
    true ->
      True_Jump = {jump,{f,Lb_Cnt+2}},
      True_Label = {label,Lb_Cnt+2},
      Rtn_St = If_St++[Test_Jump|True_St]++True_Dealloc++[True_Jump,False_Label|False_St]++False_Dealloc++[True_Label],
      {ok,Rtn_State,Rtn_St}
  end;

process({{while,_},Predicate,Do}, State) ->
  Lb_Cnt = State#state.lbcnt + 1,
  Start_Label = {label,Lb_Cnt},
  {ok,Pred_State,Pred_St} = process(Predicate,State#state{lbcnt=Lb_Cnt+1}),
  Test_Jump = {test,{x,State#state.lvcnt},{f,Lb_Cnt+1}},
  {ok,Do_State,Do_St} = process(Do,copy_lvcnt(State,Pred_State)),
  Jump = {jump,{f,Lb_Cnt}},
  End_Label = {label,Lb_Cnt + 1},
  Rtn_State = copy_lbcnt(Do_State,State),
  Rtn_St = [Start_Label|Pred_St] ++ [Test_Jump|Do_St] ++ [Jump,End_Label],
  {ok,Rtn_State,Rtn_St};

process({{do,_},Do,Predicate}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Start_Label = {label,Lb_Cnt + 1},
  {ok,Do_State,Do_St} = process(Do,State#state{lbcnt=Lb_Cnt+2}),
  {ok,Pred_State,Pred_St} = process(Predicate,copy_lbcnt(Do_State,State)),
  Test_Jump = {test,{x,State#state.lvcnt},{f,Lb_Cnt+2}},
  Jump = {jump,{f,Lb_Cnt+1}},
  End_Label = {label,Lb_Cnt + 2},
  Rtn_State = copy_lbcnt(Pred_State,State),
  Rtn_St = [Start_Label|Do_St] ++ Pred_St ++ [Test_Jump,Jump,End_Label],
  {ok,Rtn_State,Rtn_St};

process({{for,_},{Init,Predicate,Update},Loop}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Lv_Cnt = State#state.lvcnt,
  {ok,Init_State,Init_St} = process(Init,State#state{lbcnt=Lb_Cnt+2}),
  Root_State = Init_State#state{lvcnt=Lv_Cnt},
  Pred_Label = {label,Lb_Cnt+1},
  {ok,Pred_State,Pred_St} = process(Predicate,Root_State),
  Pred_Test = {test,{x,Lv_Cnt},{f,Lb_Cnt+2}},
  {ok,Loop_State,Loop_St} = process(Loop,copy_lbcnt(Pred_State,Root_State)),
  {ok,Dealloc} = deallocate_mem(Init_State#state.var,Loop_State#state.var),
  {ok,Update_State,Update_St} = process(Update,copy_lbcnt(Loop_State,Root_State)),
  Jump = {jump,{f,Lb_Cnt+1}},
  End_Label = {label,Lb_Cnt+2},
  Next_State = copy_lbcnt(Update_State,State),
  Next_St = Init_St ++ [Pred_Label|Pred_St] ++ [Pred_Test|Loop_St] ++ [Dealloc|Update_St] ++ [Jump,End_Label],
  {ok,Next_State,Next_St};

process({bif,T,[A,B]}, State) ->
  Way_1 = process_bif(T,A,B,State),
  Way_2 = process_bif(T,B,A,State),
  case {(element(2,Way_1))#state.lvcnt,(element(2,Way_2))#state.lvcnt} of
    {_A,_B} when _A>_B -> Way_2;
    _ -> Way_1
  end;

process({{'&',Ln},Raw_St}, State) ->
  {ok, Ref_State, Ref_St} = process(Raw_St, State),
  case lists:last(Ref_St) of
    {move,Src,Dest} ->
      Next_St = lists:droplast(Ref_St) ++ [{address,Src,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    {get_heap,_Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    {address,_Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    Other -> error({address_error,Other,{line,Ln}})
    end;

process({{'*',Ln},Raw_St},State) ->
  {ok, Ptr_State, Ptr_St} = process(Raw_St, State),
  case lists:last(Ptr_St) of
    {move,Src,Dest} ->
      Next_St = lists:droplast(Ptr_St) ++ [{get_heap,Src,Dest}],
      {ok,copy_lvcnt(State,Ptr_State),Next_St};
    {get_heap,_Src,Dest} ->
      Next_St = Ptr_St ++ [{get_heap,Dest,Dest}],
      {ok,copy_lvcnt(State,Ptr_State),Next_St};
    {address,_Src,Dest} ->
      Next_St = Ptr_St ++ [{get_heap,Dest,Dest}],
      {ok,copy_lvcnt(State,Ptr_State),Next_St};
    Other -> error({Other,{line,Ln}})
  end;

process(Other, State) -> error({Other,State}).

% TODO: Add analysis of `Ident` (eg. returning pointer)
% TODO: Analysis of args for pass-by-reference?
get_fn_specs({Raw_Type,{Raw_Ident,Raw_Args},Raw_St}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  {ok, Ident, _Ptr_Ident} = get_ident_specs(Raw_Ident,State),
  {ok, Arg_State, _Arg_St} = process(Raw_Args, State),
  New_Fn = maps:put(Ident,{Type,length(Raw_Args)},Arg_State#state.fn),
  {ok, N_State, N_St} = process(Raw_St,Arg_State#state{fn=New_Fn}),
  Rtn_St = [{function,Type,Ident,length(Raw_Args),N_St}],
  {ok,copy_lvcnt(State,N_State),Rtn_St}.

get_type([{long,_},{long,_},{int,_}],_) -> {ok,int64};
get_type([{long,_},{int,_}],_) -> {ok,int64};
get_type([{int,_}],_) -> {ok,int32};
get_type([{void,_}],_) -> {ok,nil};
get_type(Type,_) -> {error,{unknown_type, Type}}.

get_decl_specs(Type, [{Raw_Ident,{'=',_},Raw_St}], State) when Raw_Ident =/= identifier ->
  {ok, Ident, Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Ptr_Ident, State),
  {ok, Decl_State, Decl_St} = process(Raw_St, Mem_State),
  Rv_Cnt = Decl_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Decl_State#state.var),
  Next_State = (copy_lvcnt(State,Decl_State))#state{var=New_Var,rvcnt=Rv_Cnt+1},
  Next_St = Mem_St ++ Decl_St ++ [{move,{x,State#state.lvcnt},{y,Rv_Cnt}}],
  {ok, Next_State, Next_St};

get_decl_specs(Type, [Raw_Ident], State) ->
  {ok, Ident, _Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Ident, State),
  Rv_Cnt = Mem_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Mem_State#state.var),
  Next_State = (copy_lvcnt(State,Mem_State))#state{var=New_Var,rvcnt=Rv_Cnt+1},
  {ok,Next_State,Mem_St};

get_decl_specs(Type, Other, State) -> error({Type,Other,State}).

% TODO: Add referencing
get_ident_specs({{'*',_},Rest}, State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs(Rest, State),
  {ok, Ident, {'*',Ptr_Ident}};
get_ident_specs({{{'*',_},Ptr},Rest}, State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs({Ptr,Rest}, State),
  {ok, Ident, {'*',Ptr_Ident}};
get_ident_specs({identifier,_,Ident}, _State) ->
  {ok, Ident, Ident}.

% Do we need to say what it's a pointer to?
allocate_mem(_Type,{'*',_Ident},State) ->
  {ok,State#state{hpcnt=State#state.hpcnt+1},[{allocate,sizeof(pointer,State)}]};
allocate_mem(Type,_Ident,State) ->
  {ok,State#state{hpcnt=State#state.hpcnt+1},[{allocate,sizeof(Type,State)}]}.

deallocate_mem(State_1,State_2) ->
  Dealloc_Mem = [maps:get(Key,State_2,undef) || Key <- maps:keys(State_2),
                                                maps:get(Key,State_1,undef) /= maps:get(Key,State_2,undef)],
  Dealloc = [sizeof(Type,State_2) || {Type,_} <- Dealloc_Mem],
  {ok, {deallocate,lists:sum(Dealloc)}}.

get_assign_specs('=',[Raw_Ident,Raw_St], State) ->
  {ok,Ident,Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Ptr} = case maps:get(Ident,State#state.var,undefined) of
    {_Type,Ptr_Loc} -> {ok,Ptr_Loc};
    Other -> {error, {Other,{undeclared,Ident}}}
  end,
  {ok,_Ptr_State,Ptr_St} = get_ptr(Ptr_Ident,Ptr,State#state{lvcnt=State#state.lvcnt+1}),
  {ok,Assign_State,Assign_St} = process(Raw_St,State),
  Lv_Cnt = State#state.lvcnt,
  case Ptr_St of
    [] ->
      Next_St = Assign_St++[{move,{x,Lv_Cnt},Ptr}],
      {ok,copy_lbcnt(Assign_State,State),Next_St};
    _ ->
      {_,Src,_} = lists:last(Assign_St),
      Next_St = Assign_St ++ Ptr_St ++ [{put_heap,Src,{x,Lv_Cnt + 1}}],
      {ok,copy_lbcnt(Assign_State,State),Next_St}
  end;

% This seems useless maybe?
get_assign_specs(Op,[Raw_Ident,Raw_St], State) ->
  {ok,_,_} = get_ident_specs(Raw_Ident, State),
  get_assign_specs('=',[Raw_Ident,{bif,Op,[Raw_Ident,Raw_St]}], State);

get_assign_specs(Op, Other, State) ->
  error({Op,Other,State}).

get_ptr({'*',Ptr_Ident}, Ptr, State) ->
  Next_St = {get_heap,Ptr,{x,State#state.lvcnt}},
  {ok,State,Ptr_St} = get_ptr(Ptr_Ident,{x,State#state.lvcnt},State),
  {ok,State,[Next_St|Ptr_St]};

get_ptr(_Ptr_Ident, _Ptr, State) ->
  {ok,State,[]}.

process_bif(Type,A,B,State) ->
  {ok,A_State,A_St} = process(A,State),
  N_A_State = A_State#state{lvcnt=State#state.lvcnt+1},
  {ok,B_State,B_St} = process(B,N_A_State),
  Lv_Cnt = State#state.lvcnt,
  Statement = A_St ++ B_St ++ [{Type,{x,Lv_Cnt},[{x,Lv_Cnt},{x,Lv_Cnt+1}]}],
  {ok,B_State,Statement}.

sizeof(int32,_) -> 32;
sizeof(pointer,_) -> 32;
sizeof(Type,State) -> error({type,Type}).

copy_lbcnt(State_1,State_2) -> State_2#state{lbcnt=State_1#state.lbcnt}.
copy_lvcnt(State_1,State_2) -> State_2#state{lvcnt=State_1#state.lvcnt}.
