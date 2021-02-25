-module(build_ir).
-export([process/1]).

-record(state,{lbcnt=0,hpcnt=0,rvcnt=0,lvcnt=0,fn=#{},var=#{},typedef=#{},struct=#{},enum=#{}}).

process(Ast) ->
  io:fwrite("Ast:~n~p~n~n",[Ast]),
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

process({function,Fn_Specs}, State) ->
  get_fn_specs(Fn_Specs, State);

process({declaration,Raw_Type,Raw_St}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  get_decl_specs(Type,Raw_St,State);

process({assign,{Op,Ln},Raw_Specs}, State) ->
  get_assign_specs(Op,Raw_Specs,State);

process({{return,_},Raw_St}, State) ->
  {ok, Rtn_State, Rtn_St} = process(Raw_St, State),
  {ok, Rtn_State#state{lvcnt=0,rvcnt=0}, Rtn_St++[return]};

process({{'if',_},Predicate,True,False}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Lv_Cnt = State#state.lvcnt,
  {ok,If_State,If_St} = process(Predicate,State#state{lbcnt=Lb_Cnt+2}),
  Test_State = If_State#state{lvcnt=Lv_Cnt},
  Test_Jump = {test,{x,Lv_Cnt},{f,Lb_Cnt+1}},
  {ok,True_State,True_St} = process(True,Test_State),
  False_Label = {label,Lb_Cnt+1},
  {ok,False_State,False_St} = process(False,Test_State#state{lbcnt=True_State#state.lbcnt}),
  if
    False_St =:= [] ->
      Rtn_State = Test_State#state{lbcnt=False_State#state.lbcnt},
      Rtn_St = If_St++[Test_Jump|True_St]++[False_Label],
      {ok,Rtn_State,Rtn_St};
    true ->
      True_Jump = {jump,{f,Lb_Cnt+2}},
      True_Label = {label,Lb_Cnt+2},
      Rtn_State = Test_State#state{lbcnt=False_State#state.lbcnt},
      Rtn_St = If_St++[Test_Jump|True_St]++[True_Jump,False_Label|False_St]++[True_Label],
      {ok,Rtn_State,Rtn_St}
  end;

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
      {ok,Ref_State#state{lvcnt=State#state.lvcnt},Next_St};
    {get_heap,Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,Ref_State#state{lvcnt=State#state.lvcnt},Next_St};
    {address,Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,Ref_State#state{lvcnt=State#state.lvcnt},Next_St};
    Other -> error({address_error,Other})
    end;

process({{'*',Ln},Raw_St},State) ->
  {ok, Ptr_State, Ptr_St} = process(Raw_St, State),
  case lists:last(Ptr_St) of
    {move,Src,Dest} ->
      Next_St = lists:droplast(Ptr_St) ++ [{get_heap,Src,Dest}],
      {ok,Ptr_State#state{lvcnt=State#state.lvcnt},Next_St};
    {get_heap,Src,Dest} ->
      Next_St = Ptr_St ++ [{get_heap,Dest,Dest}],
      {ok,Ptr_State#state{lvcnt=State#state.lvcnt},Next_St};
    {address,Src,Dest} ->
      Next_St = Ptr_St ++ [{get_heap,Dest,Dest}],
      {ok,Ptr_State#state{lvcnt=State#state.lvcnt},Next_St}
  end;

process(Other, State) -> error(Other).

% TODO: Add analysis of `Ident`
get_fn_specs({Raw_Type,{Raw_Ident,Raw_Args},Raw_St}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  {ok, Ident, Ptr_Ident} = get_ident_specs(Raw_Ident,State),
  {ok, Arg_State, Arg_St} = process(Raw_Args, State),
  New_Fn = maps:put(Ident,{Type,length(Raw_Args)},Arg_State#state.fn),
  {ok, N_State, N_St} = process(Raw_St,Arg_State#state{fn=New_Fn}),
  Rtn_St = [{function,Type,Ident,length(Raw_Args),N_St}],
  {ok,N_State#state{lvcnt=State#state.lvcnt},Rtn_St}.

get_type([{long,_},{long,_},{int,_}],_) -> {ok,int64};
get_type([{long,_},{int,_}],_) -> {ok,int64};
get_type([{int,_}],_) -> {ok,int32};
get_type([{void,_}],_) -> {ok,nil};
get_type(Type,_) -> {error,{unknown_type, Type}}.

get_decl_specs(Type, [{Raw_Ident,Op,Raw_St}], State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Ptr_Ident, State),
  {ok, Decl_State, Decl_St} = process(Raw_St, Mem_State),
  Rv_Cnt = Decl_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Decl_State#state.var),
  Next_State = Decl_State#state{var=New_Var,rvcnt=Rv_Cnt+1,lvcnt=State#state.lvcnt},
  Next_St = Mem_St ++ Decl_St ++ [{move,{x,State#state.lvcnt},{y,Rv_Cnt}}],
  {ok, Next_State, Next_St};

get_decl_specs(Type, [Raw_Ident], State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Ident, State),
  Rv_Cnt = Mem_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Mem_State#state.var),
  Next_State = Mem_State#state{var=New_Var,rvcnt=Rv_Cnt+1,lvcnt=State#state.lvcnt},
  {ok,Next_State,Mem_St};

get_decl_specs(Type, Other, State) -> error(Other).

get_ident_specs({{'*',_},Rest}, State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs(Rest, State),
  {ok, Ident, {'*',Ptr_Ident}};
get_ident_specs({{{'*',_},Ptr},Rest}, State) ->
  {ok, Ident, Ptr_Ident} = get_ident_specs({Ptr,Rest}, State),
  {ok, Ident, {'*',Ptr_Ident}};
get_ident_specs({identifier,_,Ident}, State) ->
  {ok, Ident, Ident}.

get_ident({_,Next}) -> get_ident(Next);
get_ident(Ident) -> {ok,Ident}.

allocate_mem(_Type,{'*',Ident},State) ->
  {ok,State#state{hpcnt=State#state.hpcnt+1},[{allocate,pointer}]};
allocate_mem(Type,Ident,State) ->
  {ok,State#state{hpcnt=State#state.hpcnt+1},[{allocate,Type}]}.

% TODO: Make this work on pointers
get_assign_specs('=',[Raw_Ident,Raw_St], State) ->
  {ok,Ident,Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  {ok, Ptr} = case maps:get(Ident,State#state.var,undefined) of
    {_Type,Ptr_Loc} -> {ok,Ptr_Loc};
    Other -> {error, {undeclared,Ident}}
  end,
  {ok,Ptr_State,Ptr_St} = get_ptr(Ptr_Ident,Ptr,State),
  {ok,Assign_State,Assign_St} = process(Raw_St,State),
  Next_State = Assign_State#state{lvcnt=State#state.lvcnt},
  Lv_Cnt = State#state.lvcnt,
  case Ptr_St of
    [] ->
      Next_St = Assign_St++[{move,{x,Lv_Cnt},Ptr}],
      {ok,Next_State#state{lvcnt=State#state.lvcnt},Next_St};
    _ ->
      {get_heap,Src,Dest} = lists:last(Ptr_St),
      Next_St = Ptr_St ++ Assign_St ++ [{put_heap,{x,Lv_Cnt},Src}],
      {ok,Next_State#state{lvcnt=State#state.lvcnt},Next_St}
  end;

get_assign_specs(Op,[Raw_Ident,Raw_St], State) ->
  {ok,_,Ptr_Ident} = get_ident_specs(Raw_Ident, State),
  get_assign_specs('=',[Raw_Ident,{bif,Op,[Raw_Ident,Raw_St]}], State);

get_assign_specs(_Op, Other, State) ->
  error(Other).

get_ptr({'*',Ptr_Ident}, Ptr, State) ->
  Next_St = {get_heap,Ptr,{x,State#state.lvcnt}},
  {ok,State,Ptr_St} = get_ptr(Ptr_Ident,{x,State#state.lvcnt},State),
  {ok,State,[Next_St|Ptr_St]};

get_ptr(Ptr_Ident, Ptr, State) ->
  {ok,State,[]}.

process_bif(Type,A,B,State) ->
  {ok,A_State,A_St} = process(A,State),
  N_A_State = A_State#state{lvcnt=State#state.lvcnt+1},
  {ok,B_State,B_St} = process(B,N_A_State),
  Lv_Cnt = State#state.lvcnt,
  Statement = A_St ++ B_St ++ [{Type,{x,Lv_Cnt},[{x,Lv_Cnt},{x,Lv_Cnt+1}]}],
  {ok,B_State,Statement}.
