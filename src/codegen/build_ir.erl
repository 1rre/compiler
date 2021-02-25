-module(build_ir).
-export([process/1]).

-record(context,{lbcnt=0,lvcnt=0,rvcnt=0,fn=#{},var=#{},typedef=#{}}).

process(Ast) ->
  process(Ast, #context{}).
process([], Context) -> {ok,Context,[]};
process([St|Ast], Context) ->
  {ok,N_Context,St_List} = process(St,Context),
  {ok,N2_Context,N_St_List} = process(Ast,N_Context),
  {ok,N2_Context,St_List++N_St_List};

%% Functions
process({function,{Return_Type,{{identifier,_,Ident},Args},Statement}},Context) ->
  Type = check_typedef(Return_Type, Context),
  {ok, Temp_Context, _} = process(Args,Context),
  Arg_Context = Temp_Context#context{lvcnt=Context#context.lvcnt},
  Next_Fn = maps:put(Ident,{Type,length(Args)},Arg_Context#context.fn),
  {ok, N_Context, N_St} = process(Statement, Arg_Context#context{fn=Next_Fn}),
  Rtn_St = [{function, Type, Ident, length(Args),N_St}],
  Rtn_Context = Context#context{lbcnt=N_Context#context.lbcnt,fn=Next_Fn},
  {ok, Rtn_Context, Rtn_St};

% TODO: Implement any other fn types
process({function, Fn_Spec}, _Context) ->
  error({unknown_fn_spec,Fn_Spec});

process({int_l,_,Val,_Suffix}, Context) ->
  Lv_Cnt = Context#context.lvcnt,
  Next_Context = Context#context{lvcnt = Lv_Cnt + 1},
  {ok,Next_Context,[{move,{integer,Val},{x,Lv_Cnt}}]};

%% Identifiers
process({identifier,Ln,Ident}, Context) ->
  case maps:get(Ident,Context#context.var,undefined) of
    {_Type, X} ->
      Lv_Cnt = Context#context.lvcnt,
      {ok,Context#context{lvcnt=Lv_Cnt+1},[{move,X,{x,Lv_Cnt}}]};
    Other -> error({Other, Ident, {line, Ln}, {context, Context}})
  end;

process({{identifier,Ln,Ident},{apply,Args}}, Context) ->
  {ok, _, Temp_St} = process(Args,Context),
  Arg_St = lists:flatten(Temp_St),
  case maps:get(Ident,Context#context.fn) of
    {_Type, Len} when Len =:= length(Args) ->
      {ok, Context, Arg_St ++ [{call,Ident,Len,{x,Context#context.lvcnt}}]};
    Other ->
      error({Other, Ident, {args, Args}, {line, Ln}, {context, Context}})
  end;

%% Return
process({{return,_},Statement}, Context) ->
  {ok,_N_Context,N_St} = process(Statement, Context),
  {ok,Context,N_St++[return]};

%% Variable Updates
process({declaration, Raw_Type, Raw_Specs}, Context) ->
  {ok,Ident,Raw_Decl_St} = get_decl_specs(Raw_Specs),
  {ok,Decl_Context,Decl_St} = process(Raw_Decl_St,Context),
  Type = check_typedef(Raw_Type, Context),
  Rv_Cnt = Decl_Context#context.rvcnt,
  Next_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Decl_Context#context.var),
  Lv_Cnt = Context#context.lvcnt,
  Next_Context = Decl_Context#context{rvcnt=Rv_Cnt+1, var=Next_Var, lvcnt=Lv_Cnt},
  if
    Decl_St =:= [] -> {ok,Next_Context,[{move,nil,{y,Rv_Cnt}}]};
    true -> {ok,Next_Context,Decl_St ++ [{move,{x,Lv_Cnt},{y,Rv_Cnt}}]}
  end;

process({assign, {'=',Ln}, Raw_Specs}, Context) ->
  {ok,Ident,Raw_Decl_St} = get_decl_specs(Raw_Specs),
  {ok,Decl_Context,Decl_St} = process(Raw_Decl_St,Context),
  Lv_Cnt = Context#context.lvcnt,
  Next_Context = Decl_Context#context{lvcnt=Lv_Cnt},
  case maps:get(Ident, Next_Context#context.var, undefined) of
    {_Type, Mem_Loc} -> {ok, Context, Decl_St ++ [{move,{x,Lv_Cnt},Mem_Loc}]};
    undefined -> error({undeclared,Ident,{line,Ln},{context,Context}})
  end;

process({assign, {Op,Ln}, Raw_Specs}, Context) ->
  {ok,Ident,Raw_Decl_St} = get_decl_specs(Raw_Specs),
  Raw_Ident = {identifier,Ln,Ident},
  Bif = [{bif,Op,[Raw_Ident,Raw_Decl_St]}],
  process([{assign,{'=',Ln},[Raw_Ident,Bif]}], Context);

%% Jump Statements
process({{'if',_},Test,True,False}, Context) ->
  Lb_Cnt = Context#context.lbcnt,
  {ok,If_Context,If_St} = process(Test, Context#context{lbcnt=Lb_Cnt+2}),
  Test_Eq = {test,{x,Context#context.lvcnt},{f,Lb_Cnt+1}},
  {ok,T_Context,T_St} = process(True,Context#context{lbcnt=If_Context#context.lbcnt}),
  Start_Label = {label,Lb_Cnt+1},
  {ok,F_Context,F_St} = process(False, T_Context),
  if
    F_St =:= [] ->
      {ok,F_Context,If_St++[Test_Eq|T_St]++[Start_Label]};
    true ->
      Jump = {jump,{f,Lb_Cnt+1}},
      End_Label = {label,Lb_Cnt+2},
      {ok,F_Context,If_St++[Test_Eq|T_St]++[Jump,Start_Label|F_St]++[End_Label]}
  end;

process({{'while',_},Test,Do}, Context) ->
  Lb_Cnt = Context#context.lbcnt,
  Start_Label = {label,Lb_Cnt+1},
  {ok,Pred_Context,Pred_St} = process(Test,Context#context{lbcnt=Lb_Cnt+2}),
  Test_St = {test,{x,Context#context.lvcnt},{f,Lb_Cnt+2}},
  {ok, Do_Context, Do_St} = process(Do, Context#context{lbcnt=Pred_Context#context.lbcnt}),
  Jump = {jump,{f,Lb_Cnt+1}},
  End_Label = {label,Lb_Cnt+2},
  Next_Context = Context#context{lbcnt=Do_Context#context.lbcnt},
  Next_St = [Start_Label|Pred_St++[Test_St|Do_St++[Jump,End_Label]]],
  {ok,Next_Context,Next_St};

process({{'do',_},Do,Test}, Context) ->
  Lb_Cnt = Context#context.lbcnt,
  Label = {label,Lb_Cnt+1},
  {ok,Do_Context,Do_St} = process(Do,Context#context{lbcnt=Lb_Cnt+1}),
  {ok,T_Context,Test_St} = process(Test,Context#context{lbcnt=Do_Context#context.lbcnt}),
  Jump = {test,{x,Context#context.lvcnt},{f,Lb_Cnt+1}},
  Next_Context = Context#context{lbcnt=T_Context#context.lbcnt},
  Next_St = [Label|Do_St++Test_St++[Jump]],
  {ok,Next_Context,Next_St};

process({{for,_},{First,Pred,St},Loop},Context) ->
  Lb_Cnt = Context#context.lbcnt,
  Lv_Cnt = Context#context.lvcnt,
  {ok,F_Context,F_St} = process(First,Context#context{lbcnt=Lb_Cnt+3}),
  P_Label = {label,Lb_Cnt+1},
  {ok,P_Context,P_St} = process(Pred,F_Context#context{lvcnt=Lv_Cnt}),
  P_Test = {test,{x,F_Context#context.lvcnt},{f,Lb_Cnt+2}},
  {ok,L_Context,L_St} = process(Loop,P_Context#context{lvcnt=Lv_Cnt}),
  N_Lv_Cnt = F_Context#context.lvcnt,
  N_Lb_Cnt = L_Context#context.lbcnt,
  {ok,N_Context,N_St} = process(St,F_Context#context{lbcnt=N_Lb_Cnt,lvcnt=N_Lv_Cnt}),
  Jump = {jump, {f,Lb_Cnt+1}},
  E_Label = {label,Lb_Cnt+2},
  Next_Context = Context#context{lbcnt=N_Context#context.lbcnt},
  Next_St = F_St++[P_Label|P_St++[P_Test|L_St++N_St++[Jump,E_Label]]],
  {ok,Next_Context,Next_St};

%% Built-in Functions
process({bif,T,[A,B]}, Context) ->
  Way_1 = process_bif(T,A,B,Context),
  Way_2 = process_bif(T,B,A,Context),
  case {(element(2,Way_1))#context.lvcnt,(element(2,Way_2))#context.lvcnt} of
    {_A,_B} when _A>_B -> Way_2;
    _ -> Way_1
  end;

%% Base Case
process(Part, Context) ->
  io:fwrite("Part:~n~p~n~n",[Part]),
  {ok,Context,[]}.

check_typedef(Type, Context) ->
  case maps:get(Type,Context#context.typedef,undefined) of
    undefined -> get_type(Type);
    Typedef -> get_type(Typedef)
  end.

get_type([{long,_},{long,_},{int,_}]) -> int64;
get_type([{long,_},{int,_}]) -> int64;
get_type([{int,_}]) -> int32;
get_type(Type) -> error({unknown_type, Type}).

get_decl_specs([{identifier,_,Ident}]) ->
  {ok, Ident, []};
get_decl_specs([{{identifier,_,Ident},{'=',_},St}]) ->
  {ok, Ident, St};
get_decl_specs([{identifier,_,Ident},St]) ->
  {ok, Ident, St};
get_decl_specs(Unkn) ->
  io:fwrite("Unknown, ~p", [Unkn]),
  {ok,nil,[]}.

process_bif(Type,A,B,Context) ->
  {ok,A_Context,A_St} = process(A,Context),
  N_A_Context = A_Context#context{lvcnt=Context#context.lvcnt+1},
  {ok,B_Context,B_St} = process(B,N_A_Context),
  Lv_Cnt = Context#context.lvcnt,
  Statement = A_St ++ B_St ++ [{Type,{x,Lv_Cnt},[{x,Lv_Cnt},{x,Lv_Cnt+1}]}],
  {ok,B_Context,Statement}.
