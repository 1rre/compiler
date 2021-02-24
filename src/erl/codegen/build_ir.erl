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
    Decl_St =:= [] -> {ok,Next_Context,[]};
    true -> {ok,Next_Context,Decl_St ++ [{move,{x,Lv_Cnt},{y,Rv_Cnt}}]}
  end;

process({assign, {'=',Ln}, Raw_Specs}, Context) ->
  {ok,Ident,Raw_Decl_St} = get_decl_specs(Raw_Specs),
  {ok,Decl_Context,Decl_St} = process(Raw_Decl_St,Context),
  Lv_Cnt = Context#context.lvcnt,
  Next_Context = Decl_Context#context{lvcnt=Lv_Cnt},
  case maps:get(Ident, Next_Context#context.var, undefined) of
    {_Type, Mem_Loc} -> {ok, Context, Decl_St ++ [{move,{x,Lv_Cnt},Mem_Loc}]};
    undefined -> error({undeclared,Ident,{line,Ln}})
  end;

process({int_l,_,Val,_Suffix}, Context) ->
  Lv_Cnt = Context#context.lvcnt,
  Next_Context = Context#context{lvcnt = Lv_Cnt + 1},
  {ok,Next_Context,[{move,{integer,Val},{x,Lv_Cnt}}]};

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
