-module(var_rename).
-export([process/1]).

-define(LVARS, proplists:get_value(lvars, Context)).
-define(MVARS, proplists:get_value(lvars, Context)).
-define(LABELS, proplists:get_value(lvars, Context)).

process(Ast) ->
  io:fwrite("Ast:~n~n~p~n~n", [Ast]),
  process(Ast, [{labels,0},{lvars,0},{mvars,0}]).
process([], _Context) -> {ok,[],[],0,0};
process([St|Ast], Context, V_Cnt, L_Cnt) ->
  {ok,N_Context,St_List,N_V_Cnt,N_L_Cnt} = process(St,Context,V_Cnt,L_Cnt),
  {ok,N2_Context,N_St_List,N2_V_Cnt,N2_L_Cnt} = process(Ast,N_Context,V_Cnt,L_Cnt),
  {ok,N2_Context,St_List++N_St_List,N2_V_Cnt,N2_L_Cnt};

%% Functions
process({function,{Return_Type,{{identifier,_,Ident},Args},Statement}}, Context, V_Cnt, L_Cnt) ->
  Type = check_typedef(Return_Type, Context),
  {ok,N_Context,N_St,N_V_Cnt,N_L_Cnt} = process(Statement, Context, V_Cnt, L_Cnt),
  {ok,N_Context,[{function, Type, Ident, length(Args), N_St}],N_V_Cnt,N_L_Cnt};
process({function, Fn_Spec}, Context, V_Cnt, L_Cnt) ->
  error({unknown_fn_spec,Fn_Spec});

%% Declarations
process({declaration, O_Type, O_Specs}, Context, V_Cnt, L_Cnt) ->
  {ok,Ident,N_St,N_V_Cnt,N_L_Cnt} = get_decl_specs(O_Specs, Context, V_Cnt, L_Cnt),
  Type = check_typedef(O_Type, Context),
  {ok,[{variable,{Ident,{Type,{y,V_Cnt}}}}|Context],N_St,V_Cnt+1,N_L_Cnt};

%% Built-in Functions
process({bif, '+',[A,B]}, Context, V_Cnt, L_Cnt) ->
  {ok,A_Context,A_St,A_V_Cnt,A_L_Cnt} = process(A,Context,V_Cnt,L_Cnt),
  {ok,B_Context,B_St,B_V_Cnt,B_L_Cnt} = process(B,A_Context,A_V_Cnt+1,A_L_Cnt),
  Statement = A_St ++ B_St ++ [{add,V_Cnt,[A_V_Cnt,B_V_Cnt]}],
  {ok,Context,Statement,V_Cnt,B_L_Cnt};

%% Literal values
process({int_l,_,Val,_Suf}, Context, V_Cnt, L_Cnt) ->
  {ok,Context,[{move,{x,0},{integer,Val}}],V_Cnt,L_Cnt};

%% Identifiers
process({identifier,Ln,Ident}, Context, V_Cnt, L_Cnt) ->
  Variables = proplists:get_all_values(variable, Context),
  case proplists:get_value(Ident, Variables) of
    {_Type, X} -> X;
    Other -> error({Other,Ident,{line,Ln},{context,Context}})
  end;

%% Return
process({{return,_},Statement}, Context, V_Cnt, L_Cnt) ->
  {ok,_N_Context,N_St,_N_V_Cnt,N_L_Cnt} = process(Statement, Context, V_Cnt, L_Cnt),
  {ok,Context,N_St,V_Cnt,N_L_Cnt};

%% Base Case
process(Part, Context, V_Cnt, L_Cnt) ->
  io:fwrite("Part:~n~p~n~n",[Part]),
  {ok,[],[],V_Cnt,L_Cnt}.

get_decl_specs([{identifier,_,Ident}],Context, V_Cnt, L_Cnt) ->
  {ok, Ident, [], V_Cnt, L_Cnt};
get_decl_specs([{{identifier,_,Ident},{'=',_},St}],Context, V_Cnt, L_Cnt) ->
  {ok, N_Context, N_St, N_V_Cnt, N_L_Cnt} = process(St,Context,V_Cnt,L_Cnt),
  {ok, Ident, N_St, N_V_Cnt, N_L_Cnt}.


check_typedef(Type, Context) ->
  Typedefs = proplists:get_all_values(typedef, Context),
  case proplists:get_value(Type, Typedefs) of
    undefined -> get_type(Type);
    Typedef -> get_type(Typedef)
  end.

get_type([{long,_},{long,_},{int,_}]) -> int64;
get_type([{long,_},{int,_}]) -> int64;
get_type([{int,_}]) -> int32;

get_type(Type) -> error({unknown_type, Type}).
