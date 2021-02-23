-module(beam_gen).
-export([translate/1]).

translate(Ast) ->
  case translate(Ast,#{}) of
    {ok,Context,Translation} ->
      Globals = [io_lib:format(".globl ~s~n",[Ident]) ||
                 {Ident, Spec} <- maps:to_list(Context),
                 (element(1,Spec) =:= function) or (element(1,Spec) =:= variable)],
      io:fwrite("~s~s",[Globals,Translation]);
    {error,Reason} -> error(Reason)
  end.

translate([],Context) -> {ok,#{},""};
translate([Part | Rest], Context) ->
  {ok,N_Context,Stat_Tran} = translate(Part,Context),
  {ok,_,Translation} = translate(Rest,N_Context),
  {ok,N_Context,Translation++Stat_Tran};

%% Functions
translate({function,{Return_Type,{{identifier,_,Ident},[]},Statement}}, Context) ->
  Type = get_type(Return_Type, Context),
  case translate(Statement,Context) of
    {ok,_,Trn} ->
      Out = io_lib:format("~s:~n~s~n",[Ident,Trn]),
      {ok,maps:put(Ident,{function,Type},Context),Out}
  end;
translate({function,{Fn_Spec}}, Context) ->
  error({unknown_fn_spec,Fn_Spec});

%% Declarations
translate({declaration,O_Type,O_Specs},Context) ->
  Specs = get_specs(O_Specs,Context),
  Type = get_type(O_Type,Context),
  {ok,maps:put(Specs,{variable,Type},Context),""};

%% Literals
translate({int_l,_,Val,[]},Context) ->
  {ok,Context,io_lib:format("li $2, ~B~n",[Val])};
translate({int_l,N,Val,_Specs},Context) ->
  translate({int_l,N,Val,[]},Context);

%% BIFs
translate({bif,'+',[A,B]},Context) ->
  Trn_A = case translate(A,Context) of
    {ok, _, A_Trn} -> A_Trn;
    Unex_A -> error({unexpected,Unex_A})
  end,
  Trn_B = case translate(B,Context) of
    {ok, _, B_Trn} -> B_Trn;
    Unex_B -> error({unexpected,Unex_B})
  end,
  Out = io_lib:format("~smove $8, $2~n~sadd $2, $8, $2~n",[Trn_A,Trn_B]),
  {ok,Context,Out};

%% Return
translate({{return,_},Val},Context) ->
  case translate(Val,Context) of
    {ok,_,Trn} ->
      Out = io_lib:format("~sjr $ra~n",[Trn]),
      {ok,Context,Out};
    Unex -> error({unexpected,Unex})
  end;

%% Nil
translate(nil,Context) ->
  {ok,Context,""};

%% Base case
translate(Part,Context) ->
  io:fwrite(standard_error,"Part:~n~p~n~n",[Part]),
  {ok,Context,""}.


get_specs([{{identifier,_,Ident},{'=',_},Expr}|Rest],Context) ->
  [{Ident, translate(Expr,Context)}|get_specs(Rest,Context)];
get_specs([],_Context) -> [];
get_specs(Type,_) ->
  error({unknown_t,Type}).


get_type([{int,_}],Context) ->
  case maps:get({typedef,int},Context,nomatch) of
    nomatch -> int;
    Type -> get_type(Type,Context)
  end;
get_type(Type,_) -> error({not_implemented, Type}).
