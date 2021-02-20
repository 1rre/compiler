-module(type_check).
-export([check_ast/1]).

-record(variable, {ident, type}).
-record(function, {ident, type, arity, args}).
-record(typedef, {ident, type}).
-record(container, {ident, members}). % Encompasses struct, union & enum for type checking

check_ast(Ast) -> case check_ast(Ast, #{}) of
  {error, Reason} -> error(Reason);
  {ok, _Context} -> ok
end.

check_ast([], Context) -> {ok, #{}};
check_ast([Part | Rest], Context) ->
  case check_ast(Part, Context) of
    {error, Reason} -> error(Reason);
    {ok, New_Context} -> check_ast(Rest, New_Context)
  end;

check_ast({[{typedef,L0}|Type],[Ident]}, _Context) ->
  io:fwrite("typedef:~n~p~n~n", [Ident]),
  {ok, #{}};

check_ast(Statement, _Context) ->
  io:fwrite("~p~n~n", [Statement]),
  {ok, #{}}.
