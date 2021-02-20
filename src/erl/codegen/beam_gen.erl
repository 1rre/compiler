-module(beam_gen).
-export([translate/1]).

-record(variable, {ident,type}).
-record(function, {ident,type,arity,args}).

translate(Ast) ->
  case translate(Ast,#{}) of
    {ok,_Context,Translation} -> io:fwrite("Translation:~n~s~n",[Translation]);
    {error,Reason} -> error(Reason)
  end.

translate([],Context) -> {ok, #{}, ""};
translate([{function,{Return_Type,Decl,Statement}} | Rest], Context) ->
  io:fwrite("Fn:~nReturns: ~p~nDecl:~n~p~nStatement:~n~p~n", [Return_Type,Decl,Statement]),
  translate(Rest, Context);
translate([{function,{Fn_Spec}} | Rest], Context) -> error({unknown_fn_spec,Fn_Spec});
translate(Part,_) ->
  io:fwrite("Part:~n~p~n",[Part]),
  {ok,#{},""}.
