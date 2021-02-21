-module(beam_gen).
-export([translate/1]).

-record(variable, {ident,type}).
-record(function, {ident,type,arity,args}).

translate(Ast) ->
  case translate(Ast,#{}) of
    {ok,_Context,Translation} -> io:fwrite("Translation:~n~s~n",[Translation]);
    {error,Reason} -> error(Reason)
  end.

translate([],Context) -> {ok,#{},""};
translate([Part | Rest], Context) ->
  {ok,N_Context,Stat_Tran} = translate(Part,Context),
  {ok,_,Translation} = translate(Rest,N_Context),
  {ok,N_Context,Translation++Stat_Tran};
translate({function,{Return_Type,Decl,Statement}}, Context) ->
  io:fwrite("Fn:~nReturns:~n\t~p~nDecl:~n\t~p~nStatement:~n\t~p~n~n",[Return_Type,Decl,Statement]),
  translate(Statement,Context);
translate({function,{Fn_Spec}}, Context) ->
  error({unknown_fn_spec,Fn_Spec});
translate([{declaration,Type,Specs}|Rest],Context) ->
  io:fwrite("Declaration:~nType:~n\t~p~nSpecs:~n\t~p~n~n",[Type, Specs]),
  translate(Rest, Context);
translate(Part,_Context) ->
  io:fwrite("Part:~n~p~n~n",[Part]),
  {ok, #{}, ""}.
