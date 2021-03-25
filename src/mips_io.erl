-module(mips_io).

-export([fwrite/2]).

-record(opts,{indent=0,io}).

-define(PRINT_LINE,io:fwrite(Opts#opts.io,"~*s~n",[Opts#opts.indent+length(Output),Output])).

fwrite(Program,File) ->
  {ok,Iostream} = case File of
    standard_io -> {ok,standard_io};
    _ -> file:open(File,[write])
  end,
  program(Program,#opts{io=Iostream}),
  if File =:= standard_io -> ok;
     true -> file:close(Iostream) end.

program([],Opts) -> {ok,Opts};
program([Directive|Program],Opts) when is_atom(Directive) ->
  {ok,New_Opts} = directive(Directive,Opts),
  program(Program,New_Opts);
program([Statement|Program],Opts) ->
  {ok,New_Opts} = statement(Statement,Opts),
  program(Program,New_Opts).

%% TODO: Change indent on different types
directive(Directive,Opts) ->
  Output = io_lib:format("~s",[Directive]),
  ?PRINT_LINE,
  {ok,Opts}.


%% TODO: Change indent on different types
% Label
statement({A,B},Opts) when is_list(B) ->
  Output = lists:flatten(io_lib:format("~s",[A]) ++ [io_lib:format("$~B",[N]) || N <- B] ++ [$:]),
  ?PRINT_LINE,
  {ok,Opts#opts{indent=Opts#opts.indent}};%+length(Output)}};
%% TODO: Other types of statments with indent
statement({'.end',Name},Prev_Opts) ->
  Opts = Prev_Opts#opts{indent=0},
  Output = io_lib:format("~s ~s",['.end',Name]),
  ?PRINT_LINE,
  {ok,Opts};
statement({A,B={B1,B2}},Opts) when is_list(B1) andalso is_list(B2) ->
  Output = lists:flatten(part(A)++[$ |part(B)]),
  ?PRINT_LINE,
  {ok,Opts};
statement({A,B},Opts) ->
  Output = part(A)++[$ |part(B)],
  ?PRINT_LINE,
  {ok,Opts};
statement({A,B,C},Opts) ->
  Output = part(A)++[$ |part(B)]++[$,|part(C)],
  ?PRINT_LINE,
  {ok,Opts};
statement({A,B,C,D},Opts) ->
  Output = part(A)++[$ |part(B)]++[$,|part(C)]++[$,|part(D)],
  ?PRINT_LINE,
  {ok,Opts}.

part({sp,N}) when is_integer(N) ->
  io_lib:format("~B($29)",[N]);
part([{f,A},{f,_}]) ->
  io_lib:format("$f~B",[A]);
%label
part({A,B}) when is_list(B) ->
  io_lib:format("~s",[A]) ++ lists:flatten([io_lib:format("$~B",[N]) || N <- B]);
% Float register
part({f,B}) when is_integer(B) ->
  io_lib:format("$f~B",[B]);
% General register
part({R,B}) when is_integer(B) and ((R =:= i) or (R =:= s)) ->
  io_lib:format("$~B",[B]);
% Offset & register
part({Offset,{R,B}}) when is_integer(Offset) ->
  Reg = part({R,B}),
  % Idk why this space is needed here but it hates not having it
  io_lib:format("~B(~s) ",[Offset,Reg]);
% Int literal
part(A) when is_integer(A) ->
  io_lib:format("~B",[A]);
% Instruction etc.
part(A) when is_atom(A) ->
  io_lib:format("~s",[A]);
% Other?
part(A) -> error({part,A}).
