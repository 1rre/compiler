-module(mips_io).

-export([fwrite/3]).

-record(opts,{indent=0,io,debug}).

-define(PRINT_LINE(Indent),io:fwrite(Opts#opts.io,"~*s~n",[Indent+length(Output),Output])).
-define(ST_1,-6).
-define(ST_2,4).
-define(ST_3,7).
-define(ST_4,5).

fwrite(Program,File,Debug) ->
  {ok,Iostream} = case File of
    standard_io -> {ok,standard_io};
    _ -> file:open(File,[write])
  end,
  program(Program,#opts{io=Iostream,debug=Debug}),
  if File =:= standard_io -> ok;
     true -> file:close(Iostream) end.

program([asp_lookahead|Program],Opts) ->
  {ok,Asp} = find_asp(Program),
  program([{addiu,{i,29},{i,29},-Asp}|Program],Opts);
program([{asp_ref,_}|Program],Opts) ->
  program(Program,Opts);
program([],Opts) -> {ok,Opts};
program([Directive|Program],Opts) when is_atom(Directive) ->
  {ok,New_Opts} = directive(Directive,Opts),
  program(Program,New_Opts);
program([Statement|Program],Opts) ->
  {ok,New_Opts} = statement(Statement,Opts),
  program(Program,New_Opts).

find_asp([{asp_ref,Asp}|_]) -> {ok,Asp};
find_asp([_|Rest]) -> find_asp(Rest).

%% TODO: Change indent on different types

directive(Directive='.text',Opts=#opts{indent=I}) ->
  Output = io_lib:format("~s",[Directive]),
  ?PRINT_LINE(I-2),
  {ok,Opts};

directive(Directive='.data',Opts=#opts{indent=I}) ->
  Output = io_lib:format("~s",[Directive]),
  ?PRINT_LINE(I),
  {ok,Opts#opts{indent=I+2}};

directive(Directive,Opts=#opts{indent=I}) ->
  Output = io_lib:format("~s",[Directive]),
  ?PRINT_LINE(I),
  {ok,Opts}.


%% TODO: Change indent on different types
% Label
statement({A,B},Opts=#opts{indent=I}) when is_list(B) ->
  Output = lists:flatten([io_lib:format("$l~B",[N]) || N <- B] ++ io_lib:format("~s",[A]) ++ [$:]),
  ?PRINT_LINE(I),
  {ok,Opts};
%% TODO: Other types of statments with indent
statement({'.end',Name},Opts=#opts{indent=I}) ->
  Output = lists:flatten(io_lib:format("~s ~s",['.end',Name])),
  ?PRINT_LINE(I-2),
  {ok,Opts#opts{indent=I-2}};
statement({'.globl',Name},Opts=#opts{indent=I}) ->
  Output = lists:flatten(io_lib:format("~s ~s",['.globl',Name])),
  ?PRINT_LINE(I),
  {ok,Opts#opts{indent=I}};
statement({'.ent',Name},Opts=#opts{indent=I}) ->
  Output = lists:flatten(io_lib:format("~*s ~s",[-6,'.ent',Name])),
  ?PRINT_LINE(I),
  {ok,Opts#opts{indent=I+2}};
statement({A,B={B1,B2}},Opts=#opts{indent=I}) when is_list(B1) andalso is_list(B2) ->
  Pa = lists:flatten(part(A)),
  Pb = lists:flatten(part(B)),
  Output = lists:flatten(io_lib:format("~*s ~*s",[min(-length(Pa),?ST_1),Pa,max(length(Pb),?ST_2),Pb])),
  ?PRINT_LINE(I),
  {ok,Opts};
statement({A,B},Opts=#opts{indent=I}) ->
  Pa = lists:flatten(part(A)),
  Pb = lists:flatten(part(B)),
  Output = lists:flatten(io_lib:format("~*s ~*s",[min(-length(Pa),?ST_1),Pa,max(length(Pb),?ST_2),Pb])),
  ?PRINT_LINE(I),
  {ok,Opts};
statement({A,B,C},Opts=#opts{indent=I}) ->
  Pa = lists:flatten(part(A)),
  Pb = lists:flatten(part(B)),
  Pc = lists:flatten(part(C)),
  Output = lists:flatten(io_lib:format("~*s ~*s,~*s",[min(-length(Pa),?ST_1),Pa,max(length(Pb),?ST_2),Pb,max(length(Pc),?ST_3),Pc])),
  ?PRINT_LINE(I),
  {ok,Opts};
statement({A,B,C,D},Opts=#opts{indent=I}) ->
  Pa = lists:flatten(part(A)),
  Pb = lists:flatten(part(B)),
  Pc = lists:flatten(part(C)),
  Pd = lists:flatten(part(D)),
  Output = lists:flatten(io_lib:format("~*s ~*s,~*s,~*s",[min(-length(Pa),?ST_1),Pa,max(length(Pb),?ST_2),Pb,max(length(Pc),?ST_3),Pc,max(length(Pd),?ST_4),Pd])),
  ?PRINT_LINE(I),
  {ok,Opts};
% Comment
statement(Str=[$#|_],Opts=#opts{indent=I,debug=true}) when is_list(Str) ->
  Output = [$\n|Str],
  ?PRINT_LINE(I-2),
  {ok,Opts};
statement(Str=[$#|_],Opts=#opts{indent=I}) when is_list(Str) ->
  {ok,Opts}.

part({sp,N}) when is_integer(N) ->
  io_lib:format("~B($29)",[N]);
part([{f,A},{f,_}]) ->
  io_lib:format("$f~B",[A]);
%label
part({A,B}) when is_list(B) ->
  lists:flatten([io_lib:format("$l~B",[N]) || N <- B]) ++ io_lib:format("~s",[A]);
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
  io_lib:format("~B(~s)",[Offset,Reg]);
% Int literal
part(A) when is_integer(A) ->
  io_lib:format("~B",[A]);
% Instruction etc.
part(A) when is_atom(A) ->
  io_lib:format("~s",[A]);
% Other?
part(A) -> error({part,A}).
