-module(c_compiler).
-export([main/1,run_vm/2,run_vm/3]).

main(Args) ->
  Opts = lists:sort(get_args(Args)),
  File = proplists:get_value(in, Opts),
  compile(File,proplists:delete(in, Opts)).

get_args(["-S"|Rest]) -> [asm|get_args(Rest)];
get_args(["-o",File|Rest]) -> [{out,File}|get_args(Rest)];
get_args(["-d"|Rest]) -> [debug|get_args(Rest)];
get_args(["-ir"|Rest]) -> [ir|get_args(Rest)];
get_args(["-vm"|Rest]) -> [vm|get_args(Rest)];
get_args([File|Rest]) -> [{in,File}|get_args(Rest)];
get_args([]) -> [].


compile(File,[ir]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  io:fwrite("~p~n",[Statement]),
  {ok, Statement};

%compile([File|Args], [debug,vm]) -> halt(run_vm(File,[list_to_integer(N)||N<-Args],[debug]));
%compile([File|Args], [vm]) -> halt(run_vm(File,[list_to_integer(N)||N<-Args]));

compile(File,[asm,{out,Out_File}]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Mips_Code} = mips:generate(Statement,{file,Out_File}),
  {ok, Mips_Code};

compile(File,[asm]) ->
  compile(File,[asm,{out,".test/test.s"}]);

compile(File,[]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Statement}.

% Reversing the IR for now as we want main to be at the start rather than at the end
run_vm(File,Args) ->
  {ok,Ir} = main([File]),
  ir_vm:run(lists:reverse(Ir),lists:reverse(Args),[]).
run_vm(File,Fn,Args) when is_atom(Fn) ->
  {ok,Ir} = main([File]),
  ir_vm:run(lists:reverse(Ir),Fn,lists:reverse(Args),[]);
run_vm(File,Args,Flags) ->
{ok,Ir} = main([File]),
ir_vm:run(lists:reverse(Ir),lists:reverse(Args),Flags).

read_file(Io_Stream) ->
  case file:read_line(Io_Stream) of
    eof -> {ok, []};
    {ok, Line} ->
      case read_file(Io_Stream) of
        {ok, Data} -> {ok, Line ++ Data};
        {error, Err} -> {error, Err}
      end;
    {error, Err} -> {error, Err}
  end.
