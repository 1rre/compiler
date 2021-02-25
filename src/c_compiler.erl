-module(c_compiler).
-export([main/1,run_vm/1,run_vm/3]).

main(["-ir", File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = build_ir:process(Result),
  io:fwrite("~p~n",[Statement]),
  {ok, Statement};

main([File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  io:fwrite("~p~n",[Result]),
  {ok, _Context, Statement} = build_ir:process(Result),
  {ok, Statement};

main(["-vm",File]) -> halt(run_vm(File));

main(_) ->
  {ok,Ast} = main(["test/test.c"]),
  io:fwrite("~p~n",[Ast]).

run_vm(File) -> run_vm(File,main,[]).
run_vm(File,Fn,Args) ->
  {ok,Ir} = main([File]),
  ir_vm:run(Ir,Fn,Args).

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
