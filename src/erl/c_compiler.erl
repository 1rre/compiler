-module(c_compiler).
-export([main/1,run_vm/1,run_vm/3]).

main([File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = build_ir:process(Result),
  {ok, Statement};

main(["-vm",File]) -> run_vm(File);

main(_) -> main(["test/test.c"]).

run_vm(File) -> run_vm(File,[],main).
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
