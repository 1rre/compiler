-module(c89_compiler).
-export([main/0, main/1]).


main() -> main(["test/test.c"]).

main([File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  io:fwrite("Tokens:~n~p~n~n~n~n", [Tokens]),
  {Scan, Rest} = type_enum:scan(Tokens),
  io:fwrite("Scanned:~n~p~n~n~n~n", [Scan]),
  io:fwrite("Rest:~n~p~n~n~n~n", [Rest]),
  {ok, Result} = parser:parse(Scan),
  io:fwrite("Result:~n~p~n~n~n~n", [Result]),
  nif_test:send(Result),
  halt(0);

main(_) -> main(["test/test.c"]).

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
