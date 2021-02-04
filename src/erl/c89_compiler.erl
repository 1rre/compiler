-module(c89_compiler).
-export([main/1]).


main([File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  io:fwrite("Ast:~n~p~n~n~n~n", [Result]),
  ast_nif:send(Result),
  halt(0);

main(["--debug", File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  io:fwrite("Read:~n~s~n~n", [Input]),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  io:fwrite("Tokens:~n~p~n~n", [Tokens]),
  {Scan, Rest} = type_enum:scan(Tokens),
  io:fwrite("Processed Typedefs:~n~p~n~nRemainder was: ~s~n~n", [Scan, Rest]),
  {ok, Result} = parser:parse(Scan),
  io:fwrite("Ast:~n~p~n~n~n~n", [Result]),
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

