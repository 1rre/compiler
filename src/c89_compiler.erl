-module(c89_compiler).
-export([main/1]).

main([File]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(Input),
  {ok, Result} = parser:parse(Tokens),
  io:fwrite("~p~n", [Result]),
  halt(0);

main(_) -> main("test/test.c").

read_file(Io_Stream) -> 
  case file:read_line(Io_Stream) of
    eof -> {ok, []};
    {ok, Line} ->
      case read_file(Io_Stream) of
        {ok, Data} -> {ok, [Line | Data]};
        {error, Err} -> {error, Err}
      end;
    {error, Err} -> {error, Err}
  end.
