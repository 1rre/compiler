-module(c_compiler).
-export([main/1,run_vm/2,run_vm/3]).

main(Args) ->
  Opts = lists:sort(get_args(Args)),
  File = proplists:get_value(in, Opts),
  Fn_Args = [list_to_integer(Arg)||{arg,Arg} <- Opts],
  N_Args = [Atom || Atom <- Opts, is_atom(Atom) or (is_tuple(Atom) andalso (element(1,Atom) =:= out))],
  compile(File,Fn_Args,N_Args).

get_args(["-S"|Rest]) -> [asm|get_args(Rest)];
get_args(["-o",File|Rest]) -> [{out,File}|get_args(Rest)];
get_args(["-d"|Rest]) -> [debug|get_args(Rest)];
get_args(["-ir"|Rest]) -> [ir|get_args(Rest)];
get_args(["-vm"|Rest]) -> [vm|get_args(Rest)];
get_args(["-ast"|Rest]) -> [ast|get_args(Rest)];
get_args([Arg|Rest]) ->
  case filelib:is_file(Arg) of
    true -> [{in,Arg}|get_args(Rest)];
    _ -> [{arg,Arg}|get_args(Rest)]
  end;
get_args([]) -> [].


compile(File,_,[ir]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  io:fwrite("~p~n",[Statement]),
  {ok, Statement};

compile(File,_,[ast]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  io:fwrite("~p~n",[Result]),
  {ok, Result};

compile(File, Args, [debug,vm]) ->
  halt(run_vm(File,Args,[debug]));

compile(File, Args, [vm]) ->
  halt(run_vm(File,Args));

compile(File,_,[asm,{out,Out_File}]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Mips_Code} = mips:generate(Statement),
  mips_io:fwrite(Mips_Code,Out_File),
  {ok, Mips_Code};

compile(File,_,[asm,debug]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Mips_Code} = mips:generate(Statement),
  io:fwrite("~p~n~n",[Mips_Code]),
  mips_io:fwrite(Mips_Code,standard_io),
  {ok, Mips_Code};

compile(File,_,[asm,ir]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok,Tokens,_} = erl_scan:string(Input),
  {ok,[Expr]} = erl_parse:parse_exprs(Tokens),
  Statement = erl_parse:normalise(Expr),
  {ok,Mips_Code} = mips:generate(Statement),
  mips_io:fwrite(Mips_Code,standard_io),
  {ok, Mips_Code};

compile(File,_,[asm]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Mips_Code} = mips:generate(Statement),
  mips_io:fwrite(Mips_Code,standard_io),
  {ok,Mips_Code};

compile(File,_,[]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Statement}.

% Reversing the IR for now as we want main to be at the start rather than at the end
run_vm(File,Args) ->
  {ok,Ir} = compile(File,Args,[]),
  ir_vm:run(lists:reverse(Ir),lists:reverse(Args),[]).
run_vm(File,Fn,Args) when is_atom(Fn) ->
  {ok,Ir} = compile(File,Args,[]),
  ir_vm:run(lists:reverse(Ir),Fn,lists:reverse(Args),[]);
run_vm(File,Args,Flags) ->
  {ok,Ir} = compile(File,Args,[]),
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
