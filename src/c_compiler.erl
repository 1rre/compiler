-module(c_compiler).
-export([main/1,run_vm/2,run_vm/3]).

main(Args) ->
  Debug = case lists:member("-d", Args) of
            true -> [debug];
            _ -> []
          end,
  Ir = case lists:member("-ir", Args) of
         true -> [ir|Debug];
         _ -> Debug
       end,
  Vm = case lists:member("-vm", Args) of
         true -> [vm|Ir];
         _ -> Ir
       end,
  Nif = case lists:member("-nif", Args) of
         true -> [nif|Vm];
         _ -> Vm
       end,
  Mips = case lists:member("-mips", Args) of
         true -> [mips|Nif];
         _ -> Nif
       end,
  Opts = Mips,
  File = lists:dropwhile(fun (F) -> not filelib:is_file(F) end, Args),
  compile(File,Opts).


compile([File|_],[ir]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  io:fwrite("~p~n",[Statement]),
  {ok, Statement};

compile([File|Args], [vm,debugs]) -> halt(run_vm(File,[list_to_integer(N)||N<-Args],[debug]));
compile([File|Args], [vm]) -> halt(run_vm(File,[list_to_integer(N)||N<-Args]));

compile([File|_],[nif]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  io:fwrite("Sending:~n~p~n",[Statement]),
  ir2mips:translate(Statement);

compile([File|_],[mips]) ->
  {ok, Io_Stream} = file:open(File, [read]),
  {ok, Input} = read_file(Io_Stream),
  {ok, Tokens, _} = lexer:string(lists:flatten(Input)),
  {Scan, _Rest} = type_enum:scan(Tokens),
  {ok, Result} = parser:parse(Scan),
  {ok, _Context, Statement} = ir:generate(Result),
  {ok, Mips_State, Mips_Code} = mips:generate(Statement),
  {ok, Mips_Code};

compile([File|_],[]) ->
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
