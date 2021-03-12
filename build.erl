#!/usr/bin/env escript
-module(build).
-export([main/1]).
-define(ERLC_FLAGS,[{i,"./include"}]).
%% Main function

% Remove all generated files
main(["clean"]) ->
  file:del_dir_r(".build"),
  file:del_dir_r("bin"),
  [file:delete(File) || File <- filelib:wildcard("src/parsing/*.erl")];

% Compile everything
main(["compile"]) ->
  Erl_Files = build_common(),
  io:fwrite("~p~n", [Erl_Files]),
  [compile:file(Erl, [{outdir,".build"},{d,'TARGET_ARCH',mips32},report|?ERLC_FLAGS]) || Erl <- Erl_Files],
  halt(0);

% Compile everything
main(["bin/c_compiler"]) ->
  file:make_dir("bin"),
  Erl_Files = build_common(),
  io:fwrite("Compiling erlang files for ~s: ~p~n",[mips32,Erl_Files]),
  Pids = [compile_erl(Erl,[{d,'TARGET_ARCH',mips32}]) || Erl <- Erl_Files],
  Bin = [receive
           {Pid,{File,{ok,_Mod,Bin}}} -> {File,Bin};
           {Pid,Error} -> error(Error)
         end || Pid <- Pids],
  escriptise(Bin),
  halt(0);

main(["-debug"]) ->
  file:make_dir("bin"),
  Erl_Files = build_common(),
  io:fwrite("Compiling erlang files for ~s: ~p~n",[amd64,Erl_Files]),
  Pids = [compile_erl(Erl,[{d,'TARGET_ARCH',amd64}]) || Erl <- Erl_Files],
  Bin = [receive
           {Pid,{File,{ok,_Mod,Bin}}} -> {File,Bin};
           {Pid,Error} -> error(Error)
         end || Pid <- Pids],
  escriptise(Bin),
  halt(0);


% For ease, no arguments compiles everything
main([File]) ->
  Files = filelib:wildcard([$*,$*,$/|File]),
  lists:search(fun (F) ->
    case filelib:is_file(F) and (filename:extension(F) =:= ".erl") of
      true -> compile:file(F,[report,binary,{d,'TARGET_ARCH',mips32}|?ERLC_FLAGS]);
      _ -> false
    end =:= ok
  end, Files);

main([]) -> main(["bin/c_compiler"]).

%% Common tasks for compiling to beam files (shared object files) and an escript (a binary file)
build_common() ->
  case string:to_integer(erlang:system_info(otp_release)) of
    {X, _} when X >= 23 -> ok;
    _ -> error("Erts v23.0 or later required")
  end,
  file:make_dir(".build"),
  process_flag(trap_exit, true),
  Leex_Files = filelib:wildcard("src/parsing/*.xrl"),
  [leex:file(Xrl) || Xrl <- Leex_Files],
  Yecc_Files = filelib:wildcard("src/parsing/*.yrl"),
  [c:y(Yrl,{report_warnings,false}) || Yrl <- Yecc_Files],
  filelib:wildcard("src/**/*.erl").

compile_erl(Erl,Flags) ->
  S = self(),
  spawn_link(fun() ->
    S ! {self(), {change_ext(Erl,beam),compile:file(Erl,[binary|?ERLC_FLAGS]++Flags)}}
  end).

%% Wait for an executable to exit & ensure the exit code is 0
wait_exe(0, true) ->
  ("Compilation had errors.");
wait_exe(0, _) ->
  ok;
wait_exe(N, Is_Ok) ->
  receive
    {'EXIT', _, normal} -> wait_exe(N - 1, Is_Ok);
    {_Port, {data, Message}} ->
      io:fwrite(standard_error, "~s~n", [Message]),
      wait_exe(N, true);
    Message ->
      io:fwrite(standard_error, "~s~n", [Message]),
      wait_exe(N, true)
  end.

%% Change a erlang file's extension to .beam
change_ext(File, beam) -> filename:basename(File, "erl") ++ "beam".

%% Create an escript archive with all the compiled modules & make it executable
escriptise(Bin) ->
  ok = escript:create("bin/c_compiler",[shebang,{archive,Bin,[]}]),
  case os:type() of
    {_,nt} -> ok;
    _ ->
      open_port({spawn_executable, os:find_executable("chmod")},
                 [stderr_to_stdout,
                  {args, ["a+x", "bin/c_compiler"]}]),
      wait_exe(1, false)
  end.
