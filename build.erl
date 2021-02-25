#!/usr/bin/env escript
-module(build).
-export([main/1]).
%% Main function

% Remove all generated files
main(["clean"]) ->
  file:del_dir_r(".build"),
  file:del_dir_r("bin"),
  [file:delete(File) || File <- filelib:wildcard("src/parsing/*.erl")];

% Compile everything
main(["compile"]) ->
  Erl_Files = build_common(),
  [compile:file(Erl, [{outdir, ".build"}, report_errors, report_warnings]) || Erl <- Erl_Files],
  halt(0);

% Compile everything
main(["bin/c_compiler"]) ->
  file:make_dir("bin"),
  Erl_Files = build_common(),
  S=self(),
  Pids = [spawn_link(fun() -> S ! {self(), {change_ext(Erl, beam),compile:file(Erl,[binary])}} end) || Erl <- Erl_Files],
  Bin = [receive
           {Pid,{File,{ok,_Mod,Bin}}} -> {File,Bin};
           {Pid,Error} -> error(Error)
         end || Pid <- Pids],
  escriptise(Bin),
  halt(0);

% For ease, no arguments compiles everything
main(_) -> main(["bin/c_compiler"]).

%% Common tasks for compiling to beam files (shared object files) and an escript (a binary file)
build_common() ->
  case string:to_integer(erlang:system_info(otp_release)) of
    {X, _} when X >= 22 -> ok;
    _ -> error("Erts v22.0 or later required")
  end,
  file:make_dir(".build"),
  process_flag(trap_exit, true),
  Cxx = get_dep(cxx),
  Include = get_dep(erts),
  Cxx_Files = filelib:wildcard("src/cpp/*.cpp"),
  [compile(Cxx, Nif, Include) || Nif <- Cxx_Files],
  wait_exe(length(Cxx_Files), false),
  Leex_Files = filelib:wildcard("src/parsing/*.xrl"),
  [leex:file(Xrl) || Xrl <- Leex_Files],
  Yecc_Files = filelib:wildcard("src/parsing/*.yrl"),
  [c:y(Yrl,{report_warnings,false}) || Yrl <- Yecc_Files],
  filelib:wildcard("src/**/*.erl").


%% Collate the dependencies for compilation
% C++ compiler
get_dep(cxx) ->
  case os:getenv("CXX") of
    false ->
      io:fwrite("CXX environment variable not set: autodetecting C++ compiler.~n"),
      find(cxx);
    Cxx_Env ->
      case os:find_executable(Cxx_Env) of
        false ->
          io:fwrite("CXX environment variable is not in path: autodetecting C++ compiler.~n"),
          find(cxx);
        Exec -> Exec
      end
  end;

% Erlang runtime system include directory
get_dep(erts) ->
  case os:getenv("ERTS_INCLUDE") of
    false ->
      io:fwrite("ERTS_INCLUDE environment variable not set: autodetecting ERTS include directory.~n"),
      find(erts);
    Include ->
      case filelib:is_dir(Include) and filelib:is_file(filename:join(Include, "erl_nif.h")) of
        true -> Include;
        false ->
          io:fwrite("ERTS_INCLUDE path is invalid: autodetecting ERTS include directory.~n"),
          find(erts)
      end
  end.


%% Autodetect the dependencies for compilation
% Erlang runtime system include directory
find(erts) ->
  case filelib:wildcard("erts*/include/", code:root_dir()) of
    [Version | _] ->
      io:fwrite("Using ERTS Version `~s`.~n", [lists:takewhile(fun (C) -> C /= $/ end, Version)]),
      filename:join(code:root_dir(), Version);
    _ -> error("Couldn't find the Erlang Runtime System include directory. Try setting the 'ERTS_INCLUDE' environment variable and ensuring the `erlang_dev` package is installed.")
  end;

% C++ compiler
find(cxx) -> find(cxx, ["g++", "clang++"]).
find(cxx, []) -> error("Couldn't find C++ Compiler. Try setting the 'CXX' environment variable.");
find(cxx, [Cxx | Rest]) ->
  case os:find_executable(Cxx) of
    false -> find(cxx, Rest);
    Exec ->
      io:fwrite("Using `~s` as CXX.~n",[Cxx]),
      Exec
  end.


%% Asynchronously compile a C++ file
compile(Cxx, Nif, Include) ->
  open_port(
    {spawn_executable, Cxx},
     [stderr_to_stdout,
      {args,
       ["-I", Include,
        "-o", filename:join(".build", change_ext(Nif, so)),
        "-fpic",
        "-shared",
        Nif]}]).


%% Ensure that all C++ files have compiled correctly
wait_exe(0, true) ->
  error("Compilation had errors.");
wait_exe(0, _) ->
  ok;
wait_exe(N, Is_Ok) ->
  receive
    {'EXIT', _, normal} -> wait_exe(N - 1, Is_Ok);
    {_Port, {data, Message}} ->
      io:fwrite(standard_error, "~s~n", [Message]),
      wait_exe(N, true);
    _Message -> wait_exe(N, true)
  end.


%% Change a file's 4 character extention, such as '.cpp' or '.xrl' to a different extention
change_ext(File, beam) -> filename:basename(File, "erl") ++ "beam";
change_ext(File, so) -> filename:basename(File, "cpp") ++ "so".

escriptise(Bin) ->
  ok = escript:create("bin/c_compiler",
                      [shebang,
                       {archive, Bin, []}]),
  case os:type() of
    {_,nt} -> ok;
    _ ->
      open_port({spawn_executable, os:find_executable("chmod")},
                 [stderr_to_stdout,
                  {args, ["a+x", "bin/c_compiler"]}]),
      wait_exe(1, false)
  end.
