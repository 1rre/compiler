#!/usr/bin/env escript

-module(make_compiler).
-export([main/1]).

%% Main function

% Remove all generated files
main(["clean"]) -> 
  file:del_dir_r(".build"),
  file:del_dir_r("bin"),
  [file:delete(File) || File <- filelib:wildcard("src/parsing/*.erl")];

% Compile everything
main(["compile"]) -> 
  file:make_dir(".build"),
  process_flag(trap_exit, true),
  Cxx = get_dep(cxx),
  Include = get_dep(erts),
  Cxx_Files = filelib:wildcard("*.cpp", "src"),
  [compile(Cxx, Nif, Include) || Nif <- Cxx_Files],
  wait_exe(length(Cxx_Files)),
  Leex_Files = filelib:wildcard("src/parsing/*.xrl"),
  [leex:file(Xrl) || Xrl <- Leex_Files],
  Yecc_Files = filelib:wildcard("src/parsing/*.yrl"),
  [c:y(Yrl) || Yrl <- Yecc_Files],
  Erl_Files = filelib:wildcard("src/**/*.erl"),
  [compile:file(Erl, [{outdir, ".build"}, report_errors, report_warnings]) || Erl <- Erl_Files],
  halt(0);

% Compile everything
main(["bin/compiler"]) -> 
  file:make_dir(".build"),
  file:make_dir("bin"),
  process_flag(trap_exit, true),
  Cxx = get_dep(cxx),
  Include = get_dep(erts),
  Cxx_Files = filelib:wildcard("*.cpp", "src"),
  [compile(Cxx, Nif, Include) || Nif <- Cxx_Files],
  wait_exe(length(Cxx_Files)),
  Leex_Files = filelib:wildcard("src/parsing/*.xrl"),
  [leex:file(Xrl) || Xrl <- Leex_Files],
  Yecc_Files = filelib:wildcard("src/parsing/*.yrl"),
  [c:y(Yrl) || Yrl <- Yecc_Files],
  Erl_Files = filelib:wildcard("src/**/*.erl"),
  Bin = [{change_ext(Erl, beam), element(3, compile:file(Erl, [binary]))} || Erl <- Erl_Files],
  escriptise(Bin),
  halt(0);

% For ease, no arguments compiles everything 
main(_) -> main(["bin/compiler"]).


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
    [Version | _] -> filename:join(code:root_dir(), Version);
    _ -> error("Couldn't find the Erlang Runtime System include directory. Try setting the 'ERTS_INCLUDE' environment variable.")
  end;

% C++ compiler
find(cxx) -> find(cxx, ["g++", "clang++"]).
find(cxx, []) -> error("Couldn't find C++ Compiler. Try setting the 'CXX' environment variable.");
find(cxx, [Cxx | Rest]) ->
  case os:find_executable(Cxx) of
    false -> find(cxx, Rest);
    Exec -> Exec
  end.


%% Asynchronously compile a C++ file
compile(Cxx, Nif, Include) ->
  open_port(
    {spawn_executable, Cxx},
     [stderr_to_stdout,
      {args,
       ["-I", Include,
        "-o", filename:join(".build", change_ext(Nif, "so")),
        "-fpic",
        "-shared",
        filename:join("src", Nif)]}]).


%% Ensure that all C++ files have compiled correctly
wait_exe(0) -> ok;
wait_exe(N) -> 
  receive
    {'EXIT', _, normal} -> wait_exe(N - 1);
    Message -> error(Message)
  end.


%% Change a file's 4 character extention, such as '.cpp' or '.xrl' to a different extention
% Beam files are a special case as we want a local path
change_ext(File, beam) -> filename:basename(File, "erl") ++ "beam";
% All other files keep an absolute path
change_ext(File, Ext) -> lists:sublist(File, length(File) - 3) ++ Ext.

escriptise(Bin) ->
  Status = escript:create("bin/c89_compiler",
                 [shebang,
                  {archive, Bin, []}]),
  io:fwrite("~p~n", [Status]),
  open_port({spawn_executable, os:find_executable("chmod")},
             [stderr_to_stdout, 
              {args, ["a+x", "bin/c89_compiler"]}]),
  wait_exe(1).

