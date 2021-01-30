-module(make_compiler).
-export([main/1]).


main(["clean"]) -> 
  file:del_dir(".build"),
  file:del_dir("bin");

main(["bin/compiler"]) -> 
  io:fwrite("Started!"),
  process_flag(trap_exit, true),
  Cxx = get_dep(cxx),
  Include = get_dep(erts),
  Cxx_Files = filelib:wildcard("*.cpp", "src"),
  file:make_dir(".build"),
  [compile(Cxx, Nif, Include) || Nif <- Cxx_Files],
  monitor_cxx(length(Cxx_Files));

main(_) -> main(["bin/c_compiler"]).


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


find(erts) -> 
  case filelib:wildcard("erts*/include/", code:root_dir()) of
    [Version] -> 
      filename:join(code:root_dir(), Version);
    [Version | _] -> filename:join(code:root_dir(), Version);
    _ -> error("Couldn't find the Erlang Runtime System include directory. Try setting the 'ERTS_INCLUDE' environment variable.")
  end;

find(cxx) -> find(cxx, ["g++", "clang++"]).
find(cxx, []) -> error("Couldn't find C++ Compiler. Try setting the 'CXX' environment variable.");
find(cxx, [Cxx | Rest]) ->
  case os:find_executable(Cxx) of
    false -> find(cxx, Rest);
    Exec -> Exec
  end.


compile(Cxx, Nif, Include) ->
  open_port(
    {spawn_executable, Cxx},
     [stderr_to_stdout,
      {args,
       ["-I", Include,
        "-o", filename:join(".build", lists:sublist(Nif, length(Nif) - 4) ++ ".so"),
        "-fpic",
        "-shared",
        filename:join("src", Nif)]}]).
  

monitor_cxx(0) -> ok;
monitor_cxx(N) -> 
  receive
    {'EXIT', _, normal} -> monitor_cxx(N - 1);
    Message -> error(Message)
  end.



