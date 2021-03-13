-module(run_all_test).
-export([run/0]).

%% @doc Simple script to run all tests in the "compiler tests" folder.
%%      Currently this means just building the IR for each but we can add to it later as needed.
%%      I hope to convert this to use "eunit" in the future.

c_test([],_,_) -> 0;
c_test([File|Tests],Gcc,Qemu) ->
  process_flag(trap_exit, true),
  io:fwrite(standard_error,"~n~s: ",[File]),
  try c_compiler:main(["-S", "-o", ".test/test.s", File]) of
    Result ->
      open_port({spawn_executable, Gcc},
                [stderr_to_stdout,
                 exit_status,
                 {args, ["-mfp32", "-o", ".test/test.o", "-c", ".test/test.s"]}]),
      wait_exe(),
      Driver = filename:rootname(File) ++ "_driver.c",
      open_port({spawn_executable, Gcc},
                [stderr_to_stdout,
                 exit_status,
                 {args, ["-mfp32", "-static", "-o", "test/test.bin", ".test/test.o", Driver]}]),
      wait_exe(),
      open_port({spawn_executable, Qemu},
                [stderr_to_stdout,
                 exit_status,
                 {args, ["test/test.bin"]}]),
      case wait_exe() of
        0 ->
          io:fwrite(standard_error,"\e[1;32mpass\e[0;37m~n",[]),
          1;
        N ->
          io:fwrite(standard_error,"\e[1;31mfail\e[0;37m~n",[]),
          io:fwrite(standard_error,"Reason:~nqemu-mips exited with code ~B~n",[N]),
          0
      end
  catch
    _:Err ->
      io:fwrite(standard_error,"\e[1;31mfail\e[0;37m~n",[]),
      io:fwrite(standard_error,"Reason:~n~p~n",[Err]),
      0
  end + c_test(Tests,Gcc,Qemu).

wait_exe() ->
  receive
    {_,{exit_status,N}} -> N
  end.

run() ->
  file:make_dir(".test"),
  Tests = filelib:wildcard("compiler_tests/*/*.c") -- filelib:wildcard("compiler_tests/*/*_driver.c"),
  Gcc = case {os:find_executable("mips-linux-gnu-gcc"),os:find_executable("mips-linux-musl-gcc")} of
    {false,false} -> error({not_found,'mips-linux-gnu-gcc'});
    {false,Gcc_Musl} -> Gcc_Musl;
    {Gcc_Gnu,_} -> Gcc_Gnu
  end,
  Qemu = case os:find_executable("qemu-mips") of
    false -> error({not_found,'qemu-mips'});
    Qemu_Mips -> Qemu_Mips
  end,
  Success = c_test(Tests,Gcc,Qemu),
  io:fwrite(standard_error,"~B out of ~B tests passed (~f%)~n",[Success,length(Tests),Success/length(Tests)*100]).
