-module(run_all_test).
-export([run/0]).

%% @doc Simple script to run all tests in the "compiler tests" folder.
%%      Currently this means just building the IR for each but we can add to it later as needed.
%%      I hope to convert this to use "eunit" in the future.

c_test([],_,_) -> 0;
c_test([File|Tests],Gcc,Qemu) ->
  process_flag(trap_exit, true),
  try c_compiler:main(["-S", "-o", ".test/test.s", File]) of
    Result ->
      io:fwrite(standard_error,"~s: \e[1;32mcompiles\e[0;37m~n",[File]),
      open_port({spawn_executable, Gcc},
                [stderr_to_stdout,
                 {args, ["-mfp32", "-o", ".test/test.o", "-c", ".test/test.s"]}]),
      wait_exe(1, false),
      Driver = filename:rootname(File) ++ "_driver.c",
      open_port({spawn_executable, Gcc},
                [stderr_to_stdout,
                 {args, ["-mfp32", "-static", "-o", "test/test.bin", ".test/test.o", Driver]}]),
      wait_exe(1, false),
      
      1
  catch
    _:Err ->
      io:fwrite(standard_error,"~s: \e[1;31mfail\e[0;37m~n",[File]),
      io:fwrite(standard_error,"Reason:~n~p~n",[Err]),
      0
  end + c_test(Tests,Gcc,Qemu).

wait_exe(0, true) ->
  error;
wait_exe(0, _) ->
  ok;
wait_exe(N, Is_Ok) ->
  receive
    {'EXIT', _, normal} -> wait_exe(N - 1, Is_Ok);
    Message ->
      io:fwrite(standard_error,"~p~n",[Message]),
      wait_exe(N, true)
  end.

run() ->
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
  io:fwrite(standard_error,"~B out of ~B tests compile (~f%)~n",[Success,length(Tests),Success/length(Tests)*100]).
