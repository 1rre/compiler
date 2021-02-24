-module(run_all_test).
-export([main/1]).

main(_Args) ->
  [c_compiler:main([F]) || F <- filelib:wildcard("compiler_tests/*/*.c") -- filelib:wildcard("compiler_tests/*/*_driver.c")].
