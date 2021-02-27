-module(run_all_test).
-export([main/1]).

%% @doc Simple script to run all tests in the "compiler tests" folder.
%%      Currently this means just building the IR for each but we can add to it later as needed.
%%      I hope to convert this to use "eunit" in the future.

main(_Args) ->
  [c_compiler:main([F]) || F <- filelib:wildcard("compiler_tests/*/*.c") -- filelib:wildcard("compiler_tests/*/*_driver.c")].
