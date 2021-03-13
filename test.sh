#!/bin/sh
if [ $# -ne 0 ]; then
escript build.erl clean
escript build.erl compile
fi
erl -pa .build -noshell -eval 'run_all_test:run(),halt(0)'

