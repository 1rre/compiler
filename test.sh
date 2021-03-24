#!/bin/sh
clear &&\
  ./build.erl compile &&\
  clear &&\
  erl -pa .build -noshell -eval 'run_all_test:run(),halt(0)'
