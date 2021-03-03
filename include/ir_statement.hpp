#ifndef IR_STATEMENT_HPP
#define IR_STATEMENT_HPP

#include "ir_arg.hpp"
#include <vector>

namespace ir::statement {

enum statement_code {
  FUNCTION,
  MOVE,
  TEST,
  CALL,
  LABEL,
  JUMP,
  RETURN,
  ALLOCATE,
  DEALLOCATE,
  ADDRESS,
  LOAD,
  STORE,
  CAST,
  BIF,
  ERROR,
  LOR, // Logical Or          -> '||'
  LND, // Logical And         -> '&&'
  BOR, // Bitwise Or          -> '|'
  BND, // Bitwise And         -> '&'
  BXR, // Bitwise Xor         -> '^'
  EEQ, // Equality            -> '=='
  IEQ, // Inequality          -> '!='
  LES, // Less than           -> '<'
  GRT, // Greater than        -> '>'
  LEQ, // Less or Equals      -> '<='
  GEQ, // Greater or Equals   -> '>='
  SHL, // Shift left          -> '<<'
  SHR, // Shift right         -> '>>'
  ADD, // Add                 -> '+'
  SUB, // Subtract            -> '-'
  MUL, // Multiply            -> '*'
  DIV, // Division            -> '/'
  REM, // Modulo Division     -> '%'
  ERR=-1
};

class statement {
public:
  static enum statement_code code;
  statement* Next = nullptr;
};

class error: virtual public statement {
public:
  enum statement_code code = ERROR;
};

class function: virtual public statement {
public:
  enum statement_code code = FUNCTION;
  ir::arg::type* type;
  std::string name;
  int arity;
  statement* first;
  function(ErlNifEnv*,const ERL_NIF_TERM*);
};

class address: virtual public statement {
public:
  enum statement_code code = ADDRESS;
  ir::arg::memory* src;
  ir::arg::reg* dest;
  address(ErlNifEnv*,const ERL_NIF_TERM*);
};

class allocate: virtual public statement {
public:
  enum statement_code code = ALLOCATE;
  int bits;
  allocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class call: virtual public statement {
public:
  enum statement_code code = CALL;
  function* fn;
  int arity;
  call(ErlNifEnv*,const ERL_NIF_TERM*);
  // Currently we need this but I plan to change the way that args are done
  ir::arg::stack* first_arg;
};

class cast: virtual public statement {
public:
  enum statement_code code = CAST;
  ir::arg::reg* reg;
  ir::arg::type* type;
  cast(ErlNifEnv*,const ERL_NIF_TERM*);
};

class deallocate: virtual public statement {
public:
  enum statement_code code = DEALLOCATE;
  int bits;
  deallocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class jump: virtual public statement {
public:
  enum statement_code code = JUMP;
  ir::arg::label* lbl;
  jump(ErlNifEnv*,const ERL_NIF_TERM*);

};

class label: virtual public statement {
public:
  enum statement_code code = LABEL;
  label(ErlNifEnv*,const ERL_NIF_TERM*);
  int number;
};

class load: virtual public statement {
public:
  enum statement_code code = LOAD;
  ir::arg::memory* src;
  ir::arg::reg* dest;
  load(ErlNifEnv*,const ERL_NIF_TERM*);
};

class move: virtual public statement {
public:
  enum statement_code code = MOVE;
  ir::arg::data* src;
  ir::arg::memory* dest;
  move(ErlNifEnv*,const ERL_NIF_TERM*);
};

class rtn: virtual public statement {
public:
  enum statement_code code = RETURN;
  rtn();
};

class store: virtual public statement {
public:
  enum statement_code code = STORE;
  ir::arg::reg* src;
  ir::arg::reg* dest;
  store(ErlNifEnv*,const ERL_NIF_TERM*);
};

class test: virtual public statement {
public:
  enum statement_code code = TEST;
  ir::arg::reg* reg;
  ir::arg::label* lbl;
  label* branch;
  test(ErlNifEnv*,const ERL_NIF_TERM*);
};

class bif: virtual public statement {
public:
  enum statement_code code;
  ir::arg::reg* a;
  ir::arg::reg* b;
  ir::arg::reg* dest;
  bif(ErlNifEnv*,const ERL_NIF_TERM*,char*);
};

statement* factory(ErlNifEnv*, ERL_NIF_TERM);

}

#endif
