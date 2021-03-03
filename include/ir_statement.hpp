#ifndef IR_STATEMENT_HPP
#define IR_STATEMENT_HPP

#include "ir_data.hpp"
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
  static enum statement_code Code;
  statement* Next = nullptr;
};

class error: virtual public statement {
public:
  enum statement_code Code = ERROR;
};

class function: virtual public statement {
public:
  enum statement_code Code = FUNCTION;
  ir::arg::type* Type;
  std::string Name;
  int Arity;
  statement* First;
  function(ErlNifEnv*,const ERL_NIF_TERM*);
};

class move: virtual public statement {
public:
  enum statement_code Code = MOVE;
  ir::arg::data* Src;
  ir::arg::memory* Dest;
  move(ErlNifEnv*,const ERL_NIF_TERM*);
};

class call: virtual public statement {
public:
  enum statement_code Code = CALL;
  function* Function;
  int Arity;
  call(ErlNifEnv*,const ERL_NIF_TERM*);
  // Currently we need this but I plan to change the way that args are done
  ir::arg::stack* First_Arg;
};

class label: virtual public statement {
public:
  enum statement_code Code = LABEL;
  label(ErlNifEnv*,const ERL_NIF_TERM*);
};

class test: virtual public statement {
public:
  enum statement_code Code = TEST;
  ir::arg::reg* Reg;
  label* Branch;
  test(ErlNifEnv*,const ERL_NIF_TERM*);
};

class jump: virtual public statement {
public:
  enum statement_code Code = JUMP;
  ir::arg::reg* Reg;
  jump(ErlNifEnv*,const ERL_NIF_TERM*);

};

class rtn: virtual public statement {
public:
  enum statement_code Code = RETURN;
  rtn();
};

class allocate: virtual public statement {
public:
  enum statement_code Code = ALLOCATE;
  int Bits;
  allocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class deallocate: virtual public statement {
public:
  enum statement_code Code = DEALLOCATE;
  int Bits;
  deallocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class address: virtual public statement {
public:
  enum statement_code Code = ADDRESS;
  ir::arg::memory* Src;
  ir::arg::reg* Dest;
  address(ErlNifEnv*,const ERL_NIF_TERM*);
};

class load: virtual public statement {
public:
  enum statement_code Code = LOAD;
  ir::arg::memory* Src;
  ir::arg::reg* Dest;
  load(ErlNifEnv*,const ERL_NIF_TERM*);
};

class store: virtual public statement {
public:
  enum statement_code Code = STORE;
  ir::arg::reg* Src;
  ir::arg::reg* Dest;
  store(ErlNifEnv*,const ERL_NIF_TERM*);
};

class cast: virtual public statement {
public:
  enum statement_code Code = CAST;
  ir::arg::reg* Reg;
  ir::arg::type* Type;
  cast(ErlNifEnv*,const ERL_NIF_TERM*);
};

class bif: virtual public statement {
public:
  enum statement_code Code;
  ir::arg::reg* A;
  ir::arg::reg* B;
  ir::arg::reg* Dest;
  bif(ErlNifEnv*,const ERL_NIF_TERM*,char*);
};

statement* factory(ErlNifEnv*, ERL_NIF_TERM);

}

#endif
