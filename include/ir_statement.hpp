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
  struct fn_params {
    ir::arg::type type;
    std::string name;
    int arity;
    statement* first = nullptr;

    fn_params(int P,char T,int S,char* Name,int Arity):
      type(P,T,S),
      name(Name),
      arity(Arity) {}
  };
  struct mem_reg_params {
    ir::arg::memory src;
    ir::arg::reg dest;
  };
  struct data_mem_params {
    ir::arg::data src;
    ir::arg::memory dest;
  };
  struct reg_reg_params {
    ir::arg::reg src;
    ir::arg::reg dest;
  };
  struct call_params {
    statement* function = nullptr;
    int arity;
    ir::arg::stack first;
  };
  struct cast_params {
    ir::arg::reg reg;
    ir::arg::type type;
  };
  struct test_params {
    ir::arg::reg reg;
    ir::arg::label label;
    statement* branch = nullptr;
  };
  struct bif_params {
    ir::arg::reg a;
    ir::arg::reg b;
    ir::arg::reg dest;
  };
  union statement_value {
    fn_params fn;
    mem_reg_params mem_reg;
    data_mem_params data_mem;
    reg_reg_params reg_reg;
    call_params call;
    cast_params cast;
    test_params test;
    bif_params bif;
    ir::arg::label label;
    int number;
    int bits;

    statement_value() {
      number = 0;
    }
  };
  static enum statement_code code;
  statement* next = nullptr;
  statement_value value;
};


class error: virtual public statement {
public:
  enum statement_code code = ERROR;
};

class function: virtual public statement {
public:
  enum statement_code code = FUNCTION;
  function(ErlNifEnv*,const ERL_NIF_TERM*);
};

class address: virtual public statement {
public:
  enum statement_code code = ADDRESS;
  address(ErlNifEnv*,const ERL_NIF_TERM*);
};

class allocate: virtual public statement {
public:
  enum statement_code code = ALLOCATE;
  allocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class call: virtual public statement {
public:
  enum statement_code code = CALL;
  call(ErlNifEnv*,const ERL_NIF_TERM*);
};

class cast: virtual public statement {
public:
  enum statement_code code = CAST;
  cast(ErlNifEnv*,const ERL_NIF_TERM*);
};

class deallocate: virtual public statement {
public:
  enum statement_code code = DEALLOCATE;
  deallocate(ErlNifEnv*,const ERL_NIF_TERM*);
};

class jump: virtual public statement {
public:
  enum statement_code code = JUMP;
  jump(ErlNifEnv*,const ERL_NIF_TERM*);

};

class label: virtual public statement {
public:
  enum statement_code code = LABEL;
  label(ErlNifEnv*,const ERL_NIF_TERM*);
};

class load: virtual public statement {
public:
  enum statement_code code = LOAD;
  load(ErlNifEnv*,const ERL_NIF_TERM*);
};

class move: virtual public statement {
public:
  enum statement_code code = MOVE;
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
  store(ErlNifEnv*,const ERL_NIF_TERM*);
};

class test: virtual public statement {
public:
  enum statement_code code = TEST;
  test(ErlNifEnv*,const ERL_NIF_TERM*);
};

class bif: virtual public statement {
public:
  enum statement_code code;
  bif(ErlNifEnv*,const ERL_NIF_TERM*,char*);
};

statement* factory(ErlNifEnv*, ERL_NIF_TERM);

}

#endif
