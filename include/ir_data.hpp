#ifndef IR_DATA_HPP
#define IR_DATA_HPP

#include <string>
#include <erl_nif.h>

namespace ir::arg {

enum arg_code {
  TYPE,
  FLOAT,
  INT,
  REG,
  STACK,
  LABEL
};

class arg {
public:
  static enum arg_code Code;
};

class type: public virtual arg {
public:
  enum arg_code Code = TYPE;
  type(int,char,int);
  int Ref_Level;
  char Type;
  int Width;
};

class data: public virtual arg {};

class literal: public virtual data {};
class floating: public virtual literal {
public:
  enum arg_code Code = FLOAT;
  floating(double);
  double Value;
};
class integer: public virtual literal {
public:
  enum arg_code Code = INT;
  integer(int);
  int Value;
};

class memory: public virtual data {
public:
  int Number;
};
class reg: public virtual memory {
public:
  enum arg_code Code = REG;
  reg(int);
};
class stack: public virtual memory {
public:
  enum arg_code Code = STACK;
  stack(int);
};

class label: public virtual arg {
public:
  enum arg_code Code = LABEL;
  label(int);
  int Number;
};

arg* factory(ErlNifEnv*, ERL_NIF_TERM);

}

#endif
