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
  enum arg_code code;
};

class type: public virtual arg {
public:
  const enum arg_code code = TYPE;
  type(int,char,int);
  int ref_level;
  char data_type;
  int width;
};

class data: public virtual arg {
public:
  virtual ~data() = default;
};

class literal: public virtual data {};
class floating: public virtual literal {
public:
  enum arg_code code = FLOAT;
  floating(double);
  double value;
};
class integer: public virtual literal {
public:
  enum arg_code code = INT;
  integer(int);
  int value;
};

class memory: public virtual data {
public:
  int number;
  memory(int);
  virtual ~memory() = default;
};
class reg: public virtual memory {
public:
  enum arg_code code = REG;
  reg(int);
};
class stack: public virtual memory {
public:
  enum arg_code code = STACK;
  stack(int);
};

class label: public virtual arg {
public:
  enum arg_code code = LABEL;
  label(int);
  int number;
};

arg* factory(ErlNifEnv*, ERL_NIF_TERM);

}

#endif
