#ifndef IR_ARG_HPP
#define IR_ARG_HPP

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
  arg(enum arg_code Code): code(Code) {}
};

class type: public virtual arg {
public:
  type(int,char,int);
  int ref_level;
  char data_type;
  int width;
};

class data: public virtual arg {
public:
  data(enum arg_code Code): arg(Code) {}
  virtual ~data() = default;
};

class literal: public virtual data {
public:
  literal(enum arg_code Code): data(Code), arg(Code) {}
};
class floating: public virtual literal {
public:
  enum arg_code code;
  floating(double);
  double value;
};
class integer: public virtual literal {
public:
  enum arg_code code;
  integer(int);
  int value;
};

class memory: public virtual data {
public:
  int number;
  memory(int,enum arg_code Code): data(Code), arg(Code) {}
  virtual ~memory() = default;
};
class reg: public virtual memory {
public:
  enum arg_code code;
  reg(int);
};
class stack: public virtual memory {
public:
  enum arg_code code;
  stack(int);
};

class label: public virtual arg {
public:
  enum arg_code code;
  label(int);
  int number;
};

}

#endif
