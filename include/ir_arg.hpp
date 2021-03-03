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

class type: public arg {
public:
  type(int,char,int);
  int ref_level;
  char data_type;
  int width;
};

class data: public arg {
public:
  data(enum arg_code Code): arg(Code) {}
  ~data() = default;
};

class literal: public data {
public:
  literal(enum arg_code Code): data(Code) {}
};
class floating: public literal {
public:
  enum arg_code code;
  floating(double);
  double value;
};
class integer: public literal {
public:
  enum arg_code code;
  integer(int);
  int value;
};

class memory: public data {
public:
  int number;
  memory(int Num,enum arg_code Code): data(Code), number(Num) {}
};
class reg: public memory {
public:
  enum arg_code code;
  reg(int);
};
class stack: public memory {
public:
  enum arg_code code;
  stack(int);
};

class label: public arg {
public:
  enum arg_code code;
  label(int);
  int number;
};

}

#endif
