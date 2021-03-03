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

struct arg_type {
  int ref_level;
  char type;
  int width;

  arg_type(int P, char T, int S):
    ref_level(P),
    type(T),
    width(S) {}
};

union arg_value {
  arg_type type;
  double floating;
  int integer;
  int number;
  
  arg_value(): number(0) {}
};

class arg {
public:
  static enum arg_code code;
  arg_value value;
};

class type: public virtual arg {
public:
  enum arg_code code = TYPE;
  type(int,char,int);
};

class data: public virtual arg {};

class literal: public virtual data {};
class floating: public virtual literal {
public:
  enum arg_code code = FLOAT;
  floating(double);
};
class integer: public virtual literal {
public:
  enum arg_code code = INT;
  integer(int);
};

class memory: public virtual data {};
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
};

}

#endif
