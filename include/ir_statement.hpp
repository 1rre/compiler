#ifndef IR_STATEMENT_HPP
#define IR_STATEMENT_HPP

#include "ir_data.hpp"
#include "ir_bif.hpp"
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
  ERROR
};

class statement {
public:
  static enum statement_code Code;
  statement* Next;
};

class error: virtual public statement {
  enum statement_code Code = ERROR;
};

class function: virtual public statement {
  enum statement_code Code = FUNCTION;
  ir::arg::type Type;
  std::string Name;
  int Arity;
  statement* First;
};

class move: virtual public statement {
  enum statement_code Code = MOVE;
  ir::arg::data* Src;
  ir::arg::memory* Dest;
};

class call: virtual public statement {
  enum statement_code Code = CALL;
  function* Function;
  int Arity;
  // Currently we need this but I plan to change the way that args are done
  ir::arg::stack* First_Arg;
};

class label: virtual public statement {
  enum statement_code Code = LABEL;
  // I don't think we need anything else?
};

class test: virtual public statement {
  enum statement_code Code = TEST;
  ir::arg::reg* Reg;
  label* Branch;
};

class jump: virtual public statement {
  enum statement_code Code = JUMP;
  ir::arg::reg* Reg;
  // Hopefully this overrides the inherited `statement* Next`
  label* Next;
};

class rtn: virtual public statement {
  enum statement_code Code = RETURN;
  // I don't think we need anything else?
};

class allocate: virtual public statement {
  enum statement_code Code = ALLOCATE;
  int Bits;
};

class deallocate: virtual public statement {
  enum statement_code Code = DEALLOCATE;
  int Bits;
};

class address: virtual public statement {
  enum statement_code Code = ADDRESS;
  ir::arg::memory* Src;
  ir::arg::reg* Dest;
};

class load: virtual public statement {
  enum statement_code Code = LOAD;
  ir::arg::memory* Src;
  ir::arg::reg* Dest;

};

class store: virtual public statement {
  enum statement_code Code = STORE;
  ir::arg::reg* Src;
  ir::arg::reg* Dest;
};

class cast: virtual public statement {
  enum statement_code Code = CAST;
  ir::arg::reg* Reg;
  ir::arg::type Type;
};

class bif: virtual public statement {
  enum statement_code Code = BIF;
  ir::bif::op Op;
};

}

#endif
