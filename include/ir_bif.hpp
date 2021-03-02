#ifndef IR_BIF_HPP
#define IR_BIF_HPP

namespace ir::bif {
enum op_code {
  LOR, // Logical Or          -> '||'
  LND, // Logical And         -> '&&'
  BOR, // Bitwise Or          -> '|'
  BND, // Bitwise And         -> '&'
  BXR, // Bitwise Xor         -> '^'
  EEQ, // Equality            -> '=='
  IEQ, // Inequality          -> '!='
  LST, // Less than           -> '<'
  GTT, // Greater than        -> '>'
  LET, // Less or Equals      -> '<='
  GET, // Greater or Equals   -> '>='
  SHL, // Shift left          -> '<<'
  SHR, // Shift right         -> '>>'
  ADD, // Add                 -> '+'
  SUB, // Subtract            -> '-'
  MUL, // Multiply            -> '*'
  DIV, // Division            -> '/'
  REM  // Modulo Division     -> '%'
};

class op {
  static enum op_code Code;
  op(char*);
};

}

#endif
