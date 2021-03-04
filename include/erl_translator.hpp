#ifndef IR_TRANSLATE_HPP
#define IR_TRANSLATE_HPP

#include <erl_nif.h>
#include "ir_statement.hpp"

namespace ir {

enum erl_error {
  ERR_BAD_ATOM_LENGTH,
  ERR_BAD_ATOM_NAME,
  ERR_BAD_FLOAT,
  ERR_BAD_INT,
  ERR_BAD_LIST_LENGTH,
  ERR_BAD_LIST_ELEMS,
  ERR_BAD_TUPLE
};

std::vector<statement::statement*> get_ir(ErlNifEnv*, ERL_NIF_TERM);

}
#endif
