#ifndef TYPE_CONTEXT_HPP
#define TYPE_CONTEXT_HPP
#include <map>
#include <erl_nif.h>
#include "erl_translator.hpp"

namespace context {
  static std::map<ast::atom, ast::term*> Typedef;
  static std::map<ast::atom, ast::term*> Enum;
  static std::map<ast::atom, ast::term*> Var;
  static std::map<ast::atom, ast::term*> Fun;
}
#endif
