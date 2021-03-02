
#include "ir_statement.hpp"
#include <erl_nif.h>

namespace ir::statement {
address::address(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
allocate::allocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
call::call(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
cast::cast(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
deallocate::deallocate(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
function::function(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
jump::jump(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
label::label(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
load::load(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
move::move(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
store::store(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
test::test(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
lor::lor(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
lnd::lnd(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
bor::bor(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
bnd::bnd(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
bxr::bxr(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
eeq::eeq(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
ieq::ieq(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
les::les(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
grt::grt(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
leq::leq(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
geq::geq(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
shl::shl(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
shr::shr(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
add::add(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
sub::sub(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
mul::mul(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
div::div(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}
rem::rem(ErlNifEnv* Env,const ERL_NIF_TERM* Elems) {

}


statement* factory(ErlNifEnv* Env, ERL_NIF_TERM St) {
  if (enif_is_atom(Env,St)) return new rtn();
  int Arity;
  const ERL_NIF_TERM* Elems;
  if (!enif_get_tuple(Env,St,&Arity,&Elems)) return nullptr;
  unsigned Length;
  char* Atom;
  if (!enif_get_atom_length(Env,Elems[0],&Length,ERL_NIF_LATIN1) ||
      !enif_get_atom(Env,Elems[0],Atom,Length,ERL_NIF_LATIN1)) return nullptr;

  // Ignore this I know it's nasty sorry
  switch (Atom[0]) {
    case 'a': switch (Atom[1]) {
      case 'd': return new address(Env,Elems+1);
      case 'l': return new allocate(Env,Elems+1);
      default: return nullptr;
    }
    case 'c': switch (Atom[2]) {
      case 'l': return new call(Env,Elems+1);
      case 's': return new cast(Env,Elems+1);
      default: return nullptr;
    }
    case 'd': return new deallocate(Env,Elems+1);
    case 'f': return new function(Env,Elems+1);
    case 'j': return new jump(Env,Elems+1);
    case 'l': switch (Atom[1]) {
      case 'a': return new label(Env,Elems+1);
      case 'o': return new load(Env,Elems+1);
      default: return nullptr;
    }
    case 'm': return new move(Env,Elems+1);
    case 's': return new store(Env,Elems+1);
    case 't': return new test(Env,Elems+1);
    case '|': switch (Elems[1]) {
      case '\0': return new bor(Env,Elems+1);
      case '|':  return new lor(Env,Elems+1);
      default: return nullptr;
    }
    case '&': switch (Elems[1]) {
      case '\0': return new bnd(Env,Elems+1);
      case '&':  return new lnd(Env,Elems+1);
      default: return nullptr;
    }
    case '^': return new bxr(Env,Elems+1);
    case '=': return new eeq(Env,Elems+1);
    case '!': return new ieq(Env,Elems+1);
    case '<': switch (Elems[1]) {
      case '\0': return new les(Env,Elems+1);
      case '<':  return new shl(Env,Elems+1);
      case '=':  return new leq(Env,Elems+1);
    }
    case '>': switch (Elems[1]) {
      case '\0': return new grt(Env,Elems+1);
      case '>':  return new shr(Env,Elems+1);
      case '=':  return new geq(Env,Elems+1);
      default: return nullptr;
    }
    case '+': return new add(Env,Elems+1);
    case '-': return new sub(Env,Elems+1);
    case '*': return new mul(Env,Elems+1);
    case '/': return new div(Env,Elems+1);
    case '%': return new rem(Env,Elems+1);
    default: return nullptr;
  }
}
}
