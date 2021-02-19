#ifndef ERL_TRANSLATOR
#define ERL_TRANSLATOR

#include <erl_nif.h>
#include <vector>
#include <string>
#include <stdlib.h>

namespace ast { 
  enum enif_error {
    ERR_BAD_ATOM_LENGTH=1,
    ERR_BAD_ATOM_NAME,
    ERR_BAD_FLOAT,
    ERR_BAD_INT,
    ERR_BAD_LIST_LENGTH,
    ERR_BAD_LIST_ELEMS,
    ERR_BAD_TUPLE
  };

  class term {
    public:
    virtual std::string type_name() { return ""; }
    virtual std::string to_string() { return ""; }
  };

  term* translate(ErlNifEnv* env, ERL_NIF_TERM term);

  class atom : public virtual term {
  public:
    atom(ErlNifEnv* env, ERL_NIF_TERM atom) {
      unsigned _length;
      if (!enif_get_atom_length(env, atom, &_length, ERL_NIF_LATIN1)) exit(ERR_BAD_ATOM_LENGTH);
      char* _buf = (char*)malloc(_length+1);
      if (!enif_get_atom(env, atom, _buf, _length+1, ERL_NIF_LATIN1)) exit(ERR_BAD_ATOM_NAME);
      name = std::string(_buf);
      free(_buf);
    }

    std::string type_name() {
      return "atom";
    }

    std::string to_string() {
      return name;
    }

  private:
    std::string name;
  };

  class erl_float : public virtual term {
  public:
    erl_float(ErlNifEnv* env, ERL_NIF_TERM dbl) {
      if (!enif_get_double(env, dbl, &value)) exit(ERR_BAD_FLOAT);
    }
    std::string type_name() {
      return "float";
    }
    std::string to_string() {
      return std::to_string(value);
    }
  private:
    double value;
  };

  class integer : public virtual term {
  public:
    integer(ErlNifEnv* env, ERL_NIF_TERM integer) {
      if (!enif_get_int64(env, integer, &value)) exit(ERR_BAD_INT);
    }
    std::string type_name() {
      return "int";
    }
    std::string to_string() {
      return std::to_string(value);
    }
  private:
    long value;
  };

  class list : public virtual term {
  public:
    list(ErlNifEnv* env, ERL_NIF_TERM tail) {
      unsigned _length;
      ERL_NIF_TERM _head;
      if (!enif_get_list_length(env, tail, &_length)) exit(ERR_BAD_LIST_LENGTH);
      for (unsigned i = 0; i < _length; i++) 
        if (!enif_get_list_cell(env, tail, &_head, &tail)) exit(ERR_BAD_LIST_ELEMS);
        else elems.push_back(translate(env, _head));
    }
    std::string type_name() {
      return "list";
    }
    std::string to_string() {
      std::string rtn = "[";
      for (auto& t : elems) {
        rtn.append(t -> to_string());
        rtn.push_back(',');
      }
      if (*(rtn.end()-1) == ',') rtn.back() = ']';
      else rtn.push_back(']');
      return rtn;
    }
  private:
    std::vector<term*> elems;
  };

  class tuple : public virtual term {
  public:
    tuple(ErlNifEnv* env, ERL_NIF_TERM tuple) {
      int _arity;
      const ERL_NIF_TERM* _elems;
      if (!enif_get_tuple(env, tuple, &_arity, &_elems)) exit(ERR_BAD_TUPLE);
      for (unsigned i = 0; i < _arity; i++)
        elems.push_back(translate(env, _elems[i]));
    }
    std::string type_name() {
      return "tuple";
    }
    std::string to_string() {
      std::string rtn = "{";
      for (auto& t : elems) {
        rtn.append(t -> to_string());
        rtn.push_back(',');
      }
      if (rtn.back() == ',') rtn.back() = '}';
      else rtn.push_back('}');
      return rtn;
    }
  private:
    std::vector<term*> elems;
  };

  term* translate(ErlNifEnv* env, ERL_NIF_TERM term) {
    switch (enif_term_type(env, term)) {
      case ERL_NIF_TERM_TYPE_ATOM:
        return new atom(env, term);
      case ERL_NIF_TERM_TYPE_FLOAT:
        return new erl_float(env, term);
      case ERL_NIF_TERM_TYPE_INTEGER:
        return new integer(env, term);
      case ERL_NIF_TERM_TYPE_LIST:
        return new list(env, term);
      case ERL_NIF_TERM_TYPE_TUPLE:
        return new tuple(env, term);
      default:
        return nullptr;
    }
  }
}





#endif

