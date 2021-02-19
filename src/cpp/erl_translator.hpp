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

  term* translate(ErlNifEnv* Env, ERL_NIF_TERM term);

  class atom : public virtual term {
  public:
    atom(ErlNifEnv* Env, ERL_NIF_TERM Atom) {
      unsigned _Length;
      if (!enif_get_atom_length(Env, Atom, &_Length, ERL_NIF_LATIN1)) exit(ERR_BAD_ATOM_LENGTH);
      char* _Buf = (char*)malloc(_Length+1);
      if (!enif_get_atom(Env, Atom, _Buf, _Length+1, ERL_NIF_LATIN1)) exit(ERR_BAD_ATOM_NAME);
      Name = std::string(_Buf);
      free(_Buf);
    }

    std::string type_name() {
      return "atom";
    }

    std::string to_string() {
      return Name;
    }

  private:
    std::string Name;
  };

  class erl_float : public virtual term {
  public:
    erl_float(ErlNifEnv* Env, ERL_NIF_TERM Dbl) {
      if (!enif_get_double(Env, Dbl, &Value)) exit(ERR_BAD_FLOAT);
    }
    std::string type_name() {
      return "float";
    }
    std::string to_string() {
      return std::to_string(Value);
    }
  private:
    double Value;
  };

  class integer : public virtual term {
  public:
    integer(ErlNifEnv* Env, ERL_NIF_TERM Integer) {
      if (!enif_get_int64(Env, Integer, &Value)) exit(ERR_BAD_INT);
    }
    std::string type_name() {
      return "int";
    }
    std::string to_string() {
      return std::to_string(Value);
    }
  private:
    long Value;
  };

  class list : public virtual term {
  public:
    list(ErlNifEnv* Env, ERL_NIF_TERM Tail) {
      unsigned _Length;
      ERL_NIF_TERM _Head;
      if (!enif_get_list_length(Env, Tail, &_Length)) exit(ERR_BAD_LIST_LENGTH);
      for (unsigned I = 0; I < _Length; I++) 
        if (!enif_get_list_cell(Env, Tail, &_Head, &Tail)) exit(ERR_BAD_LIST_ELEMS);
        else Elems.push_back(translate(Env, _Head));
    }
    std::string type_name() {
      return "list";
    }
    std::string to_string() {
      std::string Rtn = "[";
      for (auto& t : Elems) {
        Rtn.append(t -> to_string());
        Rtn.push_back(',');
      }
      if (*(Rtn.end()-1) == ',') Rtn.back() = ']';
      else Rtn.push_back(']');
      return Rtn;
    }
  private:
    std::vector<term*> Elems;
  };

  class tuple : public virtual term {
  public:
    tuple(ErlNifEnv* Env, ERL_NIF_TERM Tuple) {
      int _Arity;
      const ERL_NIF_TERM* _Elems;
      if (!enif_get_tuple(Env, Tuple, &_Arity, &_Elems)) exit(ERR_BAD_TUPLE);
      for (unsigned i = 0; i < _Arity; i++)
        Elems.push_back(translate(Env, _Elems[i]));
    }
    std::string type_name() {
      return "tuple";
    }
    std::string to_string() {
      std::string Rtn = "{";
      for (auto& T : Elems) {
        Rtn.append(T -> to_string());
        Rtn.push_back(',');
      }
      if (Rtn.back() == ',') Rtn.back() = '}';
      else Rtn.push_back('}');
      return Rtn;
    }
  private:
    std::vector<term*> Elems;
  };

  term* translate(ErlNifEnv* Env, ERL_NIF_TERM Term) {
    switch (enif_term_type(Env, Term)) {
      case ERL_NIF_TERM_TYPE_ATOM:
        return new atom(Env, Term);
      case ERL_NIF_TERM_TYPE_FLOAT:
        return new erl_float(Env, Term);
      case ERL_NIF_TERM_TYPE_INTEGER:
        return new integer(Env, Term);
      case ERL_NIF_TERM_TYPE_LIST:
        return new list(Env, Term);
      case ERL_NIF_TERM_TYPE_TUPLE:
        return new tuple(Env, Term);
      default:
        return nullptr;
    }
  }
}





#endif

