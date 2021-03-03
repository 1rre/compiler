-module(type_enum).
-export([scan/1]).

%% @doc Takes tokens & replaces appropriate "identifier" tokens with "enum literal" or "typedef name" tokens.
%       It seems to handle shadowing etc. fairly well but is definitely a point of failiure
%       It could also mess up the input pretty badly on erronous imputs but is that really an issue tbh?

%% Arity 1 function to export
scan(Tokens) -> scan(Tokens, [], []).

%% At an opening brace, set up a new namespace
scan([{'{', Line} | Tokens], Types, Enums) ->
  {Adj, Rest} = scan(Tokens, Types, Enums),
  {A0, R0} = scan(Rest, Types, Enums),
  {[{'{', Line} | Adj ++ A0], R0};

%% On a typedef keyword, look for the next identifier and map it to the appropriate type
scan([{typedef, Line} | Tokens], Types, Enums) ->
  {Adj, Name, Rest} = find_ident(Tokens, Types, Enums),
  {A0, R0} = scan(Rest, [Name|Types], Enums),
  {[{typedef, Line} | Adj ++ A0], R0};

%% Check if the typedef or enum is locally shadowed by an assignment
scan([{identifier, L0, Name}, {'=', L1} | Tokens], Types, Enums) ->
  {Adj, Rest} = scan(Tokens, lists:delete(Name, Types), lists:delete(Name, Enums)),
  {[{identifier, L0, Name}, {'=', L1} | Adj], Rest};

%% Check if the typedef is locally shadowed by an assignment.
%  This won't match on typedefs or enums
scan([{identifier, L0, Name}, {';', L1} | Tokens], Types, Enums) ->
  {Adj, Rest} = scan(Tokens, lists:delete(Name, Types), lists:delete(Name, Enums)),
  {[{identifier, L0, Name}, {';', L1} | Adj], Rest};

%% Check if the identifier needs to be changed to a typedef or enum
scan([{identifier, L0, Name} | Tokens], Types, Enums) ->
  case lists:member(Name, Types) of
    true -> scan([{typedef_name, L0, Name} | Tokens], Types, Enums);
    false ->
      case lists:member(Name, Enums) of
        true -> scan([{enum_l, L0, Name} | Tokens], Types, Enums);
        false ->
          {Adj, Rest} = scan(Tokens, Types, Enums),
          {[{identifier, L0, Name} | Adj], Rest}
      end
  end;

%% Stop scanning at the end of a namespace,
%  and revert to the previous namespace for the rest of the tokens
scan([{'}', Line} | Tokens], _Types, _Enums) ->
  {[{'}', Line}], Tokens};

%% Fallback function for anything that isn't an identifier or typedef
scan([Token | Tokens], Types, Enums) ->
  {Adj, Rest} = scan(Tokens, Types, Enums),
  {[Token | Adj], Rest};

%% Once all tokens have been scanned, return an empty list
scan([], _Types, _Enums) ->
  {[], []};

%% Fallback function
scan(_Tokens, _Types, _Enums) -> error("how did this happen?").

%% Find the next identifier token
find_ident([{identifier, Line, Name} | Tokens], Types, Enums) ->
  case lists:member(Name, Types) of
    true -> find_ident([{typedef_name, Line, Name} | Tokens], Types, Enums);
    false ->
      case lists:member(Name, Enums) of
        true -> find_ident([{enum_l, Line, Name} | Tokens], Types, Enums);
        false ->
          {[{identifier, Line, Name}], Name, Tokens}
      end
  end;
find_ident([Other | Tokens], Types, Enums) ->
  {Adj, Name, Rest} = find_ident(Tokens, Types, Enums),
  {[Other | Adj], Name, Rest};
find_ident([], _, _) -> {[], n, []}.
