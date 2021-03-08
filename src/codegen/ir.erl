-module(ir).
-export([generate/1]).

-record(state,{lbcnt=0,rvcnt=0,lvcnt=0,fn=#{},sizeof=#{},
               var=#{},typedef=#{},struct=#{},enum=#{},typecheck=#{}}).

-include("arch_type_consts.hrl").

%% @doc Takes a slightly convoluted & complex AST and compiles them into a more human-readable form.
%       This form is also a lot closer to the target language (MIPS ASM) than an AST.

%% Arity 1 function for default arguments
generate(Ast) ->
  generate(Ast, #state{}).

%% On a leaf node of the AST, return the current state
generate([], State) -> {ok, State, []};

%% For a node with branches, process the node, then the branch & return the merged IR code
generate([Hd|Tl], State) ->
  {ok, Hd_State, Hd_St} = generate(Hd,State),
  {ok, Tl_State, Tl_St} = generate(Tl,Hd_State),
  {ok,Tl_State,Hd_St++Tl_St};

%% Process a declaration of a function by adding specification about it to the state &
%  processing the branches of the function node (arguments & statement list)
generate({function,{Raw_Type,{Raw_Ident,Raw_Args},Raw_St}}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  {ok, Ident, _Ptr_Ident, Arr} = get_ident_specs(Raw_Ident,State),
  if Arr =/= [] -> error({return_type,array});
  true -> ok end,
  {ok, Arg_State, Arg_St} = generate(Raw_Args, State),
  Arity = length(Raw_Args),
  Alloc_St = lists:flatten([[{allocate,?SIZEOF_INT},{move,{z,N},{y,N}}] || N <- lists:seq(0,Arity-1)]),
  New_Fn = maps:put(Ident,{Type,length(Raw_Args)},Arg_State#state.fn),
  {ok, N_State, N_St} = generate(Raw_St,Arg_State#state{fn=New_Fn}),
  Rtn_St = case lists:last(N_St) of
    return ->
      [{function,Type,Ident,length(Raw_Args),Alloc_St++N_St}];
    _ ->
      {ok, Dealloc} = deallocate_mem(#{},N_State#state.var),
      [{function,Type,Ident,length(Raw_Args),Alloc_St++N_St++[Dealloc,return]}]
  end,
  {ok,copy_lbcnt(N_State,State#state{fn=New_Fn}),Rtn_St};

%% As there are multiple cases for declaration, it is delegated to a helper function.
generate({declaration,Raw_Type,Raw_St}, State) ->
  {ok, Type} = get_type(Raw_Type, State),
  get_decl_specs(Type,Raw_St,State);

%% Process an integer node by moving the literal value to the active register
%  As longs are out of spec we currently don't differentiate however it may be useful to later
%% TODO: N/A
%        Add unsigned
generate({int_l,_Line,Val,[]}, State) ->
  Lv_Cnt = State#state.lvcnt,
  N_Types = maps:put({x,Lv_Cnt},{0,i,?SIZEOF_INT},State#state.typecheck),
  {ok,State#state{typecheck=N_Types},[{move,{i,Val},{x,Lv_Cnt}}]};
generate({int_l,_Line,Val,[$l]}, State) ->
  Lv_Cnt = State#state.lvcnt,
  N_Types = maps:put({x,Lv_Cnt},{0,i,?SIZEOF_LONG},State#state.typecheck),
  {ok,State#state{typecheck=N_Types},[{move,{i,Val},{x,Lv_Cnt}}]};

%% Process a float node by moving the literal value to the active register
%  As doubles are out of spec we currently don't support these however it may be useful to later
generate({float_l,_Line,Val,_Suffix}, State) ->
  Lv_Cnt = State#state.lvcnt,
  N_Types = maps:put({x,Lv_Cnt},{0,f,?SIZEOF_FLOAT},State#state.typecheck),
  {ok,State#state{typecheck=N_Types},[{move,{f,Val},{x,Lv_Cnt}}]};

%% Process an identifier by finding the integer's location on the stack
%  and moving it to the active register
generate({identifier,Ln,Ident}, State) ->
  case maps:get(Ident,State#state.var,undefined) of
    {Type, X} ->
      Lv_Cnt = State#state.lvcnt,
      N_Types = maps:put({x,Lv_Cnt},Type,State#state.typecheck),
      {ok,State#state{typecheck=N_Types},[{move,X,{x,Lv_Cnt}}]};
    Other -> error({Other,Ident,{line,Ln},{state,State}})
  end;

generate({Rest,{array, Offset}}, State) ->
  Lv_Cnt = State#state.lvcnt,
  {ok,Ptr_State,Ptr_St} = generate(Rest,State),
  {ok,Off_State,Off_St} = generate(Offset,State#state{lvcnt=Lv_Cnt+1}),
  Rtn_St = Ptr_St++Off_St++[{'+',[{x,Lv_Cnt},{x,Lv_Cnt+1}],{x,Lv_Cnt}},
                            {load,{x,Lv_Cnt},{x,Lv_Cnt}}],
  {ok,copy_lvcnt(Off_State,Ptr_State),Rtn_St};

%% Process a function call by storing the current register state on the stack,
% storing the arguments to the function on the stack,
% calling the function, then restoring the register state.
%% TODO: N/A
%        Debug arg processing order; Potentially it is in the wrong order.
generate({{identifier,Ln,Ident},{apply,Args}}, State) ->
  {ok,Arg_State,Arg_St} = lists:foldl(fun
    (Arg,{ok,Acc_State,Acc_St}) ->
      {ok,Acc_Rtn_State,Acc_Rtn_St} = generate(Arg,Acc_State),
      Lv_Cnt = Acc_State#state.lvcnt,
      {ok,copy_lbcnt(Acc_Rtn_State,Acc_State#state{lvcnt=Lv_Cnt+1}),Acc_St++Acc_Rtn_St}
  end, {ok,State,[]}, Args),
  case maps:get(Ident,Arg_State#state.fn, undefined) of
    {Type, Arity} when Arity =:= length(Args) ->
      Lv_Cnt = State#state.lvcnt,
      Rv_Cnt = State#state.rvcnt,
      Alloc_St = [{allocate,?SIZEOF_INT} || _ <- lists:seq(0,Lv_Cnt-1)],
      Mv_To_St = [{move,{x,N},{y,Rv_Cnt+N}} || N <- lists:seq(0,Lv_Cnt-1)],
      To_A_St = [{move,{x,Lv_Cnt+N},{z,N}} || N <- lists:seq(0,Arity-1)],
      Call_St = {call,Ident,Arity},
      Mv_Bk_St = [{move,{y,Rv_Cnt+N},{x,N}} || N <- lists:seq(0,Lv_Cnt-1)],
      New_St = if
        Lv_Cnt =:= 0 -> Arg_St++Alloc_St++Mv_To_St++To_A_St++[Call_St|Mv_Bk_St];
        true ->
          Dealloc_St = {gc,Rv_Cnt},
          Mv_0_St = {move,{x,0},{x,Lv_Cnt}},
          Arg_St++Alloc_St++Mv_To_St++To_A_St++[Call_St,Mv_0_St|Mv_Bk_St]++[Dealloc_St]
      end,
      N_Types = maps:put({x,Lv_Cnt},Type,State#state.typecheck),
      {ok,copy_lbcnt(Arg_State,State#state{typecheck=N_Types}),New_St};
    Other ->
      error({Other, Ident, {args, Args}, {line, Ln}, {state, State}})
  end;

generate({sizeof,Expr},State) ->
  Lv_Cnt = State#state.lvcnt,
  case get_type(Expr,State) of
    {ok,{0,_,S}} -> {ok,State,[{move,{i,S div 8},{x,Lv_Cnt}}]};
    {ok,_} -> {ok,State,[{move,{i,?SIZEOF_POINTER div 8},{x,Lv_Cnt}}]};
    _ ->
      {ok,Expr_State,Expr_St} = generate(Expr, State),
      Size = case lists:last(Expr_St) of
        {move,{y,Rv},{x,Lv}} ->
          maps:get({y,Rv},Expr_State#state.sizeof,undefined);
        _ ->
          case maps:get({x,Lv_Cnt},Expr_State#state.typecheck,{0,n,0}) of
            {0,_,S} -> S;
            R -> ?SIZEOF_POINTER
          end
        end,
      {ok,State,[{move,{i,Size div 8},{x,Lv_Cnt}}]}
  end;

%% As there are multiple cases for assignment, it is delegated to a helper function.
generate({assign,{Op,_Ln},Raw_Specs}, State) ->
  get_assign_specs(Op,Raw_Specs,State);

%% ++ and -- for both prefix and postfix operators
generate({Rest,{increment,Op,{_,Ln}}}, State) ->
  get_assign_specs('=',[Rest,{bif,Op,[Rest,{int_l,Ln,1,[]}]}], State);
generate({{increment,Op,{_,Ln}},Rest}, State) ->
  get_assign_specs('=',[Rest,{bif,Op,[Rest,{int_l,Ln,1,[]}]}], State);

%% Process a return value by deallocating any memory used (and args),
%  processing the expression which calculates the value to return
%  and storing it in the active register (which should always be 0)
generate({{return,_},Raw_St}, State) ->
  {ok, Rtn_State, Rtn_St} = generate(Raw_St, State),
  {ok, Dealloc} = deallocate_mem(#{},Rtn_State#state.var),
  {ok, Rtn_State#state{lvcnt=0,rvcnt=0}, Rtn_St++[Dealloc,return]};

%% Process an if statement by processing each of the predicate, the "if true" statement
%  and the "if false" statement, then controlling the PC flow with jumps.
%  For simple 'if's, only the "if true" statement is returned, and for 'if/else'
%  both are returned.
generate({{'if',_},Predicate,True,False}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Lv_Cnt = State#state.lvcnt,
  {ok,If_State,If_St} = generate(Predicate,State#state{lbcnt=Lb_Cnt+2}),
  Test_State = If_State#state{lvcnt=Lv_Cnt},
  Test_Jump = {test,{x,Lv_Cnt},{l,Lb_Cnt+1}},
  {ok,True_State,True_St} = generate(True,Test_State),
  {ok,True_Dealloc} = deallocate_mem(Test_State#state.var,True_State#state.var),
  False_Label = {label,Lb_Cnt+1},
  {ok,False_State,False_St} = generate(False,copy_lbcnt(True_State,Test_State)),
  {ok,False_Dealloc} = deallocate_mem(Test_State#state.var,False_State#state.var),
  Rtn_State = copy_lbcnt(False_State,State),
  if
    False_St =:= [] ->
      Rtn_St = If_St++[Test_Jump|True_St]++[True_Dealloc,False_Label],
      {ok,Rtn_State,Rtn_St};
    true ->
      True_Jump = {jump,{l,Lb_Cnt+2}},
      True_Label = {label,Lb_Cnt+2},
      Rtn_St = If_St++[Test_Jump|True_St]++[True_Dealloc,True_Jump,False_Label|False_St]++[False_Dealloc,True_Label],
      {ok,Rtn_State,Rtn_St}
  end;

%% Process a while loop by processing each of the predicate and the loop body,
%  having the PC jump to beyond the end of the loop if the predicate evaluates to zero
%  and having an unconditional jump to before the predicate at the end of the loop.
generate({{while,_},Predicate,Do}, State) ->
  Lb_Cnt = State#state.lbcnt + 1,
  Start_Label = {label,Lb_Cnt},
  {ok,Pred_State,Pred_St} = generate(Predicate,State#state{lbcnt=Lb_Cnt+1}),
  Test_Jump = {test,{x,State#state.lvcnt},{l,Lb_Cnt+1}},
  {ok,Do_State,Do_St} = generate(Do,copy_lvcnt(State,Pred_State)),
  {ok,Do_Dealloc} = deallocate_mem(Pred_State#state.var,Do_State#state.var),
  Jump = {jump,{l,Lb_Cnt}},
  End_Label = {label,Lb_Cnt + 1},
  Rtn_State = copy_lbcnt(Do_State,State),
  Rtn_St = [Start_Label|Pred_St] ++ [Test_Jump|Do_St] ++ [Do_Dealloc,Jump,End_Label],
  {ok,Rtn_State,Rtn_St};

%% Process a do while loop by processing each of the loop body and the predicate
%  and having the PC jump back to the start of the loop if the predicate,
%  which is evaluated after the loop body, evaluates to non-zero.
generate({{do,_},Do,Predicate}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Start_Label = {label,Lb_Cnt + 1},
  {ok,Do_State,Do_St} = generate(Do,State#state{lbcnt=Lb_Cnt+2}),
  {ok,Do_Dealloc} = deallocate_mem(State#state.var,Do_State#state.var),
  {ok,Pred_State,Pred_St} = generate(Predicate,copy_lbcnt(Do_State,State)),
  Test_Jump = {test,{x,State#state.lvcnt},{l,Lb_Cnt+2}},
  Jump = {jump,{l,Lb_Cnt+1}},
  End_Label = {label,Lb_Cnt + 2},
  Rtn_State = copy_lbcnt(Pred_State,State),
  Rtn_St = [Start_Label|Do_St] ++ [Do_Dealloc|Pred_St] ++ [Test_Jump,Jump,End_Label],
  {ok,Rtn_State,Rtn_St};

%% Process a for loop by processing the initialiser,
%  creating a snapshot of the state & then using this as a root state to process
%  each of the predicate, loop body and the 'update' statement.
%  If the predicate evaluates to zero, the PC jumps beyond the end of the loop.
generate({{for,_},{Init,Predicate,Update},Loop}, State) ->
  Lb_Cnt = State#state.lbcnt,
  Lv_Cnt = State#state.lvcnt,
  {ok,Init_State,Init_St} = generate(Init,State#state{lbcnt=Lb_Cnt+2}),
  Root_State = Init_State#state{lvcnt=Lv_Cnt},
  Pred_Label = {label,Lb_Cnt+1},
  {ok,Pred_State,Pred_St} = generate(Predicate,Root_State),
  Pred_Test = {test,{x,Lv_Cnt},{l,Lb_Cnt+2}},
  {ok,Loop_State,Loop_St} = generate(Loop,copy_lbcnt(Pred_State,Root_State)),
  {ok,Dealloc} = deallocate_mem(Init_State#state.var,Loop_State#state.var),
  {ok,Update_State,Update_St} = generate(Update,copy_lbcnt(Loop_State,Root_State)),
  Jump = {jump,{l,Lb_Cnt+1}},
  End_Label = {label,Lb_Cnt+2},
  Next_State = copy_lbcnt(Update_State,State),
  Next_St = Init_St ++ [Pred_Label|Pred_St] ++ [Pred_Test|Loop_St] ++ [Dealloc|Update_St] ++ [Jump,End_Label],
  {ok,Next_State,Next_St};

%% Process an arity 2 built-in function (such as add or bitwise and)
%  by calculating whether processing the 1st or 2nd operand first would be less register
%  intensive, then returning the way which is less register intensive.
%% TODO: #N/A
%        Find a more efficient way to do this, as this is exponential complexity.
generate({bif,T,[A,B]}, State) ->
  Way_1 = process_bif(T,A,B,State,true),
  Way_2 = process_bif(T,A,B,State,false),
  case {(element(2,Way_1))#state.lvcnt,(element(2,Way_2))#state.lvcnt} of
    {_A,_B} when _A>_B -> Way_2;
    _ -> Way_1
  end;

%% Process an address operator by adding an expression to take the address of
%  the value which was loaded to a register or put on the stack in the last instruction
%  and store it in the destination of the last instruction.
generate({{'&',Ln},Raw_St}, State) ->
  {ok, Ref_State, Ref_St} = generate(Raw_St, State),
  case lists:last(Ref_St) of
    {move,Src,Dest} ->
      Next_St = lists:droplast(Ref_St) ++ [{address,Src,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    {load,_Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    {address,_Src,Dest} ->
      Next_St = Ref_St ++ [{address,Dest,Dest}],
      {ok,copy_lvcnt(State,Ref_State),Next_St};
    Other -> error({address_error,Other,{line,Ln}})
    end;

%% Process a dereference operator by finding the location of the variable we are
%  dereferencing and either replacing the `move` statement with a `load` statement
%  or adding a `load` statement to the end, depending on what we are dereferncing.
generate({{'*',Ln},Raw_St},State) ->
  {ok, Ptr_State, Ptr_St} = generate(Raw_St, State),
  Active_Reg = {x,State#state.lvcnt},
  {N,T,S} = maps:get(Active_Reg,Ptr_State#state.typecheck,undefined),
  New_Types = maps:put(Active_Reg,{N-1,T,S},Ptr_State#state.typecheck),
  N_State = Ptr_State#state{typecheck=New_Types},
  case lists:last(Ptr_St) of
    {move,Src,Dest} ->
      Next_St = lists:droplast(Ptr_St) ++ [{load,Src,Dest}],
      {ok,copy_lvcnt(State,N_State),Next_St};
    {_,_,Dest} ->
      Next_St = Ptr_St ++ [{load,Dest,Dest}],
      {ok,copy_lvcnt(State,N_State),Next_St};
    Other -> error({Other,{line,Ln}})
  end;

%% Any other nodes of the AST are currently unsupported.
%  Currently we raise an error, dumping the unsupported node as well as the current state.
%% TODO: #11
%        I need to look further into globabl variables,
%        but it'd be very helpful to be able to differentiate them here.
%% TODO: #2
%        We need to add unary and postfix operators like `++` and `-`.
%% TODO: N/A
%        We need to support chars, strings, etc. here
%% TODO: #5
%        We need to support array declarations and accesses,
%        which will be done using the `[]` operators and the `offset` token.
generate(Other, State) -> error({Other,State}).

%% Delegated function for declarations.
%% Declarations with an initialisation are processed by allocating memory
%  for them on the stack, processing the initialisation value and storing
%  the initialisation value in the newly allocated stack slot.
get_decl_specs({N,Raw_T,Raw_S}, [{Raw_Ident,{'=',_},Raw_St}], State) ->
  {ok, Ident, Ptr_Depth, Arr} = get_ident_specs(Raw_Ident, State),
  Type = {N+Ptr_Depth,Raw_T,Raw_S},
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Arr, State),
  {ok, Decl_State, Decl_St} = generate(Raw_St, Mem_State),
  Active_Reg = {x,State#state.lvcnt},
  Rv_Cnt = Decl_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Decl_State#state.var),
  New_Types = maps:put({y,Rv_Cnt},Type,Decl_State#state.typecheck),
  Next_State = (copy_lvcnt(State,Decl_State))#state{var=New_Var,rvcnt=Rv_Cnt+1,typecheck=New_Types},
  Next_St = case maps:get(Active_Reg,Next_State#state.typecheck,undefined) of
    Type -> Mem_St ++ Decl_St ++ [{move,Active_Reg,{y,Rv_Cnt}}];
    _ -> Mem_St ++ Decl_St ++ [{cast,Active_Reg,Type},{move,Active_Reg,{y,Rv_Cnt}}]
  end,
  {ok, Next_State, Next_St};

%% Declarations without an initialisation are processed by allocating memory
%  for them on the stack and then updating the state to indicate the new stack size.
get_decl_specs({N,Raw_T,Raw_S}, [Raw_Ident], State) ->
  {ok, Ident, Ptr_Depth, Arr} = get_ident_specs(Raw_Ident, State),
  Type = {N+Ptr_Depth+length(Arr),Raw_T,Raw_S},
  {ok, Mem_State, Mem_St} = allocate_mem(Type, Arr, State),
  Rv_Cnt = Mem_State#state.rvcnt,
  New_Var = maps:put(Ident,{Type,{y,Rv_Cnt}},Mem_State#state.var),
  New_Types = maps:put({y,Rv_Cnt},Type,Mem_State#state.typecheck),
  Next_State = (copy_lvcnt(State,Mem_State))#state{var=New_Var,rvcnt=Rv_Cnt+1,typecheck=New_Types},
  {ok,Next_State,Mem_St};

%% Any other declaration types are currently unsupported.
%% TODO: #5
%        We need to support array declarations and accesses,
%        which will be done using the `[]` operators and the `offset` token
get_decl_specs(Type, Other, State) -> error({Type,Other,State}).

%% Function for getting information about an identifier.
%% Strips superfluous information (line numbers etc) from the identifier
%  and returns the identifier and the a version with dereference etc. operators maintained.
%% TODO: #4
%        Currently we can take the address of a variable just fine, however chaining
%        dereference & address operators (eg `*&x = 10;`) passes the parser but
%        cannot be processed. The fix here is easy but I believe other fixes are needed.
get_ident_specs({{'*',_},Rest}, State) ->
  {ok, Ident, Ptr_Depth, Arr} = get_ident_specs(Rest, State),
  {ok, Ident, Ptr_Depth+1, Arr};
get_ident_specs({{{'*',_},Ptr},Rest}, State) ->
  {ok, Ident, Ptr_Depth, Arr} = get_ident_specs({Ptr,Rest}, State),
  {ok, Ident, Ptr_Depth+1, Arr};
get_ident_specs({Rest,{array,{int_l,_,N,_}}}, State) ->
  {ok, Ident, Ptr_Depth, Arr} = get_ident_specs(Rest, State),
  {ok, Ident, Ptr_Depth,[N|Arr]};
get_ident_specs({identifier,_,Ident}, _State) ->
  {ok, Ident, 0, []};
get_ident_specs(Ident, _State) ->
  error({ident_specs,Ident}).


%% To allocate stack memory for a variable, allocate the size of the variable type.
allocate_mem(Type,[],State) ->
  Rv_Cnt = State#state.rvcnt,
  N_Sizes = maps:put({y,Rv_Cnt},sizeof(Type,State),State#state.sizeof),
  N_State = State#state{sizeof=N_Sizes},
  {ok,N_State,[{allocate,sizeof(Type,State)}]};
allocate_mem(Type,Arr,State)->
  Heap_St = lists:flatten(gen_heap(Type,Arr,State)),
  Size = lists:foldr(fun (V,Acc) -> Acc*V end,sizeof(Type,State),Arr),
  Rv_Cnt = State#state.rvcnt,
  N_Sizes = maps:put({y,Rv_Cnt},Size,State#state.sizeof),
  N_State = State#state{sizeof=N_Sizes},
  {ok,N_State,Heap_St++[{allocate,?SIZEOF_POINTER},
                      {move,{x,State#state.lvcnt},{y,Rv_Cnt}}]}.

% This is absolutely hideous
% but I don't think multi-dimensional arrays will come up too much
gen_heap({P,T,S},[],State) ->
  [{move,{i,0},{x,State#state.lvcnt}}];
gen_heap({P,T,S},[N|Arr],State) ->
  Lv_Cnt = State#state.lvcnt,
  Size = sizeof({P,T,S},State),
  [{test_heap,Size*N,{x,Lv_Cnt}},{cast,{x,Lv_Cnt},{P,T,S}} |
   [[{move,{i,Ptr},{x,Lv_Cnt+1}},
     {'+',[{x,Lv_Cnt},{x,Lv_Cnt+1}],{x,Lv_Cnt+1}} |
     gen_heap({P-1,T,S},Arr,State#state{lvcnt=Lv_Cnt+2})] ++
    [{store,{x,Lv_Cnt+2},{x,Lv_Cnt+1}}]  || Ptr <- lists:seq(0,N-1)]].



%% To deallocate memory due to variables going out of scope,
%  such as at the end of a compound statement or for a return statement,
%  the number of variables to trim the stack to is found & returned
deallocate_mem(State_1,State_2) ->
  Rv_Cnt = maps:size(State_1),
  {ok, {gc,Rv_Cnt}}.

%% Delegated function for assignment.
%% For a normal assignment, the value to be assigned is processed and stored in the
%  active register. The destination is evaluated as to whether it is a variable or
%  a memory location and then the appropriate move/store instructions are returned.
get_assign_specs('=',[Raw_Ident,Raw_St], State) ->
  {ok,Ident,Ptr_Depth,Arr} = get_ident_specs(Raw_Ident, State),
  {ok,Raw_Type,Ptr} = case maps:get(Ident,State#state.var,undefined) of
    {T,Ptr_Loc} -> {ok,T,Ptr_Loc};
    Other -> {error, {Other,{undeclared,Ident}}}
  end,
  % TODO: Arrays
  {ok,Ptr_Type,Ptr_St} = get_ptr(Raw_Type,Ptr_Depth,Ptr,State#state{lvcnt=State#state.lvcnt+1}),
  Lv_Cnt = State#state.lvcnt,
  {ok,Assign_State,Assign_St} = generate(Raw_St,State),
  St_Type = maps:get({x,Lv_Cnt},Assign_State#state.typecheck,undefined),
  case Ptr_St of
    [] ->
      End_St = if
        St_Type =/= Ptr_Type -> [{cast,{x,Lv_Cnt},Ptr_Type},{move,{x,Lv_Cnt},Ptr}];
        true -> [{move,{x,Lv_Cnt},Ptr}]
      end,
      Next_St = Assign_St ++ End_St,
      {ok,copy_lbcnt(Assign_State,State),Next_St};
    Ptr_St ->
      {_,_,Dest} = lists:last(Assign_St),
      End_St = if
        St_Type =/= Ptr_Type -> [{cast,{x,Lv_Cnt},Ptr_Type},{store,Dest,{x,Lv_Cnt+1}}];
        true -> [{store,Dest,{x,Lv_Cnt+1}}]
      end,
      Next_St = Assign_St ++ Ptr_St ++ End_St,
      {ok,copy_lbcnt(Assign_State,State),Next_St}
  end;

%% A non-normal assignment such as `+=` is confirmed into an assignment and
%  a built-in function to calulate the result of the operation.
get_assign_specs(Op,[Raw_Ident,Raw_St], State) ->
  get_assign_specs('=',[Raw_Ident,{bif,Op,[Raw_Ident,Raw_St]}], State);

%% Any other declaration types are currently unsupported.
%% I'm not certain that there are any, however if there are then it will cause an error.
get_assign_specs(Op, Other, State) ->
  error({Op,Other,State}).

%% When there is no dereference operator, an empty statement is returned.
get_ptr(Type,0,Ptr,State) ->
  {ok,Type,[]};
get_ptr({N,T,S},Ptr_Depth,Ptr,State) ->
  Type = {N-Ptr_Depth,T,S},
  Active_Reg = {x,State#state.lvcnt},
  %% Should this be -1?
  Load_St = [{move,Ptr,Active_Reg}|get_ptr_load_st(Ptr_Depth,Active_Reg,Active_Reg)],
  {ok,Type,Load_St}.

get_ptr_load_st(1,Reg,Ptr) -> [];
get_ptr_load_st(N,Reg,Ptr) -> [{load,Ptr,Reg}|get_ptr_load_st(N-1,Reg,Reg)].

%% Delegated function for processing built-in functions.
%% A special case for operations which can be done on pointers
%% TODO: N/A
%        Check for other pointer operations
%% TODO: N/A
%        Tidy this up
process_bif('+',Fst,Sec,State,Swap) ->
  Lv_Cnt = State#state.lvcnt,
  {A,B} = if Swap -> {Sec,Fst};
             true -> {Fst,Sec} end,
  {R1,R2} = if Swap -> {{x,Lv_Cnt+1},{x,Lv_Cnt}};
               true -> {{x,Lv_Cnt},{x,Lv_Cnt+1}} end,
  {ok,A_State,A_St} = generate(A,State),
  A_Type = maps:get({x,Lv_Cnt},A_State#state.typecheck,undefined),
  N_A_State = A_State#state{lvcnt=Lv_Cnt+1},
  {ok,B_State,B_St} = generate(B,N_A_State),
  B_Type = maps:get({x,Lv_Cnt+1},B_State#state.typecheck,undefined),
  R_Type = case {A_Type,B_Type} of
    {{0,T,S_A},{0,T,S_B}} -> {0,T,max(S_A,S_B)};
    {{N,T,S_A},{0,i,S_B}} -> {N,T,S_A}; %% TODO: Change size of A
    {{0,i,S_A},{N,T,S_B}} -> {N,T,S_A}; %% TODO: Change size of B
    Types -> error({{undefined_op_cast,'+'},Types})
  end,
  Lv_Cnt = State#state.lvcnt,
  Statement = A_St ++ B_St ++ [{'+',[R1,R2],{x,Lv_Cnt}}],
  N_Types = maps:put({x,Lv_Cnt},R_Type,B_State#state.typecheck),
  Rtn_State = B_State#state{typecheck=N_Types},
  {ok,Rtn_State,Statement};


process_bif('-',Fst,Sec,State,Swap) ->
  Lv_Cnt = State#state.lvcnt,
  {A,B} = if Swap -> {Sec,Fst};
             true -> {Fst,Sec} end,
  {R1,R2} = if Swap -> {{x,Lv_Cnt+1},{x,Lv_Cnt}};
               true -> {{x,Lv_Cnt},{x,Lv_Cnt+1}} end,
  {ok,A_State,A_St} = generate(A,State),
  N_A_State = A_State#state{lvcnt=Lv_Cnt+1},
  {ok,B_State,B_St} = generate(B,N_A_State),
  A_Type = maps:get({x,Lv_Cnt},A_State#state.typecheck,undefined),
  B_Type = maps:get({x,Lv_Cnt+1},B_State#state.typecheck,undefined),
  R_Type = case {A_Type,B_Type} of
    {{0,T,S_A},{0,T,S_B}} -> {0,T,max(S_A,S_B)};
    {{N,T,S_A},{0,i,S_B}} -> {N,T,S_A}; %% TODO: Change size of B
    Types -> error({{undefined_op_cast,'-'},Types})
  end,
  Lv_Cnt = State#state.lvcnt,
  Statement = A_St ++ B_St ++ [{'-',[R1,R2],{x,Lv_Cnt}}],
  N_Types = maps:put({x,Lv_Cnt},R_Type,B_State#state.typecheck),
  Rtn_State = B_State#state{typecheck=N_Types},
  {ok,Rtn_State,Statement};

%% Arity 2 BIFs are processed by processing each of their operands and
%  adding a statement which will take the active register and the register above it,
%  perform the built-in function in the values in those registers and store the result
%  in the active register.
process_bif(Type,Fst,Sec,State,Swap) ->
  Lv_Cnt = State#state.lvcnt,
  {A,B} = if Swap -> {Sec,Fst};
             true -> {Fst,Sec} end,
  {R1,R2} = if Swap -> {{x,Lv_Cnt+1},{x,Lv_Cnt}};
               true -> {{x,Lv_Cnt},{x,Lv_Cnt+1}} end,
  {ok,A_State,A_St} = generate(A,State),
  N_A_State = A_State#state{lvcnt=Lv_Cnt+1},
  {ok,B_State,B_St} = generate(B,N_A_State),
  A_Type = maps:get({x,Lv_Cnt},A_State#state.typecheck,undefined),
  B_Type = maps:get({x,Lv_Cnt+1},B_State#state.typecheck,undefined),
  R_Type = case {A_Type,B_Type} of
    {{0,T,S_A},{0,T,S_B}} -> {0,T,max(S_A,S_B)};
    {{0,_,S_A},{0,_,S_B}} -> {0,f,max(S_A,S_B)};
    Types -> error({{undefined_op_cast,Type},Types})
  end,
  Lv_Cnt = State#state.lvcnt,
  Statement = A_St ++ B_St ++ [{Type,[R1,R2],{x,Lv_Cnt}}],
  N_Types = maps:put({x,Lv_Cnt},R_Type,B_State#state.typecheck),
  Rtn_State = B_State#state{typecheck=N_Types},
  {ok,Rtn_State,Statement}.

%% Get a shortened name of a type
%% TODO: #18
%        We need to add a typedef, enum & struct resolver here
get_type([{unsigned,_}|Type],C) ->
  case get_type(Type,C) of
    {ok,{P,i,N}} -> {ok,{P,u,N}};
    {error,{unknown_type,T}} -> {error,{unknown_type,[unsigned|T]}};
    _ -> {error,{unknown_type,[unsigned|Type]}}
  end;
get_type([{signed,_}|Type],C) ->
  case get_type(Type,C) of
    {ok,{P,i,N}} -> {ok,{P,i,N}};
    {error,{unknown_type,T}} -> {error,{unknown_type,[signed|T]}};
    _ -> {error,{unknown_type,[signed|Type]}}
  end;
get_type([{long,_},{double,_}],_) -> {ok,{0,f,?SIZEOF_L_DOUBLE}};
get_type([{double,_}],_)          -> {ok,{0,f,?SIZEOF_DOUBLE}};
get_type([{float,_}],_)           -> {ok,{0,f,?SIZEOF_FLOAT}};
get_type([{long,_},{int,_}],_)    -> {ok,{0,i,?SIZEOF_LONG}};
get_type([{long,_}],_)            -> {ok,{0,i,?SIZEOF_LONG}};
get_type([{int,_}],_)             -> {ok,{0,i,?SIZEOF_INT}};
get_type([{short,_},{int,_}],_)   -> {ok,{0,i,?SIZEOF_SHORT}};
get_type([{short,_}],_)           -> {ok,{0,i,?SIZEOF_SHORT}};
get_type([{char,_}],_)            -> {ok,{0,i,?SIZEOF_CHAR}};
get_type([{void,_}],_)            -> {ok,{0,n,0}};
get_type(Type,_)                  -> {error,{unknown_type, Type}}.

%% Function to return the size of different types.
%% TODO: #5
%        Arrays will likely behave a bit weirdly under sizeof,
%        so we have to establish their behaviour and implement them accordingly.
%% TODO: #18
%        Add support for compile-time evaluation of custom types.
%% TODO: N/A
%        Add support for compile-time evaluation of the size of variables,
%        if this is possible (confirm that allocation is done at declaration time?).
sizeof({0,_,S},_) -> S;
sizeof({_,_,_},State) -> ?SIZEOF_POINTER;
sizeof(Type,State) -> error({type,Type}).

%% 2x helper functions to copy the label/local variable count from 1 state to another.
copy_lbcnt(State_1,State_2) -> State_2#state{lbcnt=State_1#state.lbcnt}.
copy_lvcnt(State_1,State_2) -> State_2#state{lvcnt=State_1#state.lvcnt}.
