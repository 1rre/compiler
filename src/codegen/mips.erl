-module(mips).
-export([generate/1,get_reg/3]).

-include("arch_type_consts.hrl").

-record(context,{fn=#{},types=#{},sp=0,s_reg=#{},reg=#{},i_reg,f_reg,labels=[],stack_size=0,fp=0}).

generate(Ir) ->
  % Use these for ints & pointers if possible
  Reg_Pref = [{i,2},{i,3},{i,8},{i,9},{i,10},{i,11},{i,12},{i,13},{i,14},{i,15},{i,24},{i,25},
              {s,16},{s,17},{s,18},{s,19},{s,20},{s,20},{s,21},{s,22},{s,23}],
  % Use these for floats if possible
  Float_Reg = [{f,0},{f,1},{f,2},{f,3},{f,4},{f,5},{f,6},{f,7},{f,8},{f,9},{f,10},{f,11},{f,12},
               {f,13},{f,14},{f,15},{f,16},{f,17},{f,18},{f,19},{f,20},{f,21},{f,22},{f,23},{f,24},
               {f,25},{f,26},{f,27},{f,28},{f,29},{f,30},{f,31}],

  Context = lists:foldl(fun
    ({function,Type,Name,Args,_},Cxt) ->
      Cxt#context{fn=maps:put(Name,{Type,Args},Cxt#context.fn)};
    ({global,Type,Name,_},Cxt) ->
      Cxt#context{types=maps:put({g,Name},Type,Cxt#context.types)}
    end, #context{i_reg=Reg_Pref,f_reg=Float_Reg}, Ir),
  {Data,Text} = lists:foldl(fun
    ({function,Type,Name,Args,St},{Data,Text}) ->
      {ok,Args_Context} = gen_arg_types(lists:reverse(Args),Context,0,false,0),
      Scope_Asm = [{addiu,{i,29},{i,29},-Args_Context#context.sp}|gen_scoped(St,Args_Context)],
      Asm = Scope_Asm,
      {Data,[{'.globl',Name},{'.ent',Name},{Name,[]}|Asm]++[{'.end',Name}|Text]};
    ({global,Type,Name,Frame},{Data,Text}) ->
      % .bgnb Name
      Asm = gen_global(Frame,Name),
      %% TODO: Label for global
      %% Reversed order such that functions
      {Data++Asm,Text}
  end,{[],[]},lists:sort(Ir)),
  {ok,['.data'|Data]++['.text'|Text]}.


%% TODO: get register positions for call
gen_arg_types([{0,f,32}|Args],Context,0,false,0) ->
  Types = maps:put({z,0},{0,f,32},Context#context.types),
  Reg = maps:put({z,0},{f,12},Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},1,false,32);

gen_arg_types([{0,f,32}|Args],Context,N,false,Bits) when 32 >= Bits ->
  Types = maps:put({z,N},{0,f,32},Context#context.types),
  Reg = maps:put({z,N},{f,14},Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},N+1,false,Bits+32);

gen_arg_types([{0,f,64}|Args],Context,0,false,0) ->
  Types = maps:put({z,0},{0,f,64},Context#context.types),
  Reg = maps:put({z,0},[{f,12},{f,13}],Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},1,false,64);

gen_arg_types([{0,f,64}|Args],Context,N,false,Bits) when 64 >= Bits ->
  Types = maps:put({z,N},{0,f,64},Context#context.types),
  Reg = maps:put({z,N},[{f,14},{f,15}],Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},N+1,false,Bits+64);

gen_arg_types([{0,T,64}|Args],Context,N,_,Bits) when 64 >= Bits andalso 2 >= N ->
  Types = maps:put({z,N},{0,T,64},Context#context.types),
  Reg = maps:put({z,N},[{i,6},{i,7}],Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},N+1,true,Bits+64);

gen_arg_types([Hd|Args],Context,N,_,Bits) when 96 >= Bits andalso 3 >= N ->
  Types = maps:put({z,N},Hd,Context#context.types),
  Reg = maps:put({z,N},{i,4+Bits div 32},Context#context.reg),
  gen_arg_types(Args,Context#context{types=Types,reg=Reg},N+1,true,Bits+32);

gen_arg_types([Hd|Args],Context,N,Int_Reg,Bits) ->
  Types = maps:put({z,N},Hd,Context#context.types),
  gen_arg_types(Args,Context#context{types=Types},N+1,true,Bits+sizeof(Hd));

% Frame pointer is offset from 0 in the context we return
% as we count the SP from the start of the arguments
% - or + here?
gen_arg_types([],Context,_,_,Bits) -> {ok,Context#context{sp=-Bits div 8}}.


%% Stack Resize Up
% I know this isn't technically necessary for C
% but it's built into the IR now and it'd be difficult to change
gen_scoped([{allocate,Size}|Rest],Context) ->
  N = Context#context.stack_size,
  Sp = Context#context.sp,
  N_S_Reg = maps:put({y,N},Sp+(Size div 8),Context#context.s_reg),
  N_Context = Context#context{s_reg=N_S_Reg,
                              sp=Sp+(Size div 8),
                              stack_size=N+1},
  [{addiu,{i,29},{i,29},-(Size div 8)}|gen_scoped(Rest,N_Context)];

%% Stack Resize Down
% I know this isn't technically necessary for C
% but it's built into the IR now and it'd be difficult to change
gen_scoped([{gc,N}|Rest],Context) ->
  Types = maps:filter(fun
    ({y,X},_) when X >= N -> false;
    (_,_) -> true
  end,Context#context.types),
  S_Reg = maps:filter(fun
    ({y,X},_) when X >= N -> false;
    (_,_) -> true
  end,Context#context.s_reg),
  Sp = maps:get({y,N},Context#context.s_reg,0),
  Diff = Context#context.sp - maps:get({y,N},Context#context.s_reg,0),
  N_Context = Context#context{sp=Sp,s_reg=S_Reg,stack_size=N,types=Types},
  [{addiu,{i,29},{i,29},Diff}|gen_scoped(Rest,N_Context)];

%% Return
%% TODO: Implement return properly with SP movement etc.
gen_scoped([return|Rest],Context) ->
  [{move,{i,29},{i,30}},{jr,{i,31}},nop|gen_scoped(Rest,Context)];

%% Move an int literal to a register
gen_scoped([{move,{i,Val},{x,N}}|Rest],Context) when 16#FFFFFFFF >= Val
                                                andalso Val >= -16#8000000 ->
  {ok,Reg,Reg_Context} = get_reg({x,N},{0,i,32},Context),
  [{li,Reg,Val}|gen_scoped(Rest,Reg_Context)];

%% Move a long literal to a register
gen_scoped([{move,{i,Val},{x,N}}|Rest],Context) ->
  error({not_impl,long});

% Move a double to a register
gen_scoped([{move,{f,Val},{x,N}},{cast,{x,N},{0,f,64}}|Rest],Context) ->
  error({not_impl,double});

% Move a float to a register
gen_scoped([{move,{f,Val},{x,N}}|Rest],Context) ->
  {ok,Reg,Reg_Context} = get_reg({x,N},{0,f,32},Context),
  <<Value:32>> = <<Val:32/float>>,
  [{'li.s',Reg,Value}|gen_scoped(Rest,Reg_Context)];


gen_scoped([{move,{x,Ns},{y,Nd}}|Rest],Context) ->
  Src = maps:get({x,Ns},Context#context.reg,{i,0}),
  % If for whatever reason the reg hasn't been declared, warn the user?
  if Src =:= {i,0} ->
       io:fwrite(standard_error,"~p not assigned to a register, using $0~n",[{x,Nd}]);
     true -> ok end,
  Dest = maps:get({y,Nd},Context#context.s_reg,Context#context.sp),
  {Ps,Ts,Ss} = maps:get({x,Ns},Context#context.types,{0,i,32}),
  {Pd,Td,Sd} = maps:get({y,Nd},Context#context.types,{Ps,Ts,Ss}),
  Size = if {Pd,Ps} =:= {0,0} andalso Ss > Sd ->
       io:fwrite(standard_error,"Truncating ~B bit item to ~B bits to avoid stack Error~n",[Ss,Sd]),
       Sd;
     true -> Ss end,
  Instr = case {Size,Src} of
    {32,{f,N}} -> 's.s';
    {64,[{f,N1},{f,N2}]} when N2 =:= N1+1 -> 's.d';
    {64,[{f,N1},N2]} -> error({non_consecutive,[{f,N1},N2]});
    {32,Reg} -> sw;
    {16,Reg} -> sh;
    {8,Reg} -> sb
  end,
  N_Types = maps:put({y,Nd},{Pd,Td,Sd},Context#context.types),
  N_Context = Context#context{types=N_Types},
  %% TODO: Find out what way around SP should be
  [{Instr,Src,{sp,Context#context.sp-Dest}}|gen_scoped(Rest,N_Context)];

% Arguments
gen_scoped([{move,{z,Ns},{y,Nd}}|Rest],Context) ->
  case maps:get({z,Ns},Context#context.reg,nil) of
    nil ->
      Types = maps:put({y,Nd},maps:get({z,Ns},Context#context.types),Context#context.types),
      gen_scoped(Rest,Context#context{types=Types});
    Src ->
      Dest = maps:get({y,Nd},Context#context.s_reg,Context#context.sp),
      {Ps,Ts,Ss} = maps:get({z,Ns},Context#context.types,{0,i,32}),
      {Pd,Td,Sd} = maps:get({y,Nd},Context#context.types,{Ps,Ts,Ss}),
      Size = if {Pd,Ps} =:= {0,0} andalso Ss > Sd ->
           io:fwrite(standard_error,"Truncating ~B bit item to ~B bits to avoid stack Error~n",[Ss,Sd]),
           Sd;
         true -> Ss end,
      Instr = case {Size,Src} of
        {32,{f,N}} -> 's.s';
        {64,[{f,N1},{f,N2}]} when N2 =:= N1+1 -> 's.d';
        {64,[{f,N1},N2]} -> error({non_consecutive,[{f,N1},N2]});
        {32,Reg} -> sw;
        {16,Reg} -> sh;
        {8,Reg} -> sb
      end,
      N_Types = maps:put({y,Nd},{Ps,Ts,Ss},Context#context.types),
      N_Context = Context#context{types=N_Types},
      %% TODO: Find out what way around SP should be
      [{Instr,Src,{sp,Context#context.sp-Dest}}|gen_scoped(Rest,N_Context)]
  end;

% Get from stack
gen_scoped([{move,{y,Ns},{x,Nd}}|Rest],Context) ->
  Src = maps:get({y,Ns},Context#context.s_reg,Context#context.sp),
  {Ps,Ts,Ss} = maps:get({y,Ns},Context#context.types,{0,i,32}),
  {ok,Dest,Reg_Context} = get_reg({x,Nd},{Ps,Ts,Ss},Context),
  Size = if Ps =:= 0 -> Ss;
            true -> 32 end,
  Instr = case {Size,Dest} of
    {32,{f,N}} -> 'l.s';
    {64,[{f,N1},{f,N2}]} when N2 =:= N1+1 -> 'l.d';
    {64,[{f,N1},N2]} -> error({non_consecutive,[{f,N1},N2]});
    {32,Reg} -> lw;
    {16,Reg} -> lh
  end,
  %% TODO: Find out what way around SP should be
  [{Instr,Dest,{sp,Context#context.sp-Src}}|gen_scoped(Rest,Reg_Context)];

% Get from stack
gen_scoped([{address,{y,Ns},{x,Nd}}|Rest],Context) ->
  Src = maps:get({y,Ns},Context#context.s_reg),
  {Ps,Ts,Ss} = maps:get({y,Ns},Context#context.types,{0,i,32}),
  {ok,Dest,Reg_Context} = get_reg({x,Nd},{Ps+1,Ts,Ss},Context),
  % Do we need a sub statement here?
  [{addiu,Dest,{i,29},Context#context.sp-Src}|gen_scoped(Rest,Reg_Context)];


% Get from stack
gen_scoped([{load,{x,Ns},{x,Nd}}|Rest],Context) ->
  {Ps,Ts,Ss} = maps:get({x,Ns},Context#context.types,{1,i,32}),
  {ok,Src,Src_Context} = get_reg({x,Ns},{Ps,Ts,Ss},Context),
  {ok,Dest,Reg_Context} = get_reg({x,Nd},{Ps-1,Ts,Ss},Src_Context),
  Instr = case {Ss,Src} of
    {32,{f,N}} -> 'l.s';
    {64,[{f,N1},{f,N2}]} when N2 =:= N1+1 -> 'l.d';
    {64,[{f,N1},N2]} -> error({non_consecutive,[{f,N1},N2]});
    {32,Reg} -> lw;
    {16,Reg} -> lh;
    {8,Reg} -> lb
  end,
  [{Instr,Src,{0,Dest}}|gen_scoped(Rest,Reg_Context)];

% Will this be ok?
gen_scoped([{load,{y,Ns},{x,Nd}}|Rest],Context) ->
  gen_scoped([{move,{y,Ns},{x,Nd}},{load,{x,Nd},{x,Nd}}|Rest],Context);


gen_scoped([{store,{x,Ns},{x,Nd}}|Rest],Context) ->
  {Ps,Ts,Ss} = maps:get({x,Ns},Context#context.types,{0,i,32}),
  {ok,Src,Src_Context} = get_reg({x,Ns},{Ps,Ts,Ss},Context),
  {Pd,Td,Sd} = maps:get({x,Nd},Context#context.types,{Ps+1,Ts,Ss}),
  {ok,Dest,Reg_Context} = get_reg({x,Nd},{Pd,Td,Sd},Src_Context),
  Size = if {Pd,Ps} =:= {0,0} andalso Ss > Sd ->
       io:fwrite(standard_error,"Truncating ~B bit item to ~B bits to avoid stack Error~n",[Ss,Sd]),
       Sd;
     true -> Ss end,
  Instr = case {Size,Src} of
    {32,{f,N}} -> 's.s';
    {64,[{f,N1},{f,N2}]} when N2 =:= N1+1 -> 's.d';
    {64,[{f,N1},N2]} -> error({non_consecutive,[{f,N1},N2]});
    {32,Reg} -> sw;
    {16,Reg} -> sh;
    {8,Reg} -> sb
  end,
  [{Instr,Src,{0,Dest}}|gen_scoped(Rest,Reg_Context)];


gen_scoped([{cast,{x,N},{0,T,S}}|Rest],Context) when (T =:= i) or (T =:= u) ->
  <<Bitmask:S>> = <<16#FFFFFFFF>>,
  case maps:get({x,N},Context#context.types,{0,i,S}) of
    {0,NT,S} when (NT =:= i) or (NT =:= u) ->
      N_Types = maps:put({x,N},{0,u,S},Context#context.types),
      gen_scoped(Rest,Context);
    %% Really we should check for float registers and changes of register here
    {0,NT,_} when (NT =:= i) or (NT =:= u) ->
      {ok,Reg,Reg_Context} = get_reg({x,N},{0,i,S},Context),
      [{andi,Reg,Reg,Bitmask}|gen_scoped(Rest,Reg_Context)];
    Other ->
      error({no_mips,cast,{{0,i,S},{Other}}})
  end;

%% TODO: Do we need to do anything else here?
gen_scoped([{cast,{y,N},T}|Rest],Context) ->
  N_Types = maps:put({y,N},T,Context#context.types),
  gen_scoped(Rest,Context#context{types=N_Types});

gen_scoped([{cast,{x,N},{0,T,S}}|Rest],Context) ->
  error({no_mips,cast,{0,T,S}});

gen_scoped([{cast,{x,N},Type}|Rest],Context) ->
  case maps:get({x,N},Context#context.types,Type) of
    Type ->
      N_Types = maps:put({x,N},Type,Context#context.types),
      gen_scoped(Rest,Context);
    {0,T,S} ->
      error({no_mips,cast,{Type,{0,T,S}}});
    {N,T,S} ->
      % Pointer to pointer needs no extra work?
      N_Types = maps:put({x,N},Type,Context#context.types),
      gen_scoped(Rest,Context)
  end;

gen_scoped([{label,N}|Rest],Context) ->
  Str_N = integer_to_list(N),
  N_Labels = [Str_N|Context#context.labels],
  [{Str_N,[maps:size(Context#context.fn)]}|gen_scoped(Rest,Context#context{labels=N_Labels})];

gen_scoped([{jump,{l,N}}|Rest],Context) ->
  Str_N = integer_to_list(N),
  [{'j',{Str_N,[maps:size(Context#context.fn)]}},nop|gen_scoped(Rest,Context)];

% TODO: More storing on the stack & updating return value
gen_scoped([{call,Fn,Arity}|Rest],Context) ->
  Ra_Store = [{sw,{i,31},{sp,0}},{addiu,{i,29},{i,29},-4}],
  Ra_Pos = Context#context.sp,
  {Arg_Context,Arg_Store} = lists:foldl(fun (N, {N_Context,St}) ->
      Type = maps:get({z,N},N_Context#context.types,{0,i,32}),
      {ok,Reg,Reg_Context} = get_reg({z,N},Type,N_Context),
      case Reg of
        {f,N} ->
          N_Sp = Context#context.sp + 4,
          {St ++ [{'s.s',Reg,{sp,0}},{addiu,{i,29},{i,29},-4}],Reg_Context#context{sp=N_Sp}};
        [{f,N1},{f,N2}] -> error(double);
        Reg ->
          N_Sp = Context#context.sp + 4,
          {St ++ [{'sw',Reg,{sp,0}},{addiu,{i,29},{i,29},-4}],Reg_Context#context{sp=N_Sp}}
      end
    end,{Context#context{sp=Context#context.sp+4},[]},lists:seq(Arity-1,0,-1)),
  Ra_Diff = Arg_Context#context.sp - Context#context.sp,
  N_Fp_Jal = [{addiu,{i,30},{i,30},-Arg_Context#context.sp},{jal,Fn}],
  R_Fp_Ra = [{addiu,{i,30},{i,30},Arg_Context#context.sp},
             {addiu,{i,29},{i,29},Ra_Diff},
             {lw,{i,31},{sp,0}}],
  Ra_Store ++ Arg_Store ++ N_Fp_Jal ++ R_Fp_Ra ++ gen_scoped(Rest,Arg_Context);

gen_scoped([{test,Src,{l,N}}|Rest],Context) ->
  Str_N = integer_to_list(N),
  Type = maps:get(Src,Context#context.types,{0,i,32}),
  {ok,Reg,Reg_Context} = get_reg(Src,Type,Context),
  case Reg of
    {f,F_Reg} -> error(test_float);
    _ -> [{beq,Reg,{i,0},{Str_N,[maps:size(Context#context.fn)]}}|gen_scoped(Rest,Reg_Context)]
  end;

gen_scoped([{Op,[Src_1,Src_2],Dest}|Rest],Context) ->
  T1 = maps:get(Src_1,Context#context.types),
  T2 = maps:get(Src_2,Context#context.types),
  case {T1,T2} of
    % Same non-pointer type
    {{0,T,S},{0,T,S}} ->
      {ok,Res,Res_Context} = gen_op(Op,T,S,Src_1,Src_2,Dest,Context),
      Res++gen_scoped(Rest,Res_Context);
    % Adding unsigned to signed (useful for unsigned + constant?)
    {{0,T1,S},{0,T2,S}} when ((T1 =:= i) or (T1 =:= u)) andalso ((T2 =:= i) or (T2 =:= u)) ->
      {ok,Res,Res_Context} = gen_op(Op,i,S,Src_1,Src_2,Dest,Context),
      Res++gen_scoped(Rest,Res_Context);
    %% TODO: Pointers
    {T1,T2} -> error({cast,{T1,T2}})
  end;


gen_scoped([],Context) -> [];
gen_scoped([Other|_],Context) -> error({no_mips,Other}).

% Reg for a 64 bit object
get_reg(Reg,{0,_,64},Context) ->
  error({not_impl,double});
% Reg for a 32 bit float
get_reg(Reg,{0,f,32},Context) ->
  N_Types = maps:put(Reg,{0,f,32},Context#context.types),
  % Check if the register has been previously assigned
  case {maps:get(Reg,Context#context.reg,nil),Context#context.f_reg} of
    {{s,Saved},[Dest|Rest]} ->
      error(saved_reg);
    {{f,N},_} ->
      {ok,{f,N},Context#context{types=N_Types}};
    {[R1,R2],[Dest|Rest]} ->
      F_Reg = [{f,N} || {f,N} <- [R1,R2]] ++ Rest,
      I_Reg = [R || R <- [R1,R2], (element(1,R) =/= f)],
      N_Reg = maps:put(Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=F_Reg,i_reg=I_Reg,reg=N_Reg,types=N_Types}};
    {nil,[Dest|Rest]} ->
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=Rest,reg=N_Reg,types=N_Types}};
    {Old_Reg,[Dest|Rest]} ->
      I_Reg = [Old_Reg | Context#context.i_reg],
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=Rest,reg=N_Reg,i_reg=I_Reg,types=N_Types}};
    Other -> error(Other)
  end;
% Reg for something else (32 bits)
get_reg(Reg,Type,Context) ->
  N_Types = maps:put(Reg,Type,Context#context.types),
  case {maps:get(Reg,Context#context.reg,nil),Context#context.i_reg} of
    {{s,Saved},[Dest|Rest]} ->
      error(saved_reg);
    {{i,N},_} ->
      {ok,{i,N},Context#context{types=N_Types}};
    {[R1,R2],[Dest|Rest]} ->
      F_Reg = [{f,N} || {f,N} <- [R1,R2]],
      I_Reg = [R || R <- [R1,R2], not is_tuple(R) or element(1,R) =:= s] ++ Rest,
      N_Reg = maps:put(Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=F_Reg,i_reg=I_Reg,reg=N_Reg,types=N_Types}};
    {nil,[Dest|Rest]} ->
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{i_reg=Rest,reg=N_Reg,types=N_Types}};
    {Old_Reg,[Dest|Rest]} ->
      F_Reg = [Old_Reg | Context#context.f_reg],
      N_Reg = maps:put(Reg,Dest,Context#context.reg),
      {ok,Dest,Context#context{f_reg=F_Reg,reg=N_Reg,i_reg=Rest,types=N_Types}};
    Other -> error(Other)
  end.


%% Putting boilerplate for built in functions in a separate file because it clutters this file
gen_op(A,B,C,D,E,F,G) -> mips_op:gen_op(A,B,C,D,E,F,G).


gen_global(Frame,Name) ->
  Frame_Asm = gen_global(Frame,Name,[]),
  case Frame_Asm of
    [{'.word',_}|_] ->
      [{'.globl',Name},{Name,[]}|Frame_Asm];
    _ ->
    [{'.globl',Name},{'.bgnb',Name}|gen_global_ptr(Frame,Name,[])] ++ Frame_Asm ++ ['.endb']
  end.

gen_global({local,Frame},Name,Depth) ->
  lists:flatten(case hd(Frame) of
                  {data,_,_} ->
                    Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
                    [{Name,Depth} | [gen_global(Data,Name,[N|Depth]) || {Data,N} <- Indices]];
                  {local,_} ->
                    Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
                    [gen_global(Local,Name,[N|Depth]) || {Local,N} <- Indices]
                end);


gen_global({data,Type,Value},Name,Depth) ->
  {ok,St} = gen_data(Type,Value),
  St.

%% Char
gen_data({0,i,8},{i,Val}) ->
  {ok,[{'.byte',Val}]};
%% Short
gen_data({0,i,16},{i,Val}) ->
  {ok,[{'.half',Val}]};
%% Int or Long
gen_data({0,i,32},{i,Val}) ->
  {ok,[{'.word',Val}]};
%% Float
gen_data({0,f,32},{i,Val}) ->
  <<Word:32>> = <<Val:32/float>>,
  {ok,[{'.word',Word}]};
%% Double
gen_data({0,f,64},{i,Val}) ->
  <<Upper:32,Lower:32>> = <<Val:64/float>>,
  {ok,[{'.word',Upper},{'.word',Lower}]};
%% Pointer
gen_data({_,_,_},{i,Val}) ->
  {ok,[{'.word',Val}]}.

gen_global_ptr({local,Frame},Name,Depth) ->
  Local = case hd(Frame) of
    {local,_} -> [{Name,Depth}];
    _ -> []
  end,
  Indices = lists:zip(Frame,lists:seq(1,length(Frame))),
  lists:flatten([gen_global_ptr(Sub_Frame,Name,[N|Depth]) || {Sub_Frame,N} <- Indices] ++ Local);

gen_global_ptr(_,_,_) -> [].

sizeof({0,_,S}) -> S;
sizeof({_,_,_}) -> ?SIZEOF_POINTER;
sizeof(Type) -> error({type,Type}).
