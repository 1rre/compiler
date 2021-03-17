-module(mips_op).
-export([gen_op/7]).

-record(context,{fn=#{},types=#{},sp=0,s_reg=#{},reg=#{},i_reg,f_reg,labels=[],stack_size=0,fp=0}).

%% Putting boilerplate for built in functions here because it clutters the mips file

simple_int_op(Op,Type,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,Type,Context),
  {ok,[{Op,Dest,Src_1,Src_2}],Dest_Context}.


simple_float_op(Op,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,[{Op,Dest,Src_1,Src_2}],Dest_Context};
    _ -> error({not_float,{Src_1,Src_2,Dest}})
  end.


simple_double_op(Op,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    % They should be though...
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 == R12+1
                                                            andalso R21 == R22+1
                                                            andalso R31 == R32+1 ->
      {ok,[{Op,R31,R11,R21}],Dest_Context};
    _ -> error({nonconsecutive,{Src_1,Src_2,Dest}})
end.


gen_op('+',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  simple_float_op('add.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('+',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  simple_double_op('add.d',Reg_1,Reg_2,Reg_3,Context);

gen_op('-',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  simple_float_op('sub.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('-',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  simple_double_op('sub.d',Reg_1,Reg_2,Reg_3,Context);

gen_op('*',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  simple_float_op('mul.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('*',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  simple_double_op('mul.d',Reg_1,Reg_2,Reg_3,Context);

gen_op('/',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  simple_float_op('div.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('/',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  simple_double_op('div.d',Reg_1,Reg_2,Reg_3,Context);

% && is multi-instruction & uses dest as temp so we need to avoid overwriting an operand
gen_op('&&',_,Size,Reg_1,Reg_2,Reg_3,Context) when 32 >= Size ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,32},Context),
  % uses dest as temp
  case {Src_1,Src_2,Dest} of
    {_,A,A} -> {ok,[{sltiu,Dest,Src_2,1},{xori,Dest,Dest,1},{movz,Dest,{i,0},Src_1}],Dest_Context};
    _ -> {ok,[{sltiu,Dest,Src_1,1},{xori,Dest,Dest,1},{movz,Dest,{i,0},Src_2}],Dest_Context}
  end;

gen_op('||',_,Size,Reg_1,Reg_2,Reg_3,Context) when 32 >= Size ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,32},Context),
  {ok,[{'or',Dest,Src_1,Src_2},{sltu,Dest,{i,0},Dest}],Dest_Context};

gen_op('|',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) or (T =:= u) ->
  simple_int_op('or',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('&',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) or (T =:= u) ->
  simple_int_op('and',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('^',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) or (T =:= u) ->
  simple_int_op('xor',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('==',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) or (T =:= u) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,T,32},Context),
  {ok,[{'xor',Dest,Src_1,Src_2},{sltiu,Dest,Dest,1}],Dest_Context};

gen_op('!=',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) or (T =:= u) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,T,32},Context),
  {ok,[{'xor',Dest,Src_1,Src_2},{sltu,Dest,{i,0},Dest}],Dest_Context};

gen_op('<',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(slt,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sltu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(slt,{0,i,Size},Reg_2,Reg_1,Reg_3,Context);

gen_op('>',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sltu,{0,u,Size},Reg_2,Reg_1,Reg_3,Context);

gen_op('<=',i,Size,Reg_1,Reg_2,Reg_3,Context)->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,Size},Context),
  {ok,[{slt,Dest,Src_2,Src_1},{'not',Dest,Dest},{andi,Dest,Dest,1}],Dest_Context};

gen_op('<=',u,Size,Reg_1,Reg_2,Reg_3,Context)->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{sltu,Dest,Src_2,Src_1},{'not',Dest,Dest},{andi,Dest,Dest,1}],Dest_Context};

gen_op('>=',i,Size,Reg_1,Reg_2,Reg_3,Context)->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,Size},Context),
  {ok,[{slt,Dest,Src_1,Src_2},{'not',Dest,Dest},{addi,Dest,Dest,2}],Dest_Context};

gen_op('>=',u,Size,Reg_1,Reg_2,Reg_3,Context)->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{sltu,Dest,Src_1,Src_2},{'not',Dest,Dest},{andi,Dest,Dest,1}],Dest_Context};


gen_op('+',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(add,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('+',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(addu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('-',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sub,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('-',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{neg,Dest,Src_2},{addu,Dest,Src_1,Dest}],Dest_Context};

gen_op('*',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,Size},Context),
  {ok,[{mult,Src_1,Src_2},{mflo,Dest}],Dest_Context};

gen_op('*',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{multu,Src_1,Src_2},{mflo,Dest}],Dest_Context};

gen_op('/',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,Size},Context),
  {ok,[{'div',Src_1,Src_2},{mflo,Dest}],Dest_Context};

gen_op('/',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{divu,Src_1,Src_2},{mflo,Dest}],Dest_Context};

gen_op('%',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,Size},Context),
  {ok,[{'div',Src_1,Src_2},{mfhi,Dest}],Dest_Context};

gen_op('%',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,u,Size},Context),
  {ok,[{divu,Src_1,Src_2},{mfhi,Dest}],Dest_Context};


gen_op(Op,_,_,_,_,_,_) -> error({no_mips,Op}).
