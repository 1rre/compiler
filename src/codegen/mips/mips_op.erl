-module(mips_op).
-export([gen_op/7]).

-record(context,{fn=#{},types=#{},sp=0,s_reg=#{},reg=#{},args=[],
                 i_reg,f_reg,labels=[],stack_size=0,fp=0,lb_sp=#{}}).
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

float_compare(Op,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  Un = abs(erlang:unique_integer()),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,A},{f,B},{R,C}} when R /= f ->
      {ok,[{Op,Src_1,Src_2},
           {bc1t,{"$float_cmp_true",[A,B,C,Un]}},
           {li,{R,C},0},
           {b,{"$float_cmp_false",[A,B,C,Un]}},
           nop,
           {"$float_cmp_true",[A,B,C,Un]},
           {li,{R,C},1},
           {"$float_cmp_false",[A,B,C,Un]}],Dest_Context};
    _ -> error({not_float,{Src_1,Src_2,Dest}})
  end.



simple_double_op(Op,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    % They should be though...
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11+1 == R12
                                                            andalso R21+1 == R22
                                                            andalso R31+1 == R32 ->
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

gen_op('|',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) orelse (T =:= u) ->
  simple_int_op('or',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('&',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) orelse (T =:= u) ->
  simple_int_op('and',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('^',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) orelse (T =:= u) ->
  simple_int_op('xor',{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('==',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) orelse (T =:= u) ->
  simple_int_op(seq,{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('!=',T,Size,Reg_1,Reg_2,Reg_3,Context) when (T =:= i) orelse (T =:= u) ->
  simple_int_op(sne,{0,T,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(slt,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sltu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sgt,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sgtu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<=',i,Size,Reg_1,Reg_2,Reg_3,Context)->
  simple_int_op(sle,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<=',u,Size,Reg_1,Reg_2,Reg_3,Context)->
  simple_int_op(sleu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>=',i,Size,Reg_1,Reg_2,Reg_3,Context)->
  simple_int_op(sge,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>=',u,Size,Reg_1,Reg_2,Reg_3,Context)->
  simple_int_op(sgeu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<<',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sllv,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('<<',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sllv,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>>',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(srav,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('>>',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(srav,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('+',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(add,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('+',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(addu,{0,u,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('-',i,Size,Reg_1,Reg_2,Reg_3,Context) ->
  simple_int_op(sub,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

gen_op('-',u,Size,Reg_1,Reg_2,Reg_3,Context) ->
    simple_int_op(subu,{0,i,Size},Reg_1,Reg_2,Reg_3,Context);

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


gen_op('==',f,Size,Reg_1,Reg_2,Reg_3,Context) ->
  float_compare('c.eq.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('!=',f,Size,Reg_1,Reg_2,Reg_3,Context) ->
  float_compare('c.neq.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('<',f,Size,Reg_1,Reg_2,Reg_3,Context) ->
  float_compare('c.lt.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('>',f,Size,Reg_1,Reg_2,Reg_3,Context) ->
  float_compare('c.lt.s',Reg_2,Reg_1,Reg_3,Context);

gen_op('<=',f,Size,Reg_1,Reg_2,Reg_3,Context)->
  float_compare('c.le.s',Reg_1,Reg_2,Reg_3,Context);

gen_op('>=',f,Size,Reg_1,Reg_2,Reg_3,Context)->
  float_compare('c.le.s',Reg_2,Reg_1,Reg_3,Context);


gen_op(Op,_,_,_,_,_,_) -> error({no_mips,Op}).
