-module(mips_op).
-export([gen_op/7]).

-record(context,{fn=#{},types=#{},sp=0,s_reg=#{},reg=#{},i_reg,f_reg,labels=[],stack_size=0,fp=0}).

%% Putting boilerplate for built in functions here because it clutters the mips file

gen_op('+',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'add.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_add,{Src_1,Src_2,Dest}})
  end;

gen_op('+',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 == R12+1
                                                            andalso R21 == R22+1
                                                            andalso R31 == R32+1 ->
      {ok,{'add.d',R31,R11,R21},Dest_Context};
    _ -> error({float_add,{Src_1,Src_2,Dest}})
  end;

gen_op('-',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'sub.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_sub,{Src_1,Src_2,Dest}})
  end;

gen_op('-',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 == R12+1
                                                            andalso R21 == R22+1
                                                            andalso R31 == R32+1 ->
      {ok,{'sub.d',R31,R11,R21},Dest_Context};
    _ -> error({float_sub,{Src_1,Src_2,Dest}})
  end;

gen_op('*',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'mul.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_mul,{Src_1,Src_2,Dest}})
  end;

gen_op('*',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 == R12+1
                                                            andalso R21 == R22+1
                                                            andalso R31 == R32+1 ->
      {ok,{'mul.d',R31,R11,R21},Dest_Context};
    _ -> error({float_mul,{Src_1,Src_2,Dest}})
  end;

gen_op('/',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'div.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_div,{Src_1,Src_2,Dest}})
  end;

gen_op('/',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 == R12+1
                                                            andalso R21 == R22+1
                                                            andalso R31 == R32+1 ->
      {ok,{'div.d',R31,R11,R21},Dest_Context};
    _ -> error({float_div,{Src_1,Src_2,Dest}})
  end;

%% Is the size bit neccessary?
%  Or the int bit?
gen_op('&&',_,Size,Reg_1,Reg_2,Reg_3,Context) when 32 >= Size ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},_,_} -> error({'&&',float});
    {_,{f,_},_} -> error({'&&',float});
    {_,_,{f,_}} -> error({'&&',float});
    _ -> {ok,[{sltiu,Dest,Src_1,1},{xori,Dest,Dest,1},{movz,{i,0},Src_2,Dest}],Dest_Context}
  end;

gen_op('||',_,Size,Reg_1,Reg_2,Reg_3,Context) when 32 >= Size ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = mips:get_reg(Reg_3,{0,i,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},_,_} -> error({'||',float});
    {_,{f,_},_} -> error({'||',float});
    {_,_,{f,_}} -> error({'||',float});
    _ -> {ok,[{'or',Dest,Src_1,Src_2},{sltu,Dest,{i,0},Dest}],Dest_Context}
  end;




gen_op(Op,_,_,_,_,_,_) -> error({no_mips,Op}).
