-module(mips_op).
-export([gen_op/7]).

%% Putting boilerplate for built in functions here because it clutters the mips file

gen_op('+',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'add.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_add,{Src_1,Src_2,Dest}})
  end;

gen_op('+',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 = R12+1
                                                            andalso R21 = R22+1
                                                            andalso R31 = R32+1 ->
      {ok,{'add.d',R31,R11,R21},Dest_Context};
    _ -> error({float_add,{Src_1,Src_2,Dest}})
  end;

gen_op('-',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'sub.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_sub,{Src_1,Src_2,Dest}})
  end;

gen_op('-',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 = R12+1
                                                            andalso R21 = R22+1
                                                            andalso R31 = R32+1 ->
      {ok,{'sub.d',R31,R11,R21},Dest_Context};
    _ -> error({float_sub,{Src_1,Src_2,Dest}})
  end;

gen_op('*',f,32,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,32},Context),
  case {Src_1,Src_2,Dest} of
    {{f,_},{f,_},{f,_}} -> {ok,{'mul.s',Dest,Src_1,Src_2},Dest_Context};
    _ -> error({float_mul,{Src_1,Src_2,Dest}})
  end;

gen_op('*',f,64,Reg_1,Reg_2,Reg_3,Context) ->
  Src_1 = maps:get(Reg_1,Context#context.reg),
  Src_2 = maps:get(Reg_2,Context#context.reg),
  {ok,Dest,Dest_Context} = get_reg(Reg_3,{0,f,64},Context),
  case {Src_1,Src_2,Dest} of
    % Make sure all registers are consecutive double registers
    {[{f,R11},{f,R12}],[{f,R21},{f,R22}],[{f,R31},{f,R32}]} when R11 = R12+1
                                                            andalso R21 = R22+1
                                                            andalso R31 = R32+1 ->
      {ok,{'mul.d',R31,R11,R21},Dest_Context};
    _ -> error({float_mul,{Src_1,Src_2,Dest}})
  end;
