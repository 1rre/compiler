-module(mips).
-export([generate/1]).

-include("arch_type_consts.hrl").

%% TODO: Replace this with a more relevant one
-record(context,{fn=main,global=#{},types=#{},reg=[],indent=0,
                 args=[],stack= <<>>,s_reg=[],fp=0}).

generate(Ir) ->
  {ok,[element(2,generate(St,#context{})) || St <- Ir]}.

generate([],Context) ->
  {ok,[]};

generate({function,R_Type,Name,Args,St},Context) ->
  Globl = io_lib:format(".globl ~s~n",[Name]),
  Indent = 7 + Context#context.indent,
  io:fwrite(Globl),
  print_label(Indent,Name),
  N_Indent = Indent+2,
  Args_Size = lists:sum([sizeof(Arg) || Arg <- Args]),
  print_instr(N_Indent,addiu,'$29','$29',integer_to_list(Args_Size div -8 - 4)),
  generate(St, Context#context{indent=N_Indent,args=Args});

generate([return|Rest],Context) ->
  Indent = Context#context.indent,
  Args = Context#context.args,
  Args_Size = lists:sum([sizeof(Arg) || Arg <- Args]),
  print_instr(Indent,addiu,'$29','$29',integer_to_list(Args_Size div 8 + 4)),
  print_instr(Indent,jr,'$31'),
  print_instr(Indent,nop),
  generate(Rest,Context);

generate([{allocate,N_Bits}|Rest],Context) ->
  Indent = Context#context.indent,
  print_instr(Indent,addiu,'$29','$29',integer_to_list(N_Bits div 8)),
  generate(Rest,Context);

generate([{move,{i,N},D}|Rest],Context) ->
  Indent = Context#context.indent,
  Rd = get_reg_mapping(D),
  print_instr(Indent,li,Rd,integer_to_list(N)),
  generate(Rest,Context);

generate([{gc,N}|Rest],Context) ->
  Indent = Context#context.indent,
  Fp_Add = integer_to_list(Context#context.fp*4),
  print_instr(Indent,addiu,'$29','$29',Fp_Add),
  generate(Rest,Context);

%% Assuming int for now, TODO: Floats
generate([{'+',[A,B],C}|Rest],Context) ->
  Indent = Context#context.indent,
  Ra = get_reg_mapping(A),
  Rb = get_reg_mapping(B),
  Rc = get_reg_mapping(C),
  print_instr(Indent,addu,Rc,Ra,Rb),
  generate(Rest,Context);

generate(St,Context) ->
  error({unknown_st,{St,Context}}).




%% General use registers
get_reg_mapping({x,00}) -> '$2';  % v0
get_reg_mapping({x,01}) -> '$3';  % v1
get_reg_mapping({x,02}) -> '$8';  % t0
get_reg_mapping({x,03}) -> '$9';  % t1
get_reg_mapping({x,04}) -> '$10'; % t2
get_reg_mapping({x,05}) -> '$11'; % t3
get_reg_mapping({x,06}) -> '$12'; % t4
get_reg_mapping({x,07}) -> '$13'; % t5
get_reg_mapping({x,08}) -> '$14'; % t6
get_reg_mapping({x,09}) -> '$15'; % t7
get_reg_mapping({x,10}) -> '$24'; % t8
get_reg_mapping({x,11}) -> '$25'; % t9
%% Saved registers (TODO: Save these if we ever have to use them)
get_reg_mapping({x,12}) -> '$16'; % t2
get_reg_mapping({x,13}) -> '$17'; % t3
get_reg_mapping({x,14}) -> '$18'; % t4
get_reg_mapping({x,15}) -> '$19'; % t5
get_reg_mapping({x,16}) -> '$20'; % t6
get_reg_mapping({x,17}) -> '$21'; % t7
get_reg_mapping({x,18}) -> '$22'; % t8
get_reg_mapping({x,19}) -> '$23'; % t9

get_reg_mapping(Reg) -> error({reg_not_mapped,Reg}).



print_label(Indent,L) ->
  io:fwrite("~*s:~n",[Indent+length(atom_to_list(L)),L]).

print_instr(Indent,Op) ->
  io:fwrite("~*s~n",[5+Indent,Op]).

print_instr(Indent,Op,A1) ->
  io:fwrite("~*s ~4s~n",[Indent+5,Op,A1]).

print_instr(Indent,Op,A1,A2) ->
  io:fwrite("~*s ~4s,~4s~n",[Indent+5,Op,A1,A2]).

print_instr(Indent,Op,A1,A2,A3) ->
  io:fwrite("~*s ~4s,~4s,~4s~n",[Indent+5,Op,A1,A2,A3]).


sizeof({0,_,S}) -> S;
sizeof({N,_,_}) -> ?SIZEOF_POINTER;
sizeof(T) -> error({unknown_t,T}).
