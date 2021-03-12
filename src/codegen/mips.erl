-module(mips).
-export([generate/2]).

-include("arch_type_consts.hrl").

%% TODO: Replace this with a more relevant one
-record(context,{fn=#{},global=#{},args=#{},indent=0,io=standard_io,types=#{},fp=0,s_reg=[],reg=[]}).

generate(Ir,{file,File}) ->
  Iostream = case file:open(File, [write]) of
    {error,_} -> standard_io;%error({eonent,File});
    {ok, Io} -> Io
  end,
  [element(2,generate(St,#context{io=Iostream})) || St <- Ir],
  file:close(Iostream),
  [];

generate([],Context) ->
  [];

generate({global,Type,Name,Data},Context) ->
  {ok,St} = generate(Data,Context),
  io_lib:format(".globl ~s~n",[Name]),
  format_label(Context#context.indent+7,Name),

  %TODO: Finish
  [];


generate({function,R_Type,Name,Args,St},Context) ->
  io_lib:format(".globl ~s~n",[Name]),
  Indent = 7 + Context#context.indent,
  format_label(Indent,Name),
  N_Indent = Indent+2,
  Args_Size = lists:sum([sizeof(Arg) || Arg <- Args]),
  [format_instr(N_Indent,addiu,'$29','$29',integer_to_list(Args_Size div -8 - 4))|
   generate(St, Context#context{indent=N_Indent,args=Args})];

generate([return|Rest],Context) ->
  Indent = Context#context.indent,
  Args = Context#context.args,
  Args_Size = lists:sum([sizeof(Arg) || Arg <- Args]),
  [format_instr(Indent,addiu,'$29','$29',integer_to_list(Args_Size div 8 + 4)),
   format_instr(Indent,jr,'$31'),
   format_instr(Indent,nop)|generate(Rest,Context)];

generate([{allocate,N_Bits}|Rest],Context) ->
  Indent = Context#context.indent,
  [format_instr(Indent,addiu,'$29','$29',integer_to_list(N_Bits div 8)),
   generate(Rest,Context)];

generate([{move,{i,N},D}|Rest],Context) ->
  Indent = Context#context.indent,
  Rd = get_reg_mapping(D),
  [format_instr(Indent,li,Rd,integer_to_list(N))|
   generate(Rest,Context)];

generate([{gc,N}|Rest],Context) ->
  Indent = Context#context.indent,
  Fp_Add = integer_to_list(Context#context.fp*4),
  [format_instr(Indent,addiu,'$29','$29',Fp_Add)|
   generate(Rest,Context)];

%% Assuming int for now, TODO: Floats
generate([{'+',[A,B],C}|Rest],Context) ->
  Indent = Context#context.indent,
  Ra = get_reg_mapping(A),
  Rb = get_reg_mapping(B),
  Rc = get_reg_mapping(C),
  [format_instr(Indent,addu,Rc,Ra,Rb)|
   generate(Rest,Context)]

generate([St|_],Context) ->
  error({unknown_st,St}).




%% General use registers
%% TODO: Replace this with better version for 64bit data
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



format_label(formatIndent,L) ->
  io:format(format"~*s:~n",[Indent+length(atom_to_list(L)),L]).

format_instr(Indent,Op) ->
  io_lib:format("~*s~n",[5+Indent,Op]).

format_instr(Indent,Op,A1) ->
  io_lib:format("~*s ~4s~n",[Indent+5,Op,A1]).

format_instr(Indent,Op,A1,A2) ->
  io_lib:format("~*s ~4s,~4s~n",[Indent+5,Op,A1,A2]).

format_instr(Indent,Op,A1,A2,A3) ->
  io_lib:format("~*s ~4s,~4s,~4s~n",[Indent+5,Op,A1,A2,A3]).


sizeof({0,_,S}) -> S;
sizeof({N,_,_}) -> ?SIZEOF_POINTER;
sizeof(T) -> error({unknown_t,T}).
