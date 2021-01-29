-module(nif_test).
-export([main/1]).
-on_load(init/0).

init() -> 
  Fileloc = filename:join(code:priv_dir(c89_compiler),"nif_test"),
  erlang:load_nif(Fileloc, 0).

cpp_get_ast(_List) ->
  error("Nif not loaded!").

main(_) ->
  Output = [{[{typedef,1},{unsigned,1},{long,1}],[{identifier,1,ulong}],{';',1}},
 {[{int,3}],{{identifier,3,main},{'(',3},{')',3}}},
 {{'{',3},
  [{[{typedef_name,4,ulong}],
    [{{identifier,4,x},{'=',4},{int_l,4,78,"ul"}}],
    {';',4}}],
  [{{{identifier,5,x},{'--',5}},{';',5}},
   {{'if',6},
    {'(',6},
    {{identifier,6,x},{'==',6},{int_l,6,78,[]}},
    {')',6},
    {{'{',6},
     [{[{int,7}],
       [{{identifier,7,ulong},
         {'=',7},
         {{identifier,7,x},{'-',7},{float_l,7,2.0e5,"f"}}}],
       {';',7}}],
     [{{return,8},{identifier,8,ulong},{';',8}}],
     {'}',9}}},
   {{return,10},{identifier,10,x},{';',10}}],
  {'}',11}}],
  io:fwrite("~p~n", [cpp_get_ast(Output)]),
  halt(0).

