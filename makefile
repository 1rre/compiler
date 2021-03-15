ERL_FILES = src/c_compiler.erl .build/lexer.erl .build/parser.erl src/type_enum.erl src/ir_vm.erl src/codegen/ir.erl src/codegen/mips.erl src/codegen/mips/mips_op.erl src/mips_io.erl
ESCRIPT_FILES = ["c_compiler.beam","lexer.beam","parser.beam","type_enum.beam","ir_vm.beam","ir.beam","mips.beam","mips_io.beam","mips_op.beam"]
ESCRIPT_CMD = 'c:cd(".build/"), escript:create("../bin/c_compiler",[shebang,{archive,$(ESCRIPT_FILES),[]}]), halt(0)'


bin/c_compiler: clean .build .build/parser.erl .build/lexer.erl
	erlc -W0 -I include/ -o .build/ -DTARGET_ARCH=mips32 $(ERL_FILES)
	erl -noshell -pa .build/ -eval $(ESCRIPT_CMD)
	chmod a+x bin/c_compiler

.build:
	mkdir .build bin

.build/parser.erl:
	erlc -W0 -o .build/parser.erl src/parsing/parser.yrl

.build/lexer.erl:
	erlc -W0 -o .build/lexer.erl src/parsing/lexer.xrl

clean:
	rm -rf .build bin .test
