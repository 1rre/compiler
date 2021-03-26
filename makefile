ERL_FILES = src/c_compiler.erl .build/lexer.erl .build/parser.erl src/type_enum.erl src/ir_vm.erl src/codegen/ir.erl src/codegen/mips/mips.erl src/codegen/mips/mips_op.erl src/mips_io.erl
ESCRIPT_FILES = ["c_compiler.beam","lexer.beam","parser.beam","type_enum.beam","ir_vm.beam","ir.beam","mips.beam","mips_io.beam","mips_op.beam"]
ESCRIPT_CMD = 'c:cd(".build/"), escript:create("../bin/c_compiler",[shebang,{archive,$(ESCRIPT_FILES),[]}]), halt(0)'

.PHONY: all
all: bin/c_compiler

bin/c_compiler: .build/ bin/ .build/parser.erl .build/lexer.erl
	erlc -W0 -I include/ -o .build/ -DTARGET_ARCH=mips32 $(ERL_FILES)
	erl -noinput -eval $(ESCRIPT_CMD)
	chmod a+x bin/c_compiler

bin/:
	mkdir -p bin

.build/:
	mkdir -p .build

.build/parser.erl:
	erlc -W0 -o .build/parser.erl src/parsing/parser.yrl

.build/lexer.erl:
	erlc -W0 -o .build/lexer.erl src/parsing/lexer.xrl

.PHONY: clean
clean:
	rm -rf .build bin .test
	rm -f src/parsing/parser.erl src/parsing/lexer.erl
