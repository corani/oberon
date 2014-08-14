all: obc

test: test/test.cpp lexer.o
	clang++ -rdynamic -g -O3 --std=c++11 -I. -o test/test test/test.cpp lexer.o `llvm-config --cppflags --ldflags --libs core jit native bitwriter`

obc: obc.cpp lexer.o context.o ast.o parser.o
	clang++ -rdynamic -g -O3 --std=c++11 -o obc obc.cpp lexer.o context.o ast.o parser.o `llvm-config --cppflags --ldflags --libs core jit native bitwriter` -lboost_program_options

lexer.o: lexer.cpp lexer.h
	clang++ --std=c++11 -c lexer.cpp -o lexer.o

clean:
	-rm -f obc
	-rm -f test/test
	-rm -f *.bc *.o *.ll *.so
