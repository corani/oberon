all: obc

test_parser: test/test_parser.cpp lexer.o ast.o parser.o
	clang++ -rdynamic -ggdb -O3 --std=c++11 -I. -o test/test_parser test/test_parser.cpp lexer.o ast.o parser.o

test_lexer: test/test_lexer.cpp lexer.o
	clang++ -rdynamic -ggdb -O3 --std=c++11 -I. -o test/test_lexer test/test_lexer.cpp lexer.o

obc: obc.cpp lexer.o context.o ast.o parser.o
	clang++ -rdynamic -ggdb -O3 --std=c++11 -o obc obc.cpp lexer.o context.o ast.o parser.o `llvm-config --cppflags --ldflags --libs core jit native bitwriter` -lboost_program_options

parser.o: parser.cpp parser.h lexer.h ast.h
	clang++ -ggdb --std=c++11 -c parser.cpp -o parser.o

ast.o: ast.cpp ast.h
	clang++ -ggdb --std=c++11 -c ast.cpp -o ast.o

lexer.o: lexer.cpp lexer.h
	clang++ -ggdb --std=c++11 -c lexer.cpp -o lexer.o

clean:
	-rm -f obc
	-rm -f test/test_lexer
	-rm -f test/test_parser
	-rm -f *.bc *.o *.ll *.so
