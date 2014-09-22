#include <iostream>
#include <fstream>
#include "../parser.h"
#include "../generator.h"

using namespace std;

int main(int argc, char**argv) {
    Generator gen("a.out");

    string name = "test_compiler.m";
    if (argc > 1) {
        name = argv[1];
    }

    ifstream f(name, ifstream::in);
    ifstream std("../std.m", ifstream::in);
    if (f.is_open() && std.is_open()) {
        Parser parser;
        parser.parseModule(make_shared<Lexer>(&std));
        auto module = parser.parseModule(make_shared<Lexer>(&f));
        GeneratorContext *ctx = gen.generate(module);
        ctx->toBitFile("a.bc");
        f.close();
    }
    return 0;
}
