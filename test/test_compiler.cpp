#include <iostream>
#include <fstream>
#include "../parser.h"
#include "../generator.h"

using namespace std;

int main(int argc, char**argv) {
    Generator gen;

    string src_n = "test_compiler.m";
    string out_n = "a.bc";
    if (argc > 1) {
        src_n = argv[1];
    }
    if (argc > 2) {
        out_n = argv[2];
    }

    ifstream src(src_n, ifstream::in);
    ifstream std("../std.m", ifstream::in);
    if (src.is_open() && std.is_open()) {
        Parser parser;
        // parse stdlib
        auto std_mod = parser.parseModule(make_shared<Lexer>(&std));
        // parse source file
        auto src_mod = parser.parseModule(make_shared<Lexer>(&src));
        auto ctx = gen.generate(src_mod);
        ctx->toBitFile(out_n);
    }
    return 0;
}
