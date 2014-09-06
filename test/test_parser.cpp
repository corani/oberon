#include <iostream>
#include <fstream>
#include "parser.h"
#include "printer.h"

using namespace std;

int main(int argc, char**argv) {
    Printer p(cout);

    string name = "test.obr";
    if (argc > 1) {
        name = argv[1];
    }

    ifstream f(name, ifstream::in);
    ifstream std("../std.m", ifstream::in);
    if (f.is_open() && std.is_open()) {
        Parser parser;
        parser.parseModule(make_shared<Lexer>(&std));
        auto module = parser.parseModule(make_shared<Lexer>(&f));
        p.print(module);

        f.close();
    }
    return 0;
}
