#include <iostream>
#include <fstream>
#include "parser.h"

using namespace std;

int main(void) {
    ifstream f("test.obr", ifstream::in);
    ifstream std("../std.mod", ifstream::in);
    if (f.is_open() && std.is_open()) {
        Parser parser;
        parser.parseModule(make_shared<Lexer>(&std));
        auto module = parser.parseModule(make_shared<Lexer>(&f));
        module->print(cout);

        f.close();
    }
    return 0;
}
