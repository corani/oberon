#include <iostream>
#include <fstream>
#include "parser.h"

using namespace std;

int main(void) {
    ifstream f("test.obr", ifstream::in);
    if (f.is_open()) {
        Parser parser;
        auto module = parser.parseModule(make_shared<Lexer>(&f));
        module->print(cout);

        f.close();
    }
    return 0;
}
