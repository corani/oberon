#include <iostream>
#include <fstream>
#include "lexer.h"

using namespace std;

ostream &operator<<(ostream &out, Location loc) {
    out << "@(line: " << loc.line << ", column: " << loc.column << ")";
    return out;
}

int main(void) {
    ifstream f("test.obr", ifstream::in);
    if (f.is_open()) {
        Lexer l(&f);

        Token t;
        while ((t = l.nextToken()).getKind() != Token::END_OF_FILE) {
            switch(t.getKind()) {
            case Token::STRLITERAL:
                cout << "STR: " << t.getText();
                break;
            case Token::INTLITERAL:
                cout << "INT: " << t.getIntVal();
                break;
            case Token::FLOATLITERAL:
                cout << "FLOAT: " << t.getFloatVal();
                break;
            case Token::IDENTIFIER:
                cout << "IDENTIFIER: " << t.getText();
                break;
            case Token::OPERATOR:
                cout << "OPERATOR: " << t.getText();
                break;
            case Token::RELATION:
                cout << "RELATION: " << t.getText();
                break;
            case Token::SEMICOLON:
                cout << "SEMICOLON";
                break;
            case Token::ASSIGNMENT:
                cout << "ASSIGNMENT";
                break;
            case Token::COLON:
                cout << "COLON";
                break;
            case Token::DOT:
                cout << "DOT";
                break;
            case Token::RANGE:
                cout << "RANGE";
                break;
            case Token::LPAREN:
                cout << "LPAREN";
                break;
            case Token::RPAREN:
                cout << "RPAREN";
                break;
            case Token::LCURLY:
                cout << "LCURLY";
                break;
            case Token::RCURLY:
                cout << "RCURLY";
                break;
            case Token::LSQUARE:
                cout << "LSQUARE";
                break;
            case Token::RSQUARE:
                cout << "RSQUARE";
                break;
            case Token::PIPE:
                cout << "PIPE";
                break;
            case Token::CARET:
                cout << "CARET";
                break;
            case Token::MODULE:
            case Token::IMPORT:
            case Token::BEGIN:
            case Token::END:
            case Token::EXTERN:
            case Token::PROCEDURE:
            case Token::EXIT:
            case Token::RETURN:
            case Token::VAR:
            case Token::CONST:
            case Token::TYPE:
            case Token::ARRAY:
            case Token::RECORD:
            case Token::POINTER:
            case Token::OF:
            case Token::TO:
            case Token::IF:
            case Token::THEN:
            case Token::ELSIF:
            case Token::ELSE:
            case Token::CASE:
            case Token::WITH:
            case Token::REPEAT:
            case Token::UNTIL:
            case Token::WHILE:
            case Token::DO:
            case Token::FOR:
            case Token::BY:
            case Token::LOOP:
                cout << t.getText();
                break;
            default:
                cout << "OTHER: " << t.getText();
            }
            cout << "\t" << t.getLocation() << endl;
        }

        f.close();
    }
    return 0;
}
