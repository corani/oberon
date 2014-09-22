#include <string>
#include <iomanip>
#include <map>
#include "lexer.h"

using namespace std;

ostream &operator<<(ostream &out, Location loc) {
    out << setfill('0') << setw(3) << (loc.line + 1) << ":" << setw(3) << (loc.column - 1) << setfill(' ');
    return out;
}

map<string, Token::Kind> str_to_tok = {
    {"MODULE",      Token::MODULE},
    {"IMPORT",      Token::IMPORT},
    {"BEGIN",       Token::BEGIN},
    {"END",         Token::END},
    {"EXTERN",      Token::EXTERN},
    {"PROCEDURE",   Token::PROCEDURE},
    {"EXIT",        Token::EXIT},
    {"RETURN",      Token::RETURN},
    {"VAR",         Token::VAR},
    {"CONST",       Token::CONST},
    {"TYPE",        Token::TYPE},
    {"NIL",         Token::NIL},
    {"ARRAY",       Token::ARRAY},
    {"RECORD",      Token::RECORD},
    {"POINTER",     Token::POINTER},
    {"OF",          Token::OF},
    {"TO",          Token::TO},
    {"IF",          Token::IF},
    {"THEN",        Token::THEN},
    {"ELSIF",       Token::ELSIF},
    {"ELSE",        Token::ELSE},
    {"CASE",        Token::CASE},
    {"WITH",        Token::WITH},
    {"REPEAT",      Token::REPEAT},
    {"UNTIL",       Token::UNTIL},
    {"WHILE",       Token::WHILE},
    {"DO",          Token::DO},
    {"FOR",         Token::FOR},
    {"BY",          Token::BY},
    {"LOOP",        Token::LOOP}
};

map<Token::Kind, string> tok_to_str = {
    {Token::INTLITERAL,     "INTLITERAL"},
    {Token::FLOATLITERAL,   "FLOATLITERAL"},
    {Token::STRLITERAL,     "STRLITERAL"},
    {Token::CHARLITERAL,    "CHARLITERAL"},
    {Token::IDENTIFIER,     "IDENTIFIER"},
    {Token::RELATION,       "RELATION"},
    {Token::OPERATOR,       "OPERATOR"},
    {Token::DOT,            "DOT"},
    {Token::RANGE,          "RANGE"},
    {Token::COMMA,          "COMMA"},
    {Token::COLON,          "COLON"},
    {Token::SEMICOLON,      "SEMICOLON"},
    {Token::ASSIGNMENT,     "ASSIGNMENT"},
    {Token::PIPE,           "PIPE"},
    {Token::CARET,          "CARET"},
    {Token::LPAREN,         "LPAREN"},
    {Token::RPAREN,         "RPAREN"},
    {Token::LCURLY,         "LCURLY"},
    {Token::RCURLY,         "RCURLY"},
    {Token::LSQUARE,        "LSQUARE"},
    {Token::RSQUARE,        "RSQUARE"},
    {Token::END_OF_FILE,    "END_OF_FILE"}
};

string kind_to_string(Token::Kind kind) {
    for (auto &entry : str_to_tok) {
        if (entry.second == kind) {
            return entry.first;
        }
    }
    return tok_to_str[kind];
}

Token::Token(Kind _kind, Location _loc, string _text) : kind(_kind), loc(_loc), text(_text) {
    if (_kind == IDENTIFIER) {
        // uppercase each character in-place
        for (auto &c : _text) {
            c = (char) toupper(c);
        }
        for (auto &entry : str_to_tok) {
            if (entry.first == _text) {
                kind = entry.second;
                return;
            }
        }
        if (_text == "OR" || _text == "DIV" || _text == "MOD") {
            kind = OPERATOR;
            text = _text;
        } else if (_text == "IN" || _text == "IS") {
            kind = RELATION;
            text = _text;
        } else if (_text == "FALSE") {
            kind = BOOLLITERAL;
            boolval = false;
        } else if (_text == "TRUE") {
            kind = BOOLLITERAL;
            boolval = true;
        }
    }
}

void Lexer::take(void) {
    if (lastChar == '\n') {
        loc.line++;
        loc.column = 1;
    } else {
        loc.column++;
    }
    lastChar = (char) in->get();
}

static bool isoper(char c) {
    switch (c) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '&':
    case '~':
    case '\\':
        return true;
    default:
        return false;
    }
}

static bool isrelation(char c) {
    switch (c) {
    case '<':
    case '>':
    case '#':
    case '=':
        return true;
    default:
        return false;
    }
}

shared_ptr<Token> Lexer::nextToken() {
    while (isspace(lastChar)) {
        take();
    }

    if (lastChar == EOF) {
        return make_shared<Token>(Token::END_OF_FILE, loc);
    }

    if (isalpha(lastChar)) {
        string text;
        do {
            text.push_back(lastChar);
            take();
        } while (isalnum(lastChar));
        return make_shared<Token>(Token::IDENTIFIER, loc, text);
    } else if (isdigit(lastChar)) {
        string numStr;
        bool isReal = false, isHex = false, isScaled = false, isSigned = false, isDone = false, isChar = false;
        while (!isDone) {
            switch (lastChar) {
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                numStr.push_back(lastChar);
                take();
                break;
            case 'A': case 'B': case 'C': case 'F':
                isHex = true;
                numStr.push_back(lastChar);
                take();
                break;
            case 'D': case 'E':
                if (isScaled) {
                    isDone = true;
                } else if (isReal) {
                    isScaled = true;
                    numStr.push_back(lastChar);
                    take();
                } else {
                    isHex = true;
                    numStr.push_back(lastChar);
                    take();
                }
                break;
            case '+': case '-':
                if (isSigned) {
                    isDone = true;
                } else if (isScaled) {
                    isSigned = true;
                    numStr.push_back(lastChar);
                    take();
                }
                break;
            case '.':
                if (isReal) {
                    isDone = true;
                } else {
                    isReal = true;
                    numStr.push_back(lastChar);
                    take();
                }
                break;
            case 'H':
                isHex = true;
                isDone = true;
                take();
                break;
            case 'X':
                if (isReal) {
                    isDone = true;
                } else {
                    isChar = true;
                    isDone = true;
                    take();
                }
                break;
            default:
                isDone = true;
            }
        }
        if (isReal) {
            return make_shared<Token>(Token::FLOATLITERAL, loc, stof(numStr.c_str()));
        } else if (isChar) {
            return make_shared<Token>(Token::CHARLITERAL, loc, stoi(numStr.c_str(), 0, 16));
        } else if (isHex) {
            return make_shared<Token>(Token::INTLITERAL, loc, stoi(numStr.c_str(), 0, 16));
        } else {
            return make_shared<Token>(Token::INTLITERAL, loc, stoi(numStr.c_str()));
        }
    } else if (isoper(lastChar)) {
        string opStr;
        do {
            opStr.push_back(lastChar);
            take();
        } while (isoper(lastChar));
        return make_shared<Token>(Token::OPERATOR, loc, opStr);
    } else if (isrelation(lastChar)) {
        string relStr;
        do {
            relStr.push_back(lastChar);
            take();
        } while (isrelation(lastChar));
        return make_shared<Token>(Token::RELATION, loc, relStr);
    } else {
        switch (lastChar) {
            case ',':
                take();
                return make_shared<Token>(Token::COMMA, loc);
            case '~':
                take();
                return make_shared<Token>(Token::TILDE, loc);
            case '^':
                take();
                return make_shared<Token>(Token::CARET, loc);
            case '|':
                take();
                return make_shared<Token>(Token::PIPE, loc);
            case ';':
                take();
                return make_shared<Token>(Token::SEMICOLON, loc);
            case ':':
                take();
                if (lastChar == '=') {
                    take();
                    return make_shared<Token>(Token::ASSIGNMENT, loc);
                } else {
                    return make_shared<Token>(Token::COLON, loc);
                }
            case '"': {
                take(); // open "
                string str;
                while (lastChar != '"') {
                    str.push_back(lastChar);
                    take();
                }
                take(); // close "
                if (str.length() == 1) {
                    return make_shared<Token>(Token::CHARLITERAL, loc, str[0]);
                } else {
                    return make_shared<Token>(Token::STRLITERAL, loc, str);
                }
            }
            case '\'': {
                take(); // open '
                string str;
                while (lastChar != '\'') {
                    str.push_back(lastChar);
                    take();
                }
                take(); // close '
                if (str.length() == 1) {
                    return make_shared<Token>(Token::CHARLITERAL, loc, str[0]);
                } else {
                    return make_shared<Token>(Token::STRLITERAL, loc, str);
                }
            }
            case '(': {
                take(); // open (
                // it's a comment
                if (lastChar == '*') {
                    char beforeLast = 0;
                    int depth = 1;
                    while(1) {
                        take();
                        if (beforeLast == '(' && lastChar == '*') {
                            depth++;
                        } else if (beforeLast == '*' && lastChar == ')') {
                            if (depth == 1) break;
                            depth--;
                        }
                        beforeLast = lastChar;
                    }
                    take(); // close )
                    return nextToken();
                } else {
                    return make_shared<Token>(Token::LPAREN, loc);
                }
            }
            case ')':
                take();
                return make_shared<Token>(Token::RPAREN, loc);
            case '{':
                take();
                return make_shared<Token>(Token::LCURLY, loc);
            case '}':
                take();
                return make_shared<Token>(Token::RCURLY, loc);
            case '[':
                take();
                return make_shared<Token>(Token::LSQUARE, loc);
            case ']':
                take();
                return make_shared<Token>(Token::RSQUARE, loc);
            case '.':
                take();
                if (lastChar == '.') {
                    take();
                    return make_shared<Token>(Token::RANGE, loc);
                } else {
                    return make_shared<Token>(Token::DOT, loc);
                }
            default:
                take();
        }
    }

    return make_shared<Token>(Token::OTHER, loc);
}
