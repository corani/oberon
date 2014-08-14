#include <string>
#include <map>
#include "lexer.h"

using namespace std;

map<string, Token::Kind> str_to_tok = {
    {"MODULE",      Token::MODULE},
    {"IMPORT",      Token::IMPORT},
    {"BEGIN",       Token::BEGIN},
    {"END",         Token::END},
    {"PROCEDURE",   Token::PROCEDURE},
    {"EXIT",        Token::EXIT},
    {"RETURN",      Token::RETURN},
    {"VAR",         Token::VAR},
    {"CONST",       Token::CONST},
    {"TYPE",        Token::TYPE},
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

Token::Token(Kind _kind, Location _loc, string _text) : kind(_kind), loc(_loc), text(_text) {
    if (_kind == IDENTIFIER) {
        // uppercase each character in-place
        for (auto &c : _text) {
            c = toupper(c);
        }
        if (auto nk = str_to_tok[_text]) {
            kind = nk;
        } else if (_text == "OR" || _text == "DIV" || _text == "MOD") {
            kind = OPERATOR;
            text = _text;
        } else if (_text == "IN" || _text == "IS") {
            kind = RELATION;
            text = _text;
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
    lastChar = in->get();
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

Token Lexer::nextToken() {
    while (isspace(lastChar)) {
        take();
    }

    if (lastChar == EOF) {
        return Token(Token::END_OF_FILE, loc);
    }

    if (isalpha(lastChar)) {
        string text;
        do {
            text.push_back(lastChar);
            take();
        } while (isalnum(lastChar));
        return Token(Token::IDENTIFIER, loc, text);
    } else if (isdigit(lastChar)) {
        string numStr;
        bool hasDot;
        do {
            if (lastChar == '.') hasDot = true;
            numStr.push_back(lastChar);
            take();
        } while (isdigit(lastChar) || (lastChar == '.' && !hasDot));
        if (hasDot) {
            return Token(Token::FLOATLITERAL, loc, atof(numStr.c_str()));
        } else {
            return Token(Token::INTLITERAL, loc, atoi(numStr.c_str()));
        }
    } else if (isoper(lastChar)) {
        string opStr;
        do {
            opStr.push_back(lastChar);
            take();
        } while (isoper(lastChar));
        return Token(Token::OPERATOR, loc, opStr);
    } else if (isrelation(lastChar)) {
        string relStr;
        do {
            relStr.push_back(lastChar);
            take();
        } while (isrelation(lastChar));
        return Token(Token::RELATION, loc, relStr);
    } else {
        switch (lastChar) {
            case '^':
                take();
                return Token(Token::CARET, loc);
            case '|':
                take();
                return Token(Token::PIPE, loc);
            case ';':
                take();
                return Token(Token::SEMICOLON, loc);
            case ':':
                take();
                if (lastChar == '=') {
                    take();
                    return Token(Token::ASSIGNMENT, loc);
                } else {
                    return Token(Token::COLON, loc);
                }
            case '"': {
                take(); // open "
                string str;
                while (lastChar != '"') {
                    str.push_back(lastChar);
                    take();
                }
                take(); // close "
                return Token(Token::STRLITERAL, loc, str);
            }
            case '(': {
                take(); // open (
                // it's a comment
                if (lastChar == '*') {
                    bool star = false;
                    while(1) {
                        take();
                        if (lastChar == '*') {
                            star = true;
                        } else if (star && lastChar == ')') {
                            break;
                        } else {
                            star = false;
                        }
                    }
                    take();
                    return nextToken();
                } else {
                    return Token(Token::LPAREN, loc);
                }
            }
            case ')':
                take();
                return Token(Token::RPAREN, loc);
            case '{':
                take();
                return Token(Token::LCURLY, loc);
            case '}':
                take();
                return Token(Token::RCURLY, loc);
            case '[':
                take();
                return Token(Token::LSQUARE, loc);
            case ']':
                take();
                return Token(Token::RSQUARE, loc);
            case '.':
                take();
                if (lastChar == '.') {
                    take();
                    return Token(Token::RANGE, loc);
                } else {
                    return Token(Token::DOT, loc);
                }
            default:
                take();
        }
    }

    return Token(Token::OTHER, loc);
}
