#include <string>
#include <map>
#include "lexer.h"

using namespace std;

map<string, Token::Kind> str_to_tok = {
    {"MODULE",    Token::MODULE},
    {"BEGIN",     Token::BEGIN},
    {"END",       Token::END},
    {"PROCEDURE", Token::PROCEDURE},
    {"RETURN",    Token::RETURN},
    {"VAR",       Token::VAR},
    {"CONST",     Token::CONST},
    {"TYPE",      Token::TYPE},
    {"IF",        Token::IF},
    {"THEN",      Token::THEN},
    {"ELSIF",     Token::ELSIF},
    {"ELSE",      Token::ELSE},
    {"REPEAT",    Token::REPEAT},
    {"UNTIL",     Token::UNTIL},
    {"WHILE",     Token::WHILE},
    {"DO",        Token::DO}
};

Token::Token(Kind _kind, Location _loc, string _text) : kind(_kind), loc(_loc), text(_text) {
    if (_kind == IDENTIFIER) {
        // uppercase each character in-place
        for (auto &c : _text) {
            c = toupper(c);
        }
        if (auto nk = str_to_tok[_text]) {
            kind = nk;
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
    case '<':
    case '>':
    case '=':
    case '\\':
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
    } else if (lastChar == ';') {
        take();
        return Token(Token::SEMICOLON, loc);
    } else if (lastChar == ':') {
        take();
        if (lastChar == '=') {
            take();
            return Token(Token::ASSIGNMENT, loc);
        } else {
            return Token(Token::COLON, loc);
        }
    } else if (lastChar == '"') {
        take(); // open "
        string str;
        while (lastChar != '"') {
            str.push_back(lastChar);
            take();
        }
        take(); // close "
        return Token(Token::STRLITERAL, loc, str);
    } else if (lastChar == '(') {
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
    } else if (lastChar == ')') {
        take();
        return Token(Token::RPAREN, loc);
    } else if (lastChar == '{') {
        take();
        return Token(Token::LCURLY, loc);
    } else if (lastChar == '}') {
        take();
        return Token(Token::RCURLY, loc);
    } else if (lastChar == '.') {
        take();
        if (lastChar == '.') {
            take();
            return Token(Token::RANGE, loc);
        } else {
            return Token(Token::DOT, loc);
        }
    } else {
        take();
    }

    return Token(Token::OTHER, loc);
}
