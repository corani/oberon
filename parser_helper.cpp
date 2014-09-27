#include "parser_helper.h"

using namespace std;

const char* ParserException::what() const noexcept {
    cout << msg << " at " << loc << endl;
    return msg.c_str();
}

void SymbolTable::enterScope() {
    scope++;
    if (scope >= symbols.capacity()) {
        symbols.push_back({});
    }
}

void SymbolTable::leaveScope() {
    if (scope >= 0) {
        symbols[scope].clear();
        scope--;
    }
}

void SymbolTable::newSymbol(string name, shared_ptr<DeclAST> ast) {
    if (scope >= 0) {
        symbols.at((unsigned) scope).push_back(make_pair(name, ast));
    }
}

shared_ptr<DeclAST> SymbolTable::findSymbol(string name) {
    for (int i = scope; i > 0; i--) {
        auto sv = symbols[i];
        for (pair<string, shared_ptr<DeclAST>> p : sv) {
            if (p.first == name) {
                return p.second;
            }
        }
    };
    return nullptr;
}

Tokenizer::Tokenizer(shared_ptr<Lexer> _lexer) : lexer(nullptr), currentToken(nullptr) {
    lexer = _lexer;
    pop(); // Prime the pump
};

void Tokenizer::push(shared_ptr<Token> token) {
    tokens.push(currentToken);
    currentToken = token;
}

shared_ptr<Token> Tokenizer::pop() {
    shared_ptr<Token> old = currentToken;
    if (tokens.empty()) {
        currentToken = lexer->nextToken();
    } else {
        currentToken = tokens.top();
        tokens.pop();
    }
    return old;
}

shared_ptr<Token> Tokenizer::peek() {
    return currentToken;
}

shared_ptr<Token> Tokenizer::popAny(vector<Token::Kind> kinds) {
    string str_kinds;
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return pop();
        }
        str_kinds += kind_to_string(kind) + ", ";
    }
    throw ParserException("Expected one of " + str_kinds + "got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Tokenizer::pop(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return pop();
    }
    throw ParserException("Expected " + kind_to_string(kind) + ", got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Tokenizer::peekAny(vector<Token::Kind> kinds) {
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return currentToken;
        }
    }
    return nullptr;
}

shared_ptr<Token> Tokenizer::peek(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return currentToken;
    }
    return nullptr;
}