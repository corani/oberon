#include <vector>
#include <algorithm>
#include "parser.h"

using namespace std;

const char* ParserException::what() const noexcept {
    return msg.c_str();
}

Token Parser::accept() {
    Token old = currentToken;
    currentToken = lexer->nextToken();
    return old;
}

Token Parser::expect() {
    return currentToken;
}

Token Parser::accept(Token::Kind kind) {
    if (currentToken.getKind() == kind) {
        return accept();
    }
    throw ParserException("Expected " + kind_to_string(kind) + ", got " + kind_to_string(currentToken.getKind()), currentToken.getLocation());
}

Token Parser::expect(Token::Kind kind) {
    if (currentToken.getKind() == kind) {
        return currentToken;
    } 
    throw ParserException("Expected " + kind_to_string(kind) + ", got " + kind_to_string(currentToken.getKind()), currentToken.getLocation());
}

Token Parser::accept(vector<Token::Kind> kinds) {
    string str_kinds;
    for (Token::Kind kind : kinds) {
        if (currentToken.getKind() == kind) {
            return accept();
        }
        str_kinds += kind_to_string(kind) + ", ";
    }
    throw ParserException("Expected one of " + str_kinds + "got " + kind_to_string(currentToken.getKind()), currentToken.getLocation());
}

Token Parser::expect(vector<Token::Kind> kinds) {
    string str_kinds;
    for (Token::Kind kind : kinds) {
        if (currentToken.getKind() == kind) {
            return currentToken;
        }
        str_kinds += kind_to_string(kind) + ", ";
    }
    throw ParserException("Expected one of " + str_kinds + "got " + kind_to_string(currentToken.getKind()), currentToken.getLocation());
}

shared_ptr<ModuleAST> Parser::parseModule(shared_ptr<Lexer> _lexer) {
    lexer = _lexer;
    accept();
    accept(Token::MODULE);
    Token name = accept(Token::IDENTIFIER);
    accept(Token::SEMICOLON);
    Token keyword = expect({ Token::IMPORT, Token::TYPE, Token::CONST, Token::VAR, Token::PROCEDURE, Token::BEGIN, Token::END });
    if (keyword.getKind() == Token::IMPORT) {
        parseImport();
        keyword = expect({ Token::TYPE, Token::CONST, Token::VAR, Token::PROCEDURE, Token::BEGIN, Token::END });
    }
    while (keyword.getKind() != Token::END) {
        switch (keyword.getKind()) {
        case Token::TYPE:
            parseTypeDecl();
            break;
        case Token::CONST:
            parseConstDecl();
            break;
        case Token::VAR:
            parseVarDecl();
            break;
        case Token::PROCEDURE:
            parseProcDecl();
            break;
        case Token::BEGIN:
            parseStatementSeq();
            break;
        }
        keyword = expect({ Token::TYPE, Token::CONST, Token::VAR, Token::PROCEDURE, Token::BEGIN, Token::END });
    }
    accept(Token::IDENTIFIER);
    accept(Token::DOT);

    return nullptr;
}

void Parser::parseImport() {
    accept(Token::IMPORT);
}

void Parser::parseTypeDecl() {
    accept(Token::TYPE);
}

void Parser::parseConstDecl() {
    accept(Token::CONST);
}

void Parser::parseVarDecl() {
    accept(Token::VAR);
}

void Parser::parseForwardDecl() {
    accept(Token::CARET);
}

void Parser::parseProcDecl() {
    accept(Token::PROCEDURE);
    if (currentToken.getKind() == Token::CARET) {
        parseForwardDecl();
    } else {
        // ...
    }
}

void Parser::parseStatementSeq() {
    accept(Token::BEGIN);
    // ...
    accept(Token::END);
}

