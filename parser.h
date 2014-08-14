#pragma once

#include <string>
#include <vector>
#include <memory>
#include <exception>
#include "lexer.h"
#include "ast.h"

class ParserException : public std::exception {
public:
    ParserException(std::string msg, Location loc) : msg(msg), loc(loc) {}
    virtual const char* what() const noexcept;
private:
    std::string msg;
    Location loc;
};

class Parser {
public:
    Parser() : lexer(nullptr) {}
    std::shared_ptr<ModuleAST> parseModule(std::shared_ptr<Lexer> lexer);
private:
    Token accept();
    Token accept(Token::Kind kind);
    Token accept(std::vector<Token::Kind> kinds);

    Token expect();
    Token expect(Token::Kind kind);
    Token expect(std::vector<Token::Kind> kinds);

    void parseImport();
    void parseTypeDecl();
    void parseConstDecl();
    void parseVarDecl();
    void parseForwardDecl();
    void parseProcDecl();
    void parseStatementSeq();
private:
    std::shared_ptr<Lexer> lexer;
    Token currentToken;
};
