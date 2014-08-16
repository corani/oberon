#pragma once

#include <string>
#include <map>
#include <vector>
#include <memory>
#include <exception>
#include "lexer.h"
#include "ast.h"

using std::shared_ptr;

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
    Parser();
    shared_ptr<ModuleAST> parseModule(shared_ptr<Lexer> lexer);
private:
    shared_ptr<Token> accept();
    shared_ptr<Token> accept(Token::Kind kind);
    shared_ptr<Token> accept(std::vector<Token::Kind> kinds);

    shared_ptr<Token> expect();
    shared_ptr<Token> expect(Token::Kind kind);
    shared_ptr<Token> expect(std::vector<Token::Kind> kinds);

    void parseImport();
    void parseTypeDecl();
    void parseConstDecl();
    void parseVarDecl();
    void parseExternDecl();
    void parseForwardDecl();
    void parseProcDecl();
    void parseStatementSeq();
    void parseCase();
    void parseReceiver();
    void parseFormalParams();
    void parseType();
    shared_ptr<ExprAST> parseConstExpr();
    shared_ptr<ExprAST> parseExpr();
    shared_ptr<ExprAST> parseUnaryExpr();
    shared_ptr<ExprAST> parseBinOpRHS(shared_ptr<ExprAST> LHS, int prec);
    shared_ptr<FactorAST> parseFactor();
    void parseDesignator();

    int getPrecedence(shared_ptr<Token> op);
private:
    std::shared_ptr<Lexer> lexer;
    std::shared_ptr<Token> currentToken;
    std::map<std::string, int> precedence;
};
