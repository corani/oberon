#pragma once

#include <string>
#include <map>
#include <vector>
#include <stack>
#include <memory>
#include <exception>
#include "lexer.h"
#include "ast.h"

using std::shared_ptr;
using std::vector;
using std::pair;
using std::string;

class ParserException : public std::exception {
public:
    ParserException(string msg, Location loc) : msg(msg), loc(loc) {}
    virtual const char* what() const noexcept;
private:
    string msg;
    Location loc;
};

class Parser {
public:
    Parser();
    shared_ptr<ModuleAST> parseModule(shared_ptr<Lexer> lexer);
private:
    void push(shared_ptr<Token> tok);

    shared_ptr<Token> pop();
    shared_ptr<Token> pop(Token::Kind kind);
    shared_ptr<Token> pop(std::vector<Token::Kind> kinds);

    shared_ptr<Token> peek();
    shared_ptr<Token> peek(Token::Kind kind);
    shared_ptr<Token> peek(std::vector<Token::Kind> kinds);

    void enterScope();
    void leaveScope();
    void newSymbol(string name, shared_ptr<DeclAST> ast);
    shared_ptr<DeclAST> findSymbol(string name);

    void parseImport(vector<pair<string, string>> &imports);
    void parseTypeDecl   (vector<shared_ptr<DeclAST>> &decls);
    void parseConstDecl  (vector<shared_ptr<DeclAST>> &decls);
    void parseVarDecl    (vector<shared_ptr<DeclAST>> &decls);
    void parseExternDecl (vector<shared_ptr<DeclAST>> &decls);
    void parseForwardDecl(shared_ptr<Token> start, vector<shared_ptr<DeclAST>> &decls);
    void parseProcDecl   (vector<shared_ptr<DeclAST>> &decls);
    void parseStatementSeq(vector<shared_ptr<StatementAST>> &stmts);
    void parseWithGuard(vector<shared_ptr<WithClauseAST>> &clauses);
    void parseCase(vector<shared_ptr<CaseClauseAST>> &clauses);
    shared_ptr<ReceiverAST> parseReceiver();
    shared_ptr<QualIdentAST> parseFormalParams(vector<shared_ptr<VarDeclAST>> &params);

    shared_ptr<TypeAST> parseType();
    shared_ptr<ExprAST> parseConstExpr();
    shared_ptr<ExprAST> parseExpr();
    shared_ptr<ExprAST> parseUnaryExpr();
    shared_ptr<ExprAST> parseBinOpRHS(shared_ptr<ExprAST> LHS, int prec);
    shared_ptr<ExprAST> parseFactor();
    shared_ptr<DesignatorAST> parseDesignator();
    shared_ptr<IdentDefAST> parseIdentDef();
    shared_ptr<QualIdentAST> parseQualIdent();

    int getPrecedence(shared_ptr<Token> op);
private:
    shared_ptr<Lexer> lexer;
    shared_ptr<Token> currentToken;
    std::stack<shared_ptr<Token>> tokens;
    std::map<string, int> precedence;
    vector<vector<pair<string, shared_ptr<DeclAST>>>> symbols;
    int scope = -1;

    shared_ptr<TypeAST> parseSimpleType(shared_ptr<Token> start);

    shared_ptr<TypeAST> parseArrayType(shared_ptr<Token> start);

    shared_ptr<TypeAST> parseRecordType(shared_ptr<Token> start);

    shared_ptr<TypeAST> parsePointerType(shared_ptr<Token> start);

    shared_ptr<TypeAST> parseProcedureType(shared_ptr<Token> start);

    shared_ptr<ReturnStatementAST> parseReturnStatement();

    shared_ptr<ExitStatementAST> parseExitStatement();

    shared_ptr<WithStatementAST> parseWithStatement();

    shared_ptr<LoopStatementAST> parseLoopStatement();

    shared_ptr<ForStatementAST> parseForStatement();

    shared_ptr<RepeatStatementAST> parseRepeatStatement();

    shared_ptr<WhileStatementAST> parseWhileStatement();

    shared_ptr<CaseStatementAST> parseCaseStatement();

    shared_ptr<IfStatementAST> parseIfStatement();

    shared_ptr<CallStatementAST> parseNakedCallStatement(shared_ptr<DesignatorAST> des);

    shared_ptr<CallStatementAST> parseCallStatement(shared_ptr<DesignatorAST> des);

    shared_ptr<AssignStatementAST> parseAssignmentStatement(shared_ptr<DesignatorAST> des);
};
