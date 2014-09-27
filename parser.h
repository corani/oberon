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

class SymbolTable {
public:
    void enterScope();
    void leaveScope();
    void newSymbol(string name, shared_ptr<DeclAST> ast);
    shared_ptr<DeclAST> findSymbol(string name);
private:
    vector<vector<pair<string, shared_ptr<DeclAST>>>> symbols;
    int scope = -1;
};

class Tokenizer {
public:
    Tokenizer(shared_ptr<Lexer> _lexer);

    void push(shared_ptr<Token> tok);

    shared_ptr<Token> pop();
    shared_ptr<Token> popAny(std::vector<Token::Kind> kinds);
    shared_ptr<Token> pop(Token::Kind kind);

    shared_ptr<Token> peek();
    shared_ptr<Token> peekAny(std::vector<Token::Kind> kinds);
    shared_ptr<Token> peek(Token::Kind kind);
private:
    shared_ptr<Lexer> lexer;
    shared_ptr<Token> currentToken;
    std::stack<shared_ptr<Token>> tokens;
};
class Parser {
public:
    Parser();
    shared_ptr<ModuleAST> parseModule(shared_ptr<Lexer> lexer);
private:
    void parseImport(vector<pair<string, string>> &imports);

    void parseTypeDecl   (vector<shared_ptr<DeclAST>> &decls);

    void parseConstDecl  (vector<shared_ptr<DeclAST>> &decls);

    void parseVarDecl    (vector<shared_ptr<DeclAST>> &decls);

    void parseExternDecl (vector<shared_ptr<DeclAST>> &decls);

    void parseForwardDecl(shared_ptr<Token> start, vector<shared_ptr<DeclAST>> &decls);

    void parseProcDecl   (vector<shared_ptr<DeclAST>> &decls);

    void parseStatementSeq(vector<shared_ptr<StatementAST>> &stmts);
    shared_ptr<AssignStatementAST> parseAssignmentStatement(shared_ptr<DesignatorAST> des);
    shared_ptr<CallStatementAST> parseCallStatement(shared_ptr<DesignatorAST> des);
    shared_ptr<CallStatementAST> parseNakedCallStatement(shared_ptr<DesignatorAST> des);
    shared_ptr<IfStatementAST> parseIfStatement();
    shared_ptr<CaseStatementAST> parseCaseStatement();
    void parseCase(vector<shared_ptr<CaseClauseAST>> &clauses);
    shared_ptr<WhileStatementAST> parseWhileStatement();
    shared_ptr<RepeatStatementAST> parseRepeatStatement();
    shared_ptr<ForStatementAST> parseForStatement();
    shared_ptr<LoopStatementAST> parseLoopStatement();
    shared_ptr<WithStatementAST> parseWithStatement();
    void parseWithGuard(vector<shared_ptr<WithClauseAST>> &clauses);
    shared_ptr<ExitStatementAST> parseExitStatement();
    shared_ptr<ReturnStatementAST> parseReturnStatement();

    shared_ptr<ReceiverAST> parseReceiver();

    shared_ptr<QualIdentAST> parseFormalParams(vector<shared_ptr<VarDeclAST>> &params);

    shared_ptr<TypeAST> parseType();
    shared_ptr<TypeAST> parseSimpleType(shared_ptr<Token> start);
    shared_ptr<TypeAST> parseArrayType(shared_ptr<Token> start);
    shared_ptr<TypeAST> parseRecordType(shared_ptr<Token> start);
    shared_ptr<TypeAST> parsePointerType(shared_ptr<Token> start);
    shared_ptr<TypeAST> parseProcedureType(shared_ptr<Token> start);

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
    shared_ptr<Tokenizer> tok;
    std::map<string, int> precedence;
    SymbolTable symbolTable;
};
