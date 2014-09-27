#include <string>
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
