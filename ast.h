#pragma once

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <iostream>

#include "lexer.h"

using std::shared_ptr;
using std::vector;
using std::pair;
using std::string;
using std::ostream;
using std::endl;

enum class Export {
    NO, YES, RO
};

class BaseAST {
public:
    virtual void print(ostream &out, string pre = "") const {};
public:
    shared_ptr<Token> start, end;
};

class StatementAST;
class IdentDefAST;
class TypeAST;
class ExprAST;
class QualIdentAST;
class VarDeclAST;

class DeclAST : public BaseAST {
public:
    bool byRef = false;
};

class ModuleAST : public DeclAST {
public:
    ModuleAST(string name) : name(name) {}

    virtual void print(ostream &out, string pre = "") const;
public:
    string name;
    vector<pair<string, string>> imports;
    vector<shared_ptr<DeclAST>> decls;
    vector<shared_ptr<StatementAST>> stmts;
};

class ProcDeclAST : public DeclAST {
public:
    ProcDeclAST(shared_ptr<IdentDefAST> ident) : ident(ident) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<QualIdentAST> ret;
    vector<shared_ptr<VarDeclAST>> params;
    vector<shared_ptr<DeclAST>> decls;
    vector<shared_ptr<StatementAST>> stmts;
};

class ForwardDeclAST : public DeclAST {
public:
    ForwardDeclAST(shared_ptr<IdentDefAST> ident) : ident(ident) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<QualIdentAST> ret;
    vector<shared_ptr<VarDeclAST>> params;
};

class ExternDeclAST : public DeclAST {
public:
    ExternDeclAST(shared_ptr<IdentDefAST> ident) : ident(ident) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<QualIdentAST> ret;
    vector<shared_ptr<VarDeclAST>> params;
};

class TypeDeclAST : public DeclAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<TypeAST> type;
};

class TypeAST : public BaseAST {
public:
    virtual void print(ostream &out, string pre = "") const;
};

class BasicTypeAST : public TypeAST {
public:
    BasicTypeAST(shared_ptr<QualIdentAST> qid) : qid(qid) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<QualIdentAST> qid;
};

class ArrayTypeAST : public TypeAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<TypeAST> arrayOf;
};

class RecordTypeAST : public TypeAST {
public:
    virtual void print(ostream &out, string pre = "") const;
};

class PointerTypeAST : public TypeAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<TypeAST> pointee;
};

class ProcedureTypeAST : public TypeAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<QualIdentAST> ret;
    vector<shared_ptr<VarDeclAST>> params;
};

class VarDeclAST : public DeclAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<TypeAST> type;
};

class ConstDeclAST : public DeclAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<IdentDefAST> ident;
    shared_ptr<ExprAST> expr;
};

class ReceiverAST : public DeclAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    string name, type;
};

class ExprAST : public BaseAST {
public:
    bool isConst;
};

class LiteralExprAST : public ExprAST {
};

class BoolLiteralAST : public LiteralExprAST {
public:
    BoolLiteralAST(bool value) : value(value) { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
public:
    bool value;
};

class IntLiteralAST : public LiteralExprAST {
public:
    IntLiteralAST(int value) : value(value) { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
public:
    int value;
};

class FloatLiteralAST : public LiteralExprAST {
public:
    FloatLiteralAST(double value) : value(value) { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
public:
    double value;
};

class StrLiteralAST : public LiteralExprAST {
public:
    StrLiteralAST(std::string value) : value(value) { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
public:
    std::string value;
};

class CharLiteralAST : public LiteralExprAST {
public:
    CharLiteralAST(char value) : value(value) { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
public:
    char value;
};

class NilLiteralAST : public LiteralExprAST {
public:
    NilLiteralAST() { isConst = true; }
    virtual void print(ostream &out, string pre = "") const;
};

class IdentDefAST : public ExprAST {
public:
    IdentDefAST(string name, Export export_ = Export::NO) : name(name), export_(export_) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    string name;
    Export export_;
};

class DesignatorAST : public ExprAST {
public:
    DesignatorAST(shared_ptr<QualIdentAST> qid) : qid(qid) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<QualIdentAST> qid;
};

class UnExprAST : public ExprAST {
public:
    UnExprAST(std::string op, shared_ptr<ExprAST> operand)
            : op(op), operand(operand) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    std::string op;
    shared_ptr<ExprAST> operand;
};

class BinExprAST : public ExprAST {
public:
    BinExprAST(std::string op, shared_ptr<ExprAST> LHS, shared_ptr<ExprAST> RHS)
            : op(op), lhs(LHS), rhs(RHS) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    std::string op;
    shared_ptr<ExprAST> lhs, rhs;
};

class FactorAST : public ExprAST {
};

class IdentifierAST : public ExprAST {
public:
    IdentifierAST(shared_ptr<DesignatorAST> des) : des(des) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<DesignatorAST> des;
};

class QualIdentAST : public ExprAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    string module, name;
};

class StatementAST : public BaseAST {
public:
    virtual void print(ostream &out, string pre = "") const = 0;
};

class IfStatementAST : public StatementAST {
public:
    IfStatementAST(shared_ptr<ExprAST> cond) : cond(cond) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<ExprAST> cond;
    vector<shared_ptr<StatementAST>> thenStmts;
    vector<shared_ptr<StatementAST>> elseStmts;
};

class CaseClauseAST : public BaseAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    vector<pair<shared_ptr<ExprAST>, shared_ptr<ExprAST>>> when;
    vector<shared_ptr<StatementAST>> stmts;
};

class CaseStatementAST : public StatementAST {
public:
    CaseStatementAST(shared_ptr<ExprAST> cond) : cond(cond) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<ExprAST> cond;
    vector<shared_ptr<CaseClauseAST>> clauses;
    vector<shared_ptr<StatementAST>> elseStmts;
};

class WhileStatementAST : public StatementAST {
public:
    WhileStatementAST(shared_ptr<ExprAST> cond) : cond(cond) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<ExprAST> cond;
    vector<shared_ptr<StatementAST>> stmts;
};

class RepeatStatementAST : public StatementAST {
public:
    RepeatStatementAST() {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<ExprAST> cond;
    vector<shared_ptr<StatementAST>> stmts;
};

class ForStatementAST : public StatementAST {
public:
    ForStatementAST() {}
    virtual void print(ostream &out, string pre = "") const;
public:
    string iden;
    shared_ptr<ExprAST> from, to, by;
    vector<shared_ptr<StatementAST>> stmts;
};

class LoopStatementAST : public StatementAST {
public:
    LoopStatementAST() {}
    virtual void print(ostream &out, string pre = "") const;
public:
    vector<shared_ptr<StatementAST>> stmts;
};

class WithClauseAST : public BaseAST {
public:
    WithClauseAST() {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<QualIdentAST> name, type;
    vector<shared_ptr<StatementAST>> stmts;
};

class WithStatementAST : public StatementAST {
public:
    WithStatementAST() {}
    virtual void print(ostream &out, string pre = "") const;
public:
    vector<shared_ptr<WithClauseAST>> clauses;
    vector<shared_ptr<StatementAST>> elseStmts;
};

class ExitStatementAST : public StatementAST {
public:
    virtual void print(ostream &out, string pre = "") const;
};

class ReturnStatementAST : public StatementAST {
public:
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<ExprAST> expr;
};

class AssignStatementAST : public StatementAST {
public:
    AssignStatementAST(shared_ptr<DesignatorAST> des) : des(des) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<DesignatorAST> des;
    shared_ptr<ExprAST> expr;
};

class CallStatementAST : public StatementAST {
public:
    CallStatementAST(shared_ptr<DesignatorAST> des) : des(des) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<DesignatorAST> des;
    vector<shared_ptr<ExprAST>> args;
};

class CallExprAST : public ExprAST {
public:
    CallExprAST(shared_ptr<CallStatementAST> call) : call(call) {}
    virtual void print(ostream &out, string pre = "") const;
public:
    shared_ptr<CallStatementAST> call;
};
