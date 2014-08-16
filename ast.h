#pragma once

#include <memory>
#include <string>

using std::shared_ptr;

class BaseAST {
};

class ModuleAST : public BaseAST {
};

class ExprAST : public BaseAST {
};

class NumberExprAST : public ExprAST {

};

class UnExprAST : public ExprAST {
public:
    UnExprAST(std::string op, shared_ptr<ExprAST> operand)
            : op(op), operand(operand) {}
private:
    std::string op;
    shared_ptr<ExprAST> operand;
};

class BinExprAST : public ExprAST {
public:
    BinExprAST(std::string op, shared_ptr<ExprAST> LHS, shared_ptr<ExprAST> RHS)
            : op(op), lhs(LHS), rhs(RHS) {}
private:
    std::string op;
    shared_ptr<ExprAST> lhs, rhs;
};

class FactorAST : public ExprAST {
};
