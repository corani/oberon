#pragma once

#include <stack>
#include <vector>
#include <memory>
#include "visitor.h"
#include "ast.h"

class PrinterContext : public Context {
public:
    void push(std::string p) {
        pres.push(pre);
        pre = pre + p;
    }
    void pop() {
        pre = pres.top();
        pres.pop();
    }
public:
    std::string pre;
private:
    std::stack<std::string> pres;
};

class Printer : public Visitor {
public:
    Printer(ostream &out) : out(out) {
        ctx = new PrinterContext();
    }
    virtual ~Printer() {
        delete ctx;
    }
    void print(shared_ptr<ModuleAST> module) {
        module->visit(this, ctx);
    }

    virtual void visitModule            (ModuleAST          *node, Context *ctx);
    virtual void visitProcDecl          (ProcDeclAST        *node, Context *ctx);
    virtual void visitForwardDecl       (ForwardDeclAST     *node, Context *ctx);
    virtual void visitExternDecl        (ExternDeclAST      *node, Context *ctx);
    virtual void visitTypeDecl          (TypeDeclAST        *node, Context *ctx);
    virtual void visitBasicType         (BasicTypeAST       *node, Context *ctx);
    virtual void visitArrayType         (ArrayTypeAST       *node, Context *ctx);
    virtual void visitRecordType        (RecordTypeAST      *node, Context *ctx);
    virtual void visitPointerType       (PointerTypeAST     *node, Context *ctx);
    virtual void visitProcedureType     (ProcedureTypeAST   *node, Context *ctx);
    virtual void visitVarDecl           (VarDeclAST         *node, Context *ctx);
    virtual void visitConstDecl         (ConstDeclAST       *node, Context *ctx);
    virtual void visitReceiver          (ReceiverAST        *node, Context *ctx);
    virtual void visitExpr              (ExprAST            *node, Context *ctx);
    virtual void visitBoolLiteral       (BoolLiteralAST     *node, Context *ctx);
    virtual void visitIntLiteral        (IntLiteralAST      *node, Context *ctx);
    virtual void visitFloatLiteral      (FloatLiteralAST    *node, Context *ctx);
    virtual void visitStrLiteral        (StrLiteralAST      *node, Context *ctx);
    virtual void visitCharLiteral       (CharLiteralAST     *node, Context *ctx);
    virtual void visitNilLiteral        (NilLiteralAST      *node, Context *ctx);
    virtual void visitSetLiteral        (SetLiteralAST      *node, Context *ctx);
    virtual void visitIdentDef          (IdentDefAST        *node, Context *ctx);
    virtual void visitDesignator        (DesignatorAST      *node, Context *ctx);
    virtual void visitUnExpr            (UnExprAST          *node, Context *ctx);
    virtual void visitBinExpr           (BinExprAST         *node, Context *ctx);
    virtual void visitIdentifier        (IdentifierAST      *node, Context *ctx);
    virtual void visitQualIdent         (QualIdentAST       *node, Context *ctx);
    virtual void visitIfStatement       (IfStatementAST     *node, Context *ctx);
    virtual void visitCaseStatement     (CaseStatementAST   *node, Context *ctx);
    virtual void visitCaseClause        (CaseClauseAST      *node, Context *ctx);
    virtual void visitWhileStatement    (WhileStatementAST  *node, Context *ctx);
    virtual void visitRepeatStatement   (RepeatStatementAST *node, Context *ctx);
    virtual void visitForStatement      (ForStatementAST    *node, Context *ctx);
    virtual void visitLoopStatement     (LoopStatementAST   *node, Context *ctx);
    virtual void visitWithStatement     (WithStatementAST   *node, Context *ctx);
    virtual void visitWithClause        (WithClauseAST      *node, Context *ctx);
    virtual void visitExitStatement     (ExitStatementAST   *node, Context *ctx);
    virtual void visitReturnStatement   (ReturnStatementAST *node, Context *ctx);
    virtual void visitAssignStatement   (AssignStatementAST *Node, Context *ctx);
    virtual void visitCallStatement     (CallStatementAST   *Node, Context *ctx);
    virtual void visitCallExpr          (CallExprAST        *Node, Context *ctx);
private:
    void loc(BaseAST *node, Context *ctx);
    std::string pre(Context *ctx, bool needPad = true);
    void push(Context *ctx, std::string p);
    void pop(Context *ctx);
private:
    std::ostream &out;
    PrinterContext *ctx;
};
