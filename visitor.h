#pragma once

#include "ast.h"

class Context {
public:
    virtual ~Context(){}
};

class Visitor {
public:
    virtual ~Visitor() {}

    virtual void visitModule            (ModuleAST          *node, Context *ctx) = 0;
    virtual void visitProcDecl          (ProcDeclAST        *node, Context *ctx) = 0;
    virtual void visitForwardDecl       (ForwardDeclAST     *node, Context *ctx) = 0;
    virtual void visitExternDecl        (ExternDeclAST      *node, Context *ctx) = 0;
    virtual void visitTypeDecl          (TypeDeclAST        *node, Context *ctx) = 0;
    virtual void visitBasicType         (BasicTypeAST       *node, Context *ctx) = 0;
    virtual void visitArrayType         (ArrayTypeAST       *node, Context *ctx) = 0;
    virtual void visitRecordType        (RecordTypeAST      *node, Context *ctx) = 0;
    virtual void visitPointerType       (PointerTypeAST     *node, Context *ctx) = 0;
    virtual void visitProcedureType     (ProcedureTypeAST   *node, Context *ctx) = 0;
    virtual void visitVarDecl           (VarDeclAST         *node, Context *ctx) = 0;
    virtual void visitConstDecl         (ConstDeclAST       *node, Context *ctx) = 0;
    virtual void visitReceiver          (ReceiverAST        *node, Context *ctx) = 0;
    virtual void visitExpr              (ExprAST            *node, Context *ctx) = 0;
    virtual void visitBoolLiteral       (BoolLiteralAST     *node, Context *ctx) = 0;
    virtual void visitIntLiteral        (IntLiteralAST      *node, Context *ctx) = 0;
    virtual void visitFloatLiteral      (FloatLiteralAST    *node, Context *ctx) = 0;
    virtual void visitStrLiteral        (StrLiteralAST      *node, Context *ctx) = 0;
    virtual void visitCharLiteral       (CharLiteralAST     *node, Context *ctx) = 0;
    virtual void visitNilLiteral        (NilLiteralAST      *node, Context *ctx) = 0;
    virtual void visitIdentDef          (IdentDefAST        *node, Context *ctx) = 0;
    virtual void visitDesignator        (DesignatorAST      *node, Context *ctx) = 0;
    virtual void visitUnExpr            (UnExprAST          *node, Context *ctx) = 0;
    virtual void visitBinExpr           (BinExprAST         *node, Context *ctx) = 0;
    virtual void visitIdentifier        (IdentifierAST      *node, Context *ctx) = 0;
    virtual void visitQualIdent         (QualIdentAST       *node, Context *ctx) = 0;
    virtual void visitIfStatement       (IfStatementAST     *node, Context *ctx) = 0;
    virtual void visitCaseStatement     (CaseStatementAST   *node, Context *ctx) = 0;
    virtual void visitCaseClause        (CaseClauseAST      *node, Context *ctx) = 0;
    virtual void visitWhileStatement    (WhileStatementAST  *node, Context *ctx) = 0;
    virtual void visitRepeatStatement   (RepeatStatementAST *node, Context *ctx) = 0;
    virtual void visitForStatement      (ForStatementAST    *node, Context *ctx) = 0;
    virtual void visitLoopStatement     (LoopStatementAST   *node, Context *ctx) = 0;
    virtual void visitWithStatement     (WithStatementAST   *node, Context *ctx) = 0;
    virtual void visitWithClause        (WithClauseAST      *node, Context *ctx) = 0;
    virtual void visitExitStatement     (ExitStatementAST   *node, Context *ctx) = 0;
    virtual void visitReturnStatement   (ReturnStatementAST *node, Context *ctx) = 0;
    virtual void visitAssignStatement   (AssignStatementAST *Node, Context *ctx) = 0;
    virtual void visitCallStatement     (CallStatementAST   *Node, Context *ctx) = 0;
    virtual void visitCallExpr          (CallExprAST        *Node, Context *ctx) = 0;
};
