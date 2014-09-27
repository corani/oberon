#pragma once
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/raw_os_ostream.h"

#include <memory>
#include <string>
#include "visitor.h"
#include "ast.h"

class GeneratorContext : public Context {
public:
    GeneratorContext();
    virtual ~GeneratorContext();
    void newModule(std::string name);
    void toBitFile(std::string name);
private:
    llvm::IRBuilder<> Builder;
    llvm::Module *mod;
    llvm::ExecutionEngine *executionEngine;
    llvm::FunctionPassManager *fpm;
};

class Generator : public Visitor {
public:
    Generator() {};

    shared_ptr<GeneratorContext> generate(shared_ptr<ModuleAST> module);
public:
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
    virtual void visitDesignatorIdentPart(DesignatorIdentPartAST *node, Context *ctx);
    virtual void visitDesignatorArrayPart(DesignatorArrayPartAST *node, Context *ctx);
    virtual void visitDesignatorDerefPart(DesignatorDerefPartAST *node, Context *ctx);
    virtual void visitDesignatorCastPart (DesignatorCastPartAST  *node, Context *ctx);
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
    GeneratorContext *castContext(Context *ctx) { return static_cast<GeneratorContext *>(ctx); }
};
