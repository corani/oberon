#include "ast.h"
#include "visitor.h"

using namespace std;

void ModuleAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitModule(this, ctx);
}

void ProcDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitProcDecl(this, ctx);
}

void ForwardDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitForwardDecl(this, ctx);
}

void ExternDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitExternDecl(this, ctx);
}

void TypeDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitTypeDecl(this, ctx);
}

void BasicTypeAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitBasicType(this, ctx);
}

void ArrayTypeAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitArrayType(this, ctx);
}

void RecordTypeAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitRecordType(this, ctx);
}

void PointerTypeAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitPointerType(this, ctx);
}

void ProcedureTypeAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitProcedureType(this, ctx);
}

void VarDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitVarDecl(this, ctx);
}

void ConstDeclAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitConstDecl(this, ctx);
}

void ReceiverAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitReceiver(this, ctx);
}

void BoolLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitBoolLiteral(this, ctx);
}

void IntLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitIntLiteral(this, ctx);
}

void FloatLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitFloatLiteral(this, ctx);
}

void StrLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitStrLiteral(this, ctx);
}

void CharLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitCharLiteral(this, ctx);
}

void NilLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitNilLiteral(this, ctx);
}

void SetLiteralAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitSetLiteral(this, ctx);
}

void IdentDefAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitIdentDef(this, ctx);
}

void DesignatorAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitDesignator(this, ctx);
}

void DesignatorIdentPartAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitDesignatorIdentPart(this, ctx);
}

void DesignatorArrayPartAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitDesignatorArrayPart(this, ctx);
}

void DesignatorDerefPartAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitDesignatorDerefPart(this, ctx);
}

void DesignatorCastPartAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitDesignatorCastPart(this, ctx);
}

void UnExprAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitUnExpr(this, ctx);
}

void BinExprAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitBinExpr(this, ctx);
}

void IdentifierAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitIdentifier(this, ctx);
}

void QualIdentAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitQualIdent(this, ctx);
}

void IfStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitIfStatement(this, ctx);
}

void CaseClauseAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitCaseClause(this, ctx);
}

void CaseStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitCaseStatement(this, ctx);
}

void WhileStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitWhileStatement(this, ctx);
}

void RepeatStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitRepeatStatement(this, ctx);
}

void ForStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitForStatement(this, ctx);
}

void LoopStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitLoopStatement(this, ctx);
}

void WithClauseAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitWithClause(this, ctx);
}

void WithStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitWithStatement(this, ctx);
}

void ExitStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitExitStatement(this, ctx);
}

void ReturnStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitReturnStatement(this, ctx);
}

void AssignStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitAssignStatement(this, ctx);
}

void CallStatementAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitCallStatement(this, ctx);
}

void CallExprAST::visit(Visitor *visitor, Context *ctx) {
    visitor->visitCallExpr(this, ctx);
}

