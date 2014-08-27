#include "printer.h"

using namespace std;

void Printer::visitModule(ModuleAST *node, Context *ctx) {
    out << pre(ctx) << "* Module[" << node->name << "] src: " << node->start->getLocation() << " - " << node->end->getLocation() << endl;
    if (!node->imports.empty()) {
        out << pre(ctx) << "+-Imports: ";
        bool first = true;
        for (auto item : node->imports) {
            if (!first) out << ", ";
            out << item.first;
            if (item.second != "") {
                out << " := " << item.second;
            }
            first = false;
        }
        out << endl;
    }
    if (!node->decls.empty()) {
        out << pre(ctx) << "+-Declarations:" << endl;
        for (auto decl : node->decls) {
            push(ctx, "|  ");
            decl->visit(this, ctx);
            pop(ctx);
        }
    }
    if (!node->stmts.empty()) {
        out << pre(ctx) << "+-Statements:" << endl;
        for (auto stmt : node->stmts) {
            push(ctx, "|  ");
            stmt->visit(this, ctx);
            pop(ctx);
        }
    }
    out << pre(ctx) << "= Module[" << node->name << "]" << endl;
}
void Printer::visitProcDecl(ProcDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* ProcDecl[" << node->ident->name << "] src: " << node->start->getLocation() << " - " << node->end->getLocation() << endl;
    if (node->ret) {
        out << pre(ctx) << "|  +- Return Type:" << endl;
        push(ctx, "|  |  ");
        node->ret->visit(this, ctx);
        pop(ctx);
    }
    if (!node->params.empty()) {
        out << pre(ctx) << "|  +- Params:" << endl;
        push(ctx, "|  |  ");
        for (auto param : node->params) {
            param->visit(this, ctx);
        }
        pop(ctx);
    }
    if (!node->decls.empty()) {
        out << pre(ctx) << "|  +- Declarations:" << endl;
        push(ctx, "|  |  ");
        for (auto decl : node->decls) {
            decl->visit(this, ctx);
        }
        pop(ctx);
    }
    if (!node->stmts.empty()) {
        out << pre(ctx) << "|  +- Statements:" << endl;
        push(ctx, "|  |  ");
        for (auto stmt : node->stmts) {
            stmt->visit(this, ctx);
        }
        pop(ctx);
    }
    out << pre(ctx) << "|  = ProcDecl[" << node->ident->name << "]" << endl;
}
void Printer::visitForwardDecl(ForwardDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* ForwardDecl[" << node->ident->name << "]" << endl;
    if (node->ret) {
        out << pre(ctx) << "|  +- Return Type:" << endl;
        push(ctx, "|  |  ");
        node->ret->visit(this, ctx);
        pop(ctx);
    }
    if (!node->params.empty()) {
        out << pre(ctx) << "|  +- Params:" << endl;
        push(ctx, "|  |  ");
        for (auto param : node->params) {
            param->visit(this, ctx);
        }
        pop(ctx);
    }
    out << pre(ctx) << "|  = ForwardDecl[" << node->ident->name << "]" << endl;
}
void Printer::visitExternDecl(ExternDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* ExternDecl[" << node->ident->name << "]" << endl;
    if (node->ret) {
        out << pre(ctx) << "|  +- Return Type:" << endl;
        push(ctx, "|  |  ");
        node->ret->visit(this, ctx);
        pop(ctx);
    }
    if (!node->params.empty()) {
        out << pre(ctx) << "|  +- Params:" << endl;
        push(ctx, "|  |  ");
        for (auto param : node->params) {
            param->visit(this, ctx);
        }
        pop(ctx);
    }
    out << pre(ctx) << "|  = ExternDecl[" << node->ident->name << "]" << endl;
}
void Printer::visitTypeDecl(TypeDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* TypeDecl[" << node->ident->name << "] src: " << node->start->getLocation() << " - " << node->end->getLocation() << endl;
    push(ctx, "|  |  ");
    node->type->visit(this, ctx);
    pop(ctx);
}
void Printer::visitBasicType(BasicTypeAST *node, Context *ctx) {
    out << pre(ctx) << "* BasicType:" << endl;
    push(ctx, "|  ");
    node->qid->visit(this, ctx);
    pop(ctx);
}
void Printer::visitArrayType(ArrayTypeAST *node, Context *ctx) {
    out << pre(ctx) << "* ArrayType" << endl;
    push(ctx, "|  ");
    node->arrayOf->visit(this, ctx);
    pop(ctx);
}
void Printer::visitRecordType(RecordTypeAST *node, Context *ctx) {
    out << pre(ctx) << "* RecordType" << endl;
    push(ctx, "|  ");
    if (node->base) {
        out << pre(ctx) << "+- Base:" << endl;
        push(ctx, "|  ");
        node->base->visit(this, ctx);
        pop(ctx);
    }
    out << pre(ctx) << "+- Fields:" << endl;
    push(ctx, "|  ");
    for (auto field : node->fields) {
        out << pre(ctx) << field.first->name << " = " << endl;
        push(ctx, "|  ");
        field.second->visit(this, ctx);
        pop(ctx);
    }
    pop(ctx);
    pop(ctx);
}
void Printer::visitPointerType(PointerTypeAST *node, Context *ctx) {
    out << pre(ctx) << "* PointerType to:" << endl;
    push(ctx, "|  ");
    node->pointee->visit(this, ctx);
    pop(ctx);
}
void Printer::visitProcedureType(ProcedureTypeAST *node, Context *ctx) {
    out << pre(ctx) << "* ProcedureType" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Returns: " << node->ret->name << endl;
    out << pre(ctx) << "+- Params:" << endl;
    push(ctx, "|  ");
    for (auto param : node->params) {
        param->visit(this, ctx);
    }
    pop(ctx);
    pop(ctx);
}
void Printer::visitVarDecl(VarDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* VarDecl[" << node->ident->name << "]" << endl;
    push(ctx, "|  ");
    node->type->visit(this, ctx);
    pop(ctx);
}
void Printer::visitConstDecl(ConstDeclAST *node, Context *ctx) {
    out << pre(ctx) << "* ConstDecl[" << node->ident->name << "]" << endl;
    push(ctx, "|  ");
    node->expr->visit(this, ctx);
    pop(ctx);
}
void Printer::visitReceiver(ReceiverAST *node, Context *ctx) {
    out << pre(ctx) << "* Receiver[" << node->name << ": " << node->type << "]" << endl;
}
void Printer::visitExpr(ExprAST *node, Context *ctx) {
    out << pre(ctx) << "Expr" << endl;
}
void Printer::visitBoolLiteral(BoolLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "BoolLiteral[" << (node->value ? "TRUE" : "FALSE") << "]" << endl;
}
void Printer::visitIntLiteral(IntLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "IntLiteral[" << node->value << "]" << endl;
}
void Printer::visitFloatLiteral(FloatLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "FloatLiteral[" << node->value << "]" << endl;
}
void Printer::visitStrLiteral(StrLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "StrLiteral[" << node->value << "]" << endl;
}
void Printer::visitCharLiteral(CharLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "CharLiteral[" << string(1, node->value) << "]" << endl;
}
void Printer::visitNilLiteral(NilLiteralAST *node, Context *ctx) {
    out << pre(ctx) << "NilLiteral" << endl;
}
void Printer::visitIdentDef(IdentDefAST *node, Context *ctx) {
    out << pre(ctx) << "IdentDef[" << node->name;
    switch(node->export_) {
        case Export::YES:
            out << "*";
            break;
        case Export::RO:
            out << "-";
            break;
        default:
            break;
    }
    out << "]" << endl;
}
void Printer::visitDesignator(DesignatorAST *node, Context *ctx) {
    out << pre(ctx) << "Designator" << endl;
    push(ctx, "|  ");
    node->qid->visit(this, ctx);
    pop(ctx);
}
void Printer::visitUnExpr(UnExprAST *node, Context *ctx) {
    out << pre(ctx) << "* UnExpr[" << node->op << "]" << endl;
    push(ctx, "|  ");
    node->operand->visit(this, ctx);
    pop(ctx);
}
void Printer::visitBinExpr(BinExprAST *node, Context *ctx) {
    out << pre(ctx) << "* BinExpr[" << node->op << "]" << endl;
    push(ctx, "L  ");
    node->lhs->visit(this, ctx);
    pop(ctx);
    push(ctx, "R  ");
    node->rhs->visit(this, ctx);
    pop(ctx);
}
void Printer::visitIdentifier(IdentifierAST *node, Context *ctx) {
    out << pre(ctx) << "Identifier:" << endl;
    push(ctx, "|  ");
    node->des->visit(this, ctx);
    pop(ctx);
}
void Printer::visitQualIdent(QualIdentAST *node, Context *ctx) {
    out << pre(ctx) << "* QualIdent[" << node->module << "." << node->name << "]" << endl;
}
void Printer::visitIfStatement(IfStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* IfStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Condition:" << endl;
    push(ctx, "|  ");
    node->cond->visit(this, ctx);
    pop(ctx);
    if (!node->thenStmts.empty()) {
        out << pre(ctx) << "+- Then:" << endl;
        push(ctx, "|  ");
        for (auto stmt : node->thenStmts) {
            stmt->visit(this, ctx);
        }
        pop(ctx);
    }
    if (!node->elseStmts.empty()) {
        out << pre(ctx) << "+- Else:" << endl;
        push(ctx, "|  ");
        for (auto stmt : node->elseStmts) {
            stmt->visit(this, ctx);
        }
        pop(ctx);
    }
    out << pre(ctx) << "= IfStatement" << endl;
    pop(ctx);
}
void Printer::visitCaseStatement(CaseStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* CaseStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Condition:" << endl;
    push(ctx, "|  ");
    node->cond->visit(this, ctx);
    for (auto clause : node->clauses) {
        clause->visit(this, ctx);
    }
    pop(ctx);
    if (!node->elseStmts.empty()) {
        out << pre(ctx) << "+- Else:" << endl;
        push(ctx, "|  ");
        for (auto stmt : node->elseStmts) {
            stmt->visit(this, ctx);
        }
        pop(ctx);
    }
    out << pre(ctx) << "+- Else:" << endl;
    pop(ctx);
}
void Printer::visitCaseClause(CaseClauseAST *node, Context *ctx) {
    out << pre(ctx) << "+- CaseClause: " << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Ranges:" << endl;
    push(ctx, "|  ");
    for (auto range : node->when) {
        out << pre(ctx) << "+- From:" << endl;
        push(ctx, "|  ");
        range.first->visit(this, ctx);
        pop(ctx);
        out << pre(ctx) << "+- To:" << endl;
        push(ctx, "|  ");
        range.second->visit(this, ctx);
        pop(ctx);
    }
    pop(ctx);
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt: node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    pop(ctx);
}
void Printer::visitWhileStatement(WhileStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* WhileStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Condition:" << endl;
    push(ctx, "|  ");
    node->cond->visit(this, ctx);
    pop(ctx);
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt : node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    out << pre(ctx) << "= WhileStatement" << endl;
    pop(ctx);
}
void Printer::visitRepeatStatement(RepeatStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* RepeatStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt : node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    out << pre(ctx) << "+- Condition:" << endl;
    push(ctx, "|  ");
    node->cond->visit(this, ctx);
    pop(ctx);
    out << pre(ctx) << "= RepeatStatement" << endl;
    pop(ctx);
}
void Printer::visitForStatement(ForStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* ForStatement[" << node->iden << "]" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- From:" << endl;
    push(ctx, "|  ");
    node->from->visit(this, ctx);
    pop(ctx);
    out << pre(ctx) << "+- To:" << endl;
    push(ctx, "|  ");
    node->to->visit(this, ctx);
    pop(ctx);
    if (node->by) {
        out << pre(ctx) << "+- By:" << endl;
        push(ctx, "|  ");
        node->by->visit(this, ctx);
        pop(ctx);
    }
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt : node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    out << pre(ctx) << "= ForStatement" << endl;
    pop(ctx);
}
void Printer::visitLoopStatement(LoopStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* LoopStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt : node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    out << pre(ctx) << "= LoopStatement" << endl;
    pop(ctx);
}
void Printer::visitWithStatement(WithStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* WithStatement" << endl;
    push(ctx, "|  ");
    if (!node->elseStmts.empty()) {
        out << pre(ctx) << "+- Else:" << endl;
        push(ctx, "|  ");
        for (auto stmt : node->elseStmts) {
            stmt->visit(this, ctx);
        }
        pop(ctx);
    }
    pop(ctx);
}
void Printer::visitWithClause(WithClauseAST *node, Context *ctx) {
    out << pre(ctx) << "+- WithClause" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Name:" << endl;
    push(ctx, "|  ");
    node->name->visit(this, ctx);
    pop(ctx);
    out << pre(ctx) << "+- Type:" << endl;
    push(ctx, "|  ");
    node->type->visit(this, ctx);
    pop(ctx);
    out << pre(ctx) << "+- Statements:" << endl;
    push(ctx, "|  ");
    for (auto stmt: node->stmts) {
        stmt->visit(this, ctx);
    }
    pop(ctx);
    pop(ctx);
}
void Printer::visitExitStatement(ExitStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* ExitStatement" << endl;
}
void Printer::visitReturnStatement(ReturnStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* ReturnStatement" << endl;
    if (node->expr) {
        push(ctx, "|  ");
        node->expr->visit(this, ctx);
        pop(ctx);
    }
}
void Printer::visitAssignStatement(AssignStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* AssignStatement:" << endl;
    push(ctx, "|  ");
    push(ctx, "L  ");
    node->des->visit(this, ctx);
    pop(ctx);
    push(ctx, "R  ");
    node->expr->visit(this, ctx);
    pop(ctx);
    pop(ctx);
}
void Printer::visitCallStatement(CallStatementAST *node, Context *ctx) {
    out << pre(ctx) << "* CallStatement" << endl;
    push(ctx, "|  ");
    out << pre(ctx) << "+- Method:" << endl;
    push(ctx, "|  ");
    node->des->visit(this, ctx);
    pop(ctx);
    if (!node->args.empty()) {
        out << pre(ctx) << "+- Arguments:" << endl;
        int i = 0;
        for (auto arg : node->args) {
            push(ctx, to_string(i++) + "  ");
            arg->visit(this, ctx);
            pop(ctx);
        }
    }
    pop(ctx);
}
void Printer::visitCallExpr(CallExprAST *node, Context *ctx) {
    node->call->visit(this, ctx);
}

string Printer::pre(Context *ctx) {
    PrinterContext *pc = dynamic_cast<PrinterContext*>(ctx);
    if (pc) {
        return pc->pre;
    } else {
        return "";
    }
}

void Printer::push(Context *ctx, string p) {
    PrinterContext *pc = dynamic_cast<PrinterContext*>(ctx);
    if (pc) {
        pc->push(p);
    }
}

void Printer::pop(Context *ctx) {
    PrinterContext *pc = dynamic_cast<PrinterContext*>(ctx);
    if (pc) {
        pc->pop();
    }
}
