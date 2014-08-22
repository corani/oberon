#include "ast.h"

using namespace std;

void ModuleAST::print(ostream &out, string pre) const {
    out << pre << "* Module[" << name << "] src: " << start->getLocation() << " - " << end->getLocation() << endl;
    if (!imports.empty()) {
        out << pre << "+-Imports: ";
        bool first = true;
        for (auto item : imports) {
            if (!first) out << ", ";
            out << item.first;
            if (item.second != "") {
                out << " := " << item.second;
            }
            first = false;
        }
        out << endl;
    }
    if (!decls.empty()) {
        out << pre << "+-Declarations:" << endl;
        for (auto decl : decls) {
            decl->print(out, pre + "|  ");
        }
    }
    if (!stmts.empty()) {
        out << pre << "+-Statements:" << endl;

        for (auto stmt : stmts) {
            stmt->print(out, pre + "|  ");
        }
    }
    out << pre << "= Module[" << name << "]" << endl;
}

void ProcDeclAST::print(ostream &out, string pre) const {
    out << pre << "* ProcDecl[" << ident->name << "] src: " << start->getLocation() << " - " << end->getLocation() << endl;
    if (!decls.empty()) {
        out << pre << "+- Declarations:" << endl;
        for (auto decl : decls) {
            decl->print(out, pre + "|  ");
        }
    }
    if (!stmts.empty()) {
        out << pre << "+- Statements:" << endl;
        for (auto stmt : stmts) {
            stmt->print(out, pre + "|  ");
        }
    }
    out << pre << "= ProcDecl[" << ident->name << "]" << endl;
}

void ForwardDeclAST::print(ostream &out, string pre) const {
    out << pre << "* ForwardDecl[" << ident->name << "]" << endl;
}

void ExternDeclAST::print(ostream &out, string pre) const {
    out << pre << "* ExternDecl[" << ident->name << "]" << endl;
}

void TypeDeclAST::print(ostream &out, string pre) const {
    out << pre << "* TypeDecl[" << ident->name << "] src: " << start->getLocation() << " - " << end->getLocation() << endl;
}

void TypeAST::print(ostream &out, string pre) const {
    out << pre << "* Type" << endl;
}

void ConstDeclAST::print(ostream &out, string pre) const {
    out << pre << "* ConstDecl" << endl;
}

void VarDeclAST::print(ostream &out, string pre) const {
    out << pre << "* VarDecl" << endl;
}

void ReceiverAST::print(ostream &out, string pre) const {
    out << pre << "* Receiver[" << name << ": " << type << "]" << end;
}

void IfStatementAST::print(ostream &out, string pre) const {
    out << pre << "* IfStatement" << endl;
    if (!thenStmts.empty()) {
        out << pre << "+- Then:" << endl;
        for (auto stmt : thenStmts) {
            stmt->print(out, pre + "|  ");
        }
    }
    if (!elseStmts.empty()) {
        out << pre << "+- Else:" << endl;
        for (auto stmt : elseStmts) {
            stmt->print(out, pre + "|  ");
        }
    }
    out << pre << "= IfStatement" << endl;
}

void CaseClauseAST::print(ostream &out, string pre) const {
    out << pre << "* CaseClause" << endl;
}

void CaseStatementAST::print(ostream &out, string pre) const {
    out << pre << "* CaseStatement" << endl;
}

void WhileStatementAST::print(ostream &out, string pre) const {
    out << pre << "* WhileStatement" << endl;
    for (auto stmt : stmts) {
        stmt->print(out, pre + "|  ");
    }
    out << pre << "= WhileStatement" << endl;
}

void RepeatStatementAST::print(ostream &out, string pre) const {
    out << pre << "* RepeatStatement" << endl;
    for (auto stmt : stmts) {
        stmt->print(out, pre + "|  ");
    }
    out << pre << "= RepeatStatement" << endl;
}

void ForStatementAST::print(ostream &out, string pre) const {
    out << pre << "* ForStatement" << endl;
    for (auto stmt : stmts) {
        stmt->print(out, pre + "|  ");
    }
    out << pre << "= ForStatement" << endl;
}

void LoopStatementAST::print(ostream &out, string pre) const {
    out << pre << "* LoopStatement" << endl;
    for (auto stmt : stmts) {
        stmt->print(out, pre + "|  ");
    }
    out << pre << "= LoopStatement" << endl;
}

void WithClauseAST::print(ostream &out, string pre) const {
    out << pre << "* WithClause" << endl;
}

void WithStatementAST::print(ostream &out, string pre) const {
    out << pre << "* WithStatement" << endl;
}

void ExitStatementAST::print(ostream &out, string pre) const {
    out << pre << "* ExitStatement" << endl;
}

void ReturnStatementAST::print(ostream &out, string pre) const {
    out << pre << "* ReturnStatement" << endl;
}

void AssignStatementAST::print(ostream &out, string pre) const {
    out << pre << "* AssignStatement[" << des->name << "] = " << endl;
    expr->print(out, pre + "|  ");
}

void CallStatementAST::print(ostream &out, string pre) const {
    out << pre << "* CallStatement[" << des->name << "]" << endl;
    for (auto arg : args) {
        arg->print(out, pre + "|  ");
    }
}

void UnExprAST::print(ostream &out, string pre) const {
    out << pre << "* UnExpr[" << op << "]" << endl;
    operand->print(out, pre + "|  ");
}

void BinExprAST::print(ostream &out, string pre) const {
    out << pre << "* BinExpr[" << op << "]" << endl;
    lhs->print(out, pre + "L  ");
    rhs->print(out, pre + "R  ");
}

void IdentDefAST::print(ostream &out, string pre) const {
    out << pre << "IdentDef[" << name << "]" << endl;
}

void DesignatorAST::print(ostream &out, string pre) const {
    out << pre << "Designator" << endl;
}

void NilLiteralAST::print(ostream &out, string pre) const {
    out << pre << "NilLiteral" << endl;
}

void CharLiteralAST::print(ostream &out, string pre) const {
    out << pre << "CharLiteral[" << string(1, value) << "]" << endl;
}

void StrLiteralAST::print(ostream &out, string pre) const {
    out << pre << "StrLiteral[" << value << "]" << endl;
}

void FloatLiteralAST::print(ostream &out, string pre) const {
    out << pre << "FloatLiteral[" << value << "]" << endl;
}

void IntLiteralAST::print(ostream &out, string pre) const {
    out << pre << "IntLiteral[" << value << "]" << endl;
}

void BoolLiteralAST::print(ostream &out, string pre) const {
    out << pre << "BoolLiteral[" << (value ? "TRUE" : "FALSE") << "]" << endl;
}

void CallExprAST::print(ostream &out, string pre) const {
    call->print(out, pre);
}

void IdentifierAST::print(ostream &out, string pre) const {
    out << pre << "Identifier[" << des->name << "]" << endl;
}
