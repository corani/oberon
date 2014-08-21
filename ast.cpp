#include "ast.h"

using namespace std;

void ModuleAST::print(ostream &out, string pre) const {
    out << pre << "* Module[" << name << "]" << endl;
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
    out << pre << "* ProcDecl[" << ident->name << "]" << endl;
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
    out << pre << "* TypeDecl" << endl;
}

void ConstDeclAST::print(ostream &out, string pre) const {
    out << pre << "* ConstDecl" << endl;
}

void VarDeclAST::print(ostream &out, string pre) const {
    out << pre << "* VarDecl" << endl;
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
    out << pre << "* AssignStatement[" << des->name << "]" << endl;
}

void CallStatementAST::print(ostream &out, string pre) const {
    out << pre << "* CallStatement[" << des->name << "]" << endl;
}

