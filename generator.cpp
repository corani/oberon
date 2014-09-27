#include "llvm/Analysis/Passes.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "generator.h"
#include <fstream>

using namespace std;
using namespace llvm;

GeneratorContext::GeneratorContext()
        : Builder(getGlobalContext()),
          executionEngine(nullptr),
          mod(nullptr),
          fpm(nullptr) {
    InitializeNativeTarget();
}

GeneratorContext::~GeneratorContext() {
    delete executionEngine; executionEngine = nullptr;
    delete mod; mod = nullptr;
    delete fpm; fpm = nullptr;
}

void GeneratorContext::newModule(string name) {
    mod = new Module(name, getGlobalContext());

    string ErrStr;
    executionEngine = EngineBuilder(mod).setErrorStr(&ErrStr).create();
    if (!executionEngine) {
        cerr << "Could not create ExecutionEngine: " << ErrStr << endl;
        exit(1);
    }

    fpm = new FunctionPassManager(mod);

    fpm->add(new DataLayoutPass(*executionEngine->getDataLayout()));
    fpm->add(createPromoteMemoryToRegisterPass());
    fpm->add(createBasicAliasAnalysisPass());
    fpm->add(createInstructionCombiningPass());
    fpm->add(createReassociatePass());
    fpm->add(createGVNPass());
    fpm->add(createCFGSimplificationPass());
    fpm->doInitialization();
}

void GeneratorContext::toBitFile(const string name) {
    ofstream fout(name, ofstream::out | ofstream::binary);
    if (fout.is_open()) {
        llvm::raw_os_ostream out(fout);
        WriteBitcodeToFile(mod, out);
        out.flush();
        fout.close();
    }
}

shared_ptr<GeneratorContext> Generator::generate(shared_ptr<ModuleAST> module) {
    auto ctx = make_shared<GeneratorContext>();
    visitModule(module.get(), ctx.get());
    return ctx;
}

void Generator::visitModule(ModuleAST *node, Context *ctx) {
    castContext(ctx)->newModule(node->name);
    for (auto stmt : node->stmts) {
        stmt->visit(this, ctx);
    }
}

void Generator::visitProcDecl(ProcDeclAST *node, Context *ctx) {
}

void Generator::visitForwardDecl(ForwardDeclAST *node, Context *ctx) {
}

void Generator::visitExternDecl(ExternDeclAST *node, Context *ctx) {
}

void Generator::visitTypeDecl(TypeDeclAST *node, Context *ctx) {
}

void Generator::visitBasicType(BasicTypeAST *node, Context *ctx) {
}

void Generator::visitArrayType(ArrayTypeAST *node, Context *ctx) {
}

void Generator::visitRecordType(RecordTypeAST *node, Context *ctx) {
}

void Generator::visitPointerType(PointerTypeAST *node, Context *ctx) {
}

void Generator::visitProcedureType(ProcedureTypeAST *node, Context *ctx) {
}

void Generator::visitVarDecl(VarDeclAST *node, Context *ctx) {
}

void Generator::visitConstDecl(ConstDeclAST *node, Context *ctx) {
}

void Generator::visitReceiver(ReceiverAST *node, Context *ctx) {
}

void Generator::visitExpr(ExprAST *node, Context *ctx) {
}

void Generator::visitBoolLiteral(BoolLiteralAST *node, Context *ctx) {
}

void Generator::visitIntLiteral(IntLiteralAST *node, Context *ctx) {
}

void Generator::visitFloatLiteral(FloatLiteralAST *node, Context *ctx) {
}

void Generator::visitStrLiteral(StrLiteralAST *node, Context *ctx) {
}

void Generator::visitCharLiteral(CharLiteralAST *node, Context *ctx) {
}

void Generator::visitNilLiteral(NilLiteralAST *node, Context *ctx) {
}

void Generator::visitSetLiteral(SetLiteralAST *node, Context *ctx) {
}

void Generator::visitIdentDef(IdentDefAST *node, Context *ctx) {
}

void Generator::visitDesignator(DesignatorAST *node, Context *ctx) {
}

void Generator::visitDesignatorIdentPart(DesignatorIdentPartAST *node, Context *ctx) {
}

void Generator::visitDesignatorArrayPart(DesignatorArrayPartAST *node, Context *ctx) {
}

void Generator::visitDesignatorDerefPart(DesignatorDerefPartAST *node, Context *ctx) {
}

void Generator::visitDesignatorCastPart(DesignatorCastPartAST *node, Context *ctx) {
}

void Generator::visitUnExpr(UnExprAST *node, Context *ctx) {
}

void Generator::visitBinExpr(BinExprAST *node, Context *ctx) {
}

void Generator::visitIdentifier(IdentifierAST *node, Context *ctx) {
}

void Generator::visitQualIdent(QualIdentAST *node, Context *ctx) {
}

void Generator::visitIfStatement(IfStatementAST *node, Context *ctx) {
}

void Generator::visitCaseStatement(CaseStatementAST *node, Context *ctx) {
}

void Generator::visitCaseClause(CaseClauseAST *node, Context *ctx) {
}

void Generator::visitWhileStatement(WhileStatementAST *node, Context *ctx) {
}

void Generator::visitRepeatStatement(RepeatStatementAST *node, Context *ctx) {
}

void Generator::visitForStatement(ForStatementAST *node, Context *ctx) {
}

void Generator::visitLoopStatement(LoopStatementAST *node, Context *ctx) {
}

void Generator::visitWithStatement(WithStatementAST *node, Context *ctx) {
}

void Generator::visitWithClause(WithClauseAST *node, Context *ctx) {
}

void Generator::visitExitStatement(ExitStatementAST *node, Context *ctx) {
}

void Generator::visitReturnStatement(ReturnStatementAST *node, Context *ctx) {
}

void Generator::visitAssignStatement(AssignStatementAST *node, Context *ctx) {
}

void Generator::visitCallStatement(CallStatementAST *node, Context *ctx) {
}

void Generator::visitCallExpr(CallExprAST *node, Context *ctx) {
}
