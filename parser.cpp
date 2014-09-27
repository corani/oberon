#include <vector>
#include <memory>
#include <algorithm>
#include "parser.h"

using namespace std;

Parser::Parser() {
    precedence = {
            {"*",   40},
            {"/",   40},
            {"DIV", 40},
            {"MOD", 40},
            {"&",   40},

            {"+",   20},
            {"-",   20},
            {"OR",  20}
    };
}

shared_ptr<ModuleAST> Parser::parseModule(shared_ptr<Lexer> lexer) {
    tok = make_shared<Tokenizer>(lexer);

    auto start = tok->pop(Token::MODULE);
    auto name = tok->pop(Token::IDENTIFIER);
    tok->pop(Token::SEMICOLON);

    auto module = make_shared<ModuleAST>(name->getText());
    symbolTable.enterScope();
    if (module->name != "STD") symbolTable.enterScope();
    symbolTable.newSymbol(module->name, module);

    if (tok->peek(Token::IMPORT)) {
        parseImport(module->imports);
    }

    shared_ptr<Token> keyword;
    while ((keyword = tok->peekAny({Token::TYPE, Token::CONST, Token::VAR}))) {
        switch (keyword->getKind()) {
        case Token::TYPE:
            parseTypeDecl(module->decls);
            break;
        case Token::CONST:
            parseConstDecl(module->decls);
            break;
        case Token::VAR:
            parseVarDecl(module->decls);
            break;
        default:
            break;
        }
    }

    while ((keyword = tok->peekAny({Token::PROCEDURE, Token::EXTERN, Token::SEMICOLON}))) {
        switch(keyword->getKind()) {
        case Token::PROCEDURE:
            parseProcDecl(module->decls);
            break;
        case Token::EXTERN:
            parseExternDecl(module->decls);
            break;
        default:
            tok->pop();
            break;
        }
    }

    if (tok->peek(Token::BEGIN)) {
        tok->pop(Token::BEGIN);
        parseStatementSeq(module->stmts);
    }

    tok->pop(Token::END);
    // TODO Warning if names don't match
    tok->pop(Token::IDENTIFIER);
    auto end = tok->pop(Token::DOT);

    module->start = start;
    module->end = end;

    symbolTable.leaveScope();

    return module;
}

void Parser::parseImport(vector<pair<string, string>> &imports) {
    tok->pop(Token::IMPORT);
    do {
        pair<string, string> import;
        auto keyword = tok->pop(Token::IDENTIFIER);
        import.first = keyword->getText();

        if (tok->peek(Token::ASSIGNMENT)) {
            tok->pop(Token::ASSIGNMENT);
            keyword = tok->pop(Token::IDENTIFIER);
            import.second = keyword->getText();
        } else {
            import.second = "";
        }
        if (tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
        }
        imports.push_back(import);
        symbolTable.newSymbol(import.first, make_shared<ModuleAST>(import.first));
    } while (!tok->peek(Token::SEMICOLON));
    tok->pop(Token::SEMICOLON);
}

void Parser::parseTypeDecl(vector<shared_ptr<DeclAST>> &decls) {
    tok->pop(Token::TYPE);
    do {
        auto decl = make_shared<TypeDeclAST>();
        decl->start = tok->peek();
        decl->ident = parseIdentDef();
        shared_ptr<Token> keyword = tok->pop(Token::RELATION);
        if (keyword->getText() != "=") {
            throw ParserException("Expected '=', got " + keyword->getText(), keyword->getLocation());
        }
        decl->type = parseType();
        decl->end = tok->pop(Token::SEMICOLON);
        symbolTable.newSymbol(decl->ident->name, decl);
        decls.push_back(decl);
    } while (tok->peek(Token::IDENTIFIER));
}

void Parser::parseConstDecl(vector<shared_ptr<DeclAST>> &decls) {
    tok->pop(Token::CONST);
    do {
        auto decl = make_shared<ConstDeclAST>();
        decl->start = tok->peek();
        decl->ident = parseIdentDef();
        shared_ptr<Token> keyword = tok->pop(Token::RELATION);
        if (keyword->getText() != "=") {
            throw ParserException("Expected '='. got " + keyword->getText(), keyword->getLocation());
        }
        decl->expr = parseConstExpr();
        decl->end = tok->pop(Token::SEMICOLON);
        symbolTable.newSymbol(decl->ident->name, decl);
        decls.push_back(decl);
    } while (tok->peek(Token::IDENTIFIER));
}

void Parser::parseVarDecl(vector<shared_ptr<DeclAST>> &decls) {
    tok->pop(Token::VAR);
    do {
        vector<shared_ptr<VarDeclAST>> vars;
        auto start = tok->peek();
        auto decl = make_shared<VarDeclAST>();
        decl->ident = parseIdentDef();
        vars.push_back(decl);
        while (tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
            decl = make_shared<VarDeclAST>();
            decl->ident = parseIdentDef();
            vars.push_back(decl);
        }
        tok->pop(Token::COLON);
        auto type = parseType();
        auto end = tok->pop(Token::SEMICOLON);

        for (auto var : vars) {
            var->start = start;
            var->end = end;
            var->type = type;
            var->byRef = true;
            symbolTable.newSymbol(var->ident->name, var);
            decls.push_back(var);
        }
    } while (tok->peek(Token::IDENTIFIER));
}

void Parser::parseExternDecl(vector<shared_ptr<DeclAST>> &decls) {
    auto start = tok->pop(Token::EXTERN);
    auto ident = parseIdentDef();
    auto _extern = make_shared<ExternDeclAST>(ident);
    _extern->ret = parseFormalParams(_extern->params);
    _extern->start = start;
    _extern->end = tok->peek();
    symbolTable.newSymbol(ident->name, _extern);
    decls.push_back(_extern);
}

void Parser::parseForwardDecl(shared_ptr<Token> start, vector<shared_ptr<DeclAST>> &decls) {
    tok->pop(Token::CARET);
    if (tok->peek(Token::LPAREN)) {
        parseReceiver();
    }
    auto ident = parseIdentDef();
    auto forward = make_shared<ForwardDeclAST>(ident);
    forward->ret = parseFormalParams(forward->params);
    forward->start = start;
    forward->end = tok->peek();
    symbolTable.newSymbol(ident->name, forward);
    decls.push_back(forward);
}

void Parser::parseProcDecl(vector<shared_ptr<DeclAST>> &decls) {
    auto start = tok->pop(Token::PROCEDURE);
    if (tok->peek(Token::CARET)) {
        parseForwardDecl(start, decls);
    } else {
        if (tok->peek(Token::LPAREN)) {
            parseReceiver();
        }
        auto ident = parseIdentDef();
        auto proc = make_shared<ProcDeclAST>(ident);
        symbolTable.newSymbol(ident->name, proc);

        symbolTable.enterScope();
        proc->ret = parseFormalParams(proc->params);
        tok->pop(Token::SEMICOLON);

        shared_ptr<Token> keyword;
        while ((keyword = tok->peekAny({Token::TYPE, Token::CONST, Token::VAR}))) {
            switch (keyword->getKind()) {
                case Token::TYPE:
                    parseTypeDecl(proc->decls);
                    break;
                case Token::CONST:
                    parseConstDecl(proc->decls);
                    break;
                case Token::VAR:
                    parseVarDecl(proc->decls);
                    break;
                default:
                    break;
            }
        }

        while (tok->peek(Token::PROCEDURE)) {
            parseProcDecl(proc->decls);
        }

        if (tok->peek(Token::BEGIN)) {
            tok->pop(Token::BEGIN);
            parseStatementSeq(proc->stmts);
        }

        symbolTable.leaveScope();

        tok->pop(Token::END);
        auto end = tok->pop(Token::IDENTIFIER);

        proc->start = start;
        proc->end = end;

        decls.push_back(proc);
    }
}

void Parser::parseStatementSeq(vector<shared_ptr<StatementAST>> &stmts) {
    while (1) {
        // To allow semicolon after the last statement
        if (tok->peekAny({Token::END, Token::UNTIL, Token::ELSIF, Token::ELSE, Token::PIPE})) {
            break;
        }
        shared_ptr<Token> keyword = tok->peekAny({
                Token::IDENTIFIER, Token::IF, Token::CASE, Token::WHILE,
                Token::REPEAT, Token::FOR, Token::LOOP, Token::WITH, Token::EXIT,
                Token::RETURN
        });
        if (!keyword) {
            keyword = tok->pop();
            throw ParserException("Expected statement, got " + kind_to_string(keyword->getKind()), keyword->getLocation());
        }
        switch(keyword->getKind()) {
        case Token::IDENTIFIER: {
            auto des = parseDesignator();
            if (tok->peek(Token::ASSIGNMENT)) {
                stmts.push_back(parseAssignmentStatement(des));
            } else if (tok->peek(Token::LPAREN)) {
                stmts.push_back(parseCallStatement(des));
            } else {
                stmts.push_back(parseNakedCallStatement(des));
            }
            break;
        }
        case Token::IF:
            stmts.push_back(parseIfStatement());
            break;
        case Token::CASE:
            stmts.push_back(parseCaseStatement());
            break;
        case Token::WHILE:
            stmts.push_back(parseWhileStatement());
            break;
        case Token::REPEAT:
            stmts.push_back(parseRepeatStatement());
            break;
        case Token::FOR:
            stmts.push_back(parseForStatement());
            break;
        case Token::LOOP:
            stmts.push_back(parseLoopStatement());
            break;
        case Token::WITH:
            stmts.push_back(parseWithStatement());
            break;
        case Token::EXIT:
            stmts.push_back(parseExitStatement());
            break;
        case Token::RETURN:
            stmts.push_back(parseReturnStatement());
            break;
        default:
            break;
        }
        if (!tok->peek(Token::SEMICOLON)) {
            break;
        }
        while (tok->peek(Token::SEMICOLON)) {
            tok->pop(Token::SEMICOLON);
        }
    }
}

shared_ptr<AssignStatementAST> Parser::parseAssignmentStatement(shared_ptr<DesignatorAST> des) {
    auto assign = make_shared<AssignStatementAST>(des);
    auto sym = symbolTable.findSymbol(des->qid->name);
    auto var = dynamic_cast<VarDeclAST *>(sym.get());
    if (!var) {
        throw ParserException("Modifiable variable expected", des->start->getLocation());
    }
    tok->pop(Token::ASSIGNMENT);
    assign->expr = parseExpr();
    assign->start = des->start;
    assign->end = tok->peek();
    return assign;
}

shared_ptr<CallStatementAST> Parser::parseCallStatement(shared_ptr<DesignatorAST> des) {
    auto call = make_shared<CallStatementAST>(des);
    call->start = des->start;
    tok->pop(Token::LPAREN);
    if (!tok->peek(Token::RPAREN)) {
        call->args.push_back(parseExpr());
        while (tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
            call->args.push_back(parseExpr());
        }
    }
    call->end = tok->pop(Token::RPAREN);
    return call;
}

shared_ptr<CallStatementAST> Parser::parseNakedCallStatement(shared_ptr<DesignatorAST> des) {
    auto call = make_shared<CallStatementAST>(des);
    call->start = des->start;
    call->end = tok->peek();
    return call;
}

shared_ptr<IfStatementAST> Parser::parseIfStatement() {
    auto start = tok->pop(Token::IF);
    auto cond = parseExpr();
    auto _if = make_shared<IfStatementAST>(cond);
    _if->start = start;
    auto root = _if;
    tok->pop(Token::THEN);
    parseStatementSeq(_if->thenStmts);
    while (tok->peek(Token::ELSIF)) {
        start = tok->pop(Token::ELSIF);
        cond = parseExpr();
        auto _newif = make_shared<IfStatementAST>(cond);
        _newif->start = start;
        _if->end = start;
        tok->pop(Token::THEN);
        parseStatementSeq(_newif->thenStmts);
        _if->elseStmts.push_back(_newif);
        _if = _newif;
    }
    if (tok->peek(Token::ELSE)) {
        tok->pop(Token::ELSE);
        parseStatementSeq(_if->elseStmts);
    }
    _if->end = tok->pop(Token::END);
    return root;
}

shared_ptr<CaseStatementAST> Parser::parseCaseStatement() {
    auto start = tok->pop(Token::CASE);
    auto cond = parseExpr();
    auto _case = make_shared<CaseStatementAST>(cond);
    tok->pop(Token::OF);
    parseCase(_case->clauses);
    while (tok->peek(Token::PIPE)) {
        tok->pop(Token::PIPE);
        parseCase(_case->clauses);
    }
    if (tok->peek(Token::ELSE)) {
        tok->pop(Token::ELSE);
        parseStatementSeq(_case->elseStmts);
    }
    _case->start = start;
    _case->end = tok->pop(Token::END);
    return _case;
}

shared_ptr<WhileStatementAST> Parser::parseWhileStatement() {
    auto start = tok->pop(Token::WHILE);
    auto cond = parseExpr();
    auto _while = make_shared<WhileStatementAST>(cond);
    tok->pop(Token::DO);
    parseStatementSeq(_while->stmts);
    _while->start = start;
    _while->end = tok->pop(Token::END);
    return _while;
}

shared_ptr<RepeatStatementAST> Parser::parseRepeatStatement() {
    auto repeat = make_shared<RepeatStatementAST>();
    repeat->start = tok->pop(Token::REPEAT);
    parseStatementSeq(repeat->stmts);
    tok->pop(Token::UNTIL);
    repeat->cond = parseExpr();
    repeat->end = tok->peek();
    return repeat;
}

shared_ptr<ForStatementAST> Parser::parseForStatement() {
    auto _for = make_shared<ForStatementAST>();
    _for->start = tok->pop(Token::FOR);
    auto iden = tok->pop(Token::IDENTIFIER);
    _for->iden = iden->getText();
    tok->pop(Token::ASSIGNMENT);
    _for->from = parseExpr();
    tok->pop(Token::TO);
    _for->to = parseExpr();
    if (tok->peek(Token::BY)) {
        tok->pop(Token::BY);
        _for->by = parseConstExpr();
    } else {
        _for->by = nullptr;
    }
    tok->pop(Token::DO);
    parseStatementSeq(_for->stmts);
    _for->end = tok->pop(Token::END);
    return _for;
}

shared_ptr<LoopStatementAST> Parser::parseLoopStatement() {
    auto loop = make_shared<LoopStatementAST>();
    loop->start = tok->pop(Token::LOOP);
    parseStatementSeq(loop->stmts);
    loop->end = tok->pop(Token::END);
    return loop;
}

shared_ptr<WithStatementAST> Parser::parseWithStatement() {
    auto with = make_shared<WithStatementAST>();
    with->start = tok->pop(Token::WITH);
    parseWithGuard(with->clauses);
    while (tok->peek(Token::PIPE)) {
        tok->pop(Token::PIPE);
        parseWithGuard(with->clauses);
    }
    if (tok->peek(Token::ELSE)) {
        tok->pop(Token::ELSE);
        parseStatementSeq(with->elseStmts);
    }
    with->end = tok->pop(Token::END);
    return with;
}

shared_ptr<ExitStatementAST> Parser::parseExitStatement() {
    auto exit = make_shared<ExitStatementAST>();
    exit->start = tok->pop(Token::EXIT);
    exit->end = exit->start;
    return exit;
}

shared_ptr<ReturnStatementAST> Parser::parseReturnStatement() {
    auto _return = make_shared<ReturnStatementAST>();
    _return->start = tok->pop(Token::RETURN);
    if (!tok->peekAny({Token::SEMICOLON, Token::END})) {
        _return->expr = parseExpr();
    }
    _return->end = tok->peek();
    return _return;
}

void Parser::parseWithGuard(vector<shared_ptr<WithClauseAST>> &clauses) {
    auto clause = make_shared<WithClauseAST>();
    clause->name = parseQualIdent();
    tok->pop(Token::COLON);
    clause->type = parseQualIdent();
    tok->pop(Token::DO);
    parseStatementSeq(clause->stmts);
    clauses.push_back(clause);
}

void Parser::parseCase(vector<shared_ptr<CaseClauseAST>> &clauses) {
    shared_ptr<CaseClauseAST> clause = make_shared<CaseClauseAST>();
    clause->start = tok->peek();
    pair<shared_ptr<ExprAST>, shared_ptr<ExprAST>> item;
    item.first = parseConstExpr();
    if (tok->peek(Token::RANGE)) {
        tok->pop(Token::RANGE);
        item.second = parseConstExpr();
    } else {
        item.second = nullptr;
    }
    clause->when.push_back(item);

    while (tok->peek(Token::COMMA)) {
        tok->pop(Token::COMMA);
        item.first = parseConstExpr();
        if (tok->peek(Token::RANGE)) {
            tok->pop(Token::RANGE);
            item.second = parseConstExpr();
        } else {
            item.second = nullptr;
        }
        clause->when.push_back(item);
    }
    tok->pop(Token::COLON);
    parseStatementSeq(clause->stmts);

    clause->end = tok->peek();

    clauses.push_back(clause);
}

shared_ptr<ReceiverAST> Parser::parseReceiver() {
    auto receiver = make_shared<ReceiverAST>();
    receiver->start = tok->pop(Token::LPAREN);
    if (tok->peek(Token::VAR)) {
        tok->pop(Token::VAR);
        receiver->byRef = true;
    }
    auto name = tok->pop(Token::IDENTIFIER);
    tok->pop(Token::COLON);
    auto type = tok->pop(Token::IDENTIFIER);
    receiver->name = name->getText();
    receiver->type = type->getText();
    receiver->end = tok->pop(Token::RPAREN);

    auto var = make_shared<VarDeclAST>();
    var->ident = make_shared<IdentDefAST>(name->getText());

    auto sym = symbolTable.findSymbol(type->getText());
    if (sym) {
        auto typeDecl = dynamic_cast<TypeDeclAST *>(sym.get());
        if (typeDecl) {
            var->type = typeDecl->type;
        }
    }
    symbolTable.newSymbol(name->getText(), var);

    return receiver;
}

shared_ptr<QualIdentAST> Parser::parseFormalParams(vector<shared_ptr<VarDeclAST>> &params) {
    tok->pop(Token::LPAREN);
    while (!tok->peek(Token::RPAREN)) {
        bool byRef = false;
        vector<string> names;
        if (tok->peek(Token::VAR)) {
            tok->pop(Token::VAR);
            byRef = true;
        }
        auto iden = tok->pop(Token::IDENTIFIER);
        names.push_back(iden->getText());
        while(tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
            iden = tok->pop(Token::IDENTIFIER);
            names.push_back(iden->getText());
        }
        tok->pop(Token::COLON);
        auto type = parseType();
        for (string name : names) {
            auto param = make_shared<VarDeclAST>();
            param->start = iden;
            param->end = tok->peek();
            param->ident = make_shared<IdentDefAST>(name);
            param->type = type;
            param->byRef = byRef;
            symbolTable.newSymbol(name, param);
            params.push_back(param);
        }
        if (tok->peek(Token::SEMICOLON)) {
            tok->pop(Token::SEMICOLON);
        }
    }
    tok->pop(Token::RPAREN);
    if (tok->peek(Token::COLON)) {
        tok->pop(Token::COLON);
        return parseQualIdent();
    } else {
        return nullptr;
    }
}

shared_ptr<TypeAST> Parser::parseType() {
    shared_ptr<Token> keyword = tok->popAny({
            Token::IDENTIFIER, Token::ARRAY, Token::RECORD, Token::POINTER, Token::PROCEDURE});
    switch(keyword->getKind()) {
    case Token::IDENTIFIER:
        tok->push(keyword);
        return parseSimpleType(keyword);
    case Token::ARRAY:
        return parseArrayType(keyword);
    case Token::RECORD:
        return parseRecordType(keyword);
    case Token::POINTER:
        return parsePointerType(keyword);
    case Token::PROCEDURE:
        return parseProcedureType(keyword);
    default:
        return nullptr;
    }
}

shared_ptr<TypeAST> Parser::parseProcedureType(shared_ptr<Token> start) {
    auto type = make_shared<ProcedureTypeAST>();
    type->ret = parseFormalParams(type->params);
    type->start = start;
    type->end = tok->peek();
    return type;
}

shared_ptr<TypeAST> Parser::parsePointerType(shared_ptr<Token> start) {
    auto type = make_shared<PointerTypeAST>();
    tok->pop(Token::TO);
    type->pointee = parseType();
    type->start = start;
    type->end = tok->peek();
    return type;
}

shared_ptr<TypeAST> Parser::parseRecordType(shared_ptr<Token> start) {
    auto type = make_shared<RecordTypeAST>();
    if (tok->peek(Token::LPAREN)) {
        tok->pop(Token::LPAREN);
        type->base = parseQualIdent();
        tok->pop(Token::RPAREN);
        auto base = symbolTable.findSymbol(type->base->name);
        auto decl = dynamic_pointer_cast<TypeDeclAST>(base);
        if (decl) {
            type->baseType = decl->type;
        }
    }
    while (1) {
        bool had_semicolon = false;
        vector<shared_ptr<IdentDefAST>> idents;
        idents.push_back(parseIdentDef());
        while (tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
            idents.push_back(parseIdentDef());
        }
        tok->pop(Token::COLON);
        auto ft = parseType();
        for (auto ident : idents) {
            type->fields.push_back(make_pair(ident, ft));
        }
        if (tok->peek(Token::SEMICOLON)) {
            while (tok->peek(Token::SEMICOLON)) {
                tok->pop(Token::SEMICOLON);
            }
            had_semicolon = true;
        }
        if (tok->peek(Token::END)) {
            break;
        } else if (!had_semicolon) {
            auto keyword = tok->pop();
            throw ParserException("Expected ';' or 'END' after RECORD FieldList, got " + keyword->getText(), keyword->getLocation());
        }
    }
    type->start = start;
    type->end = tok->pop(Token::END);
    return type;
}

shared_ptr<TypeAST> Parser::parseArrayType(shared_ptr<Token> start) {
    auto type = make_shared<ArrayTypeAST>();
    if (!tok->peek(Token::OF)) {
        parseConstExpr();
        while (tok->peek(Token::COMMA)) {
            tok->pop(Token::COMMA);
            parseConstExpr();
        }
    }
    tok->pop(Token::OF);
    type->arrayOf = parseType();
    type->start = start;
    type->end = tok->peek();
    return type;
}

shared_ptr<TypeAST> Parser::parseSimpleType(shared_ptr<Token> start) {
    shared_ptr<TypeAST> type;
    auto qid = parseQualIdent();
    auto sym = symbolTable.findSymbol(qid->name);
    if (sym) {
        auto symType = dynamic_pointer_cast<TypeDeclAST>(sym);
        if (symType) {
            type = symType->type;
        } else {
            throw ParserException("Type expected: " + qid->name, start->getLocation());
        }
    } else {
        type = make_shared<BasicTypeAST>(qid);
    }
    type->start = start;
    type->end = start;
    return type;
}

shared_ptr<ExprAST> Parser::parseConstExpr() {
    auto expr = parseExpr();
    if (expr) {
        expr->isConst = true;
    }
    return expr;
}

shared_ptr<ExprAST> Parser::parseExpr() {
    auto start = tok->peek();
    shared_ptr<ExprAST> LHS = parseUnaryExpr();
    if (!LHS) {
        return nullptr;
    }
    shared_ptr<ExprAST> lexpr = parseBinOpRHS(LHS, 0);
    shared_ptr<ExprAST> rexpr = nullptr;
    if (tok->peek(Token::RELATION)) {
        auto rel = tok->pop(Token::RELATION);

        LHS = parseUnaryExpr();
        if (!LHS) {
            return nullptr;
        }
        rexpr = parseBinOpRHS(LHS, 0);
        auto binExpr = make_shared<BinExprAST>(rel->getText(), lexpr, rexpr);
        binExpr->start = start;
        binExpr->end = tok->peek();
        return binExpr;
    }
    return lexpr;
}

shared_ptr<ExprAST> Parser::parseUnaryExpr() {
    auto factor = parseFactor();
    if (factor) {
        return factor;
    }
    auto op = tok->pop(Token::OPERATOR);
    auto operand = parseUnaryExpr();
    if (!op) {
        return nullptr;
    }
    auto expr = make_shared<UnExprAST>(op->getText(), operand);
    expr->start = op;
    expr->end = tok->peek();
    return expr;
}

shared_ptr<ExprAST> Parser::parseBinOpRHS(shared_ptr<ExprAST> LHS, int exprPrec) {
    while (1) {
        auto op = tok->peek(Token::OPERATOR);
        int opPrec = getPrecedence(op);

        if (opPrec < exprPrec) {
            return LHS;
        }

        tok->pop(Token::OPERATOR);

        shared_ptr<ExprAST> RHS = parseUnaryExpr();
        if (!RHS) {
            return nullptr;
        }

        auto nextOp = tok->peek(Token::OPERATOR);
        int nextPrec = getPrecedence(nextOp);
        if (opPrec < nextPrec) {
            RHS = parseBinOpRHS(RHS, opPrec + 1);
            if (!RHS) {
                return nullptr;
            }
        }

        auto start = LHS->start;
        LHS = make_shared<BinExprAST>(op->getText(), LHS, RHS);
        LHS->start = start;
        LHS->end = tok->peek();
    }
}

shared_ptr<ExprAST> Parser::parseFactor() {
    shared_ptr<Token> keyword = tok->peekAny({
            Token::IDENTIFIER, Token::BOOLLITERAL, Token::INTLITERAL,
            Token::FLOATLITERAL, Token::STRLITERAL, Token::CHARLITERAL, Token::NIL,
            Token::LCURLY, Token::LPAREN, Token::TILDE
    });
    if (!keyword) {
        return nullptr;
    }
    switch(keyword->getKind()) {
    case Token::IDENTIFIER: {
        // Variable/Constant or Procedure call
        auto des = parseDesignator();
        if (tok->peek(Token::LPAREN)) {
            auto call = make_shared<CallStatementAST>(des);
            call->start = keyword;
            tok->pop(Token::LPAREN);
            if (!tok->peek(Token::RPAREN)) {
                call->args.push_back(parseExpr());
                while (tok->peek(Token::COMMA)) {
                    tok->pop(Token::COMMA);
                    call->args.push_back(parseExpr());
                }
            }
            call->end = tok->pop(Token::RPAREN);
            auto callExpr = make_shared<CallExprAST>(call);
            callExpr->start = call->start;
            callExpr->end = call->end;
            return callExpr;
        }
        auto iden = make_shared<IdentifierAST>(des);
        iden->start = keyword;
        iden->end = tok->peek();
        return iden;
    }
    case Token::BOOLLITERAL: {
        tok->pop(Token::BOOLLITERAL);
        auto lit = make_shared<BoolLiteralAST>(keyword->getBoolVal());
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::INTLITERAL: {
        tok->pop(Token::INTLITERAL);
        auto lit = make_shared<IntLiteralAST>(keyword->getIntVal());
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::FLOATLITERAL: {
        tok->pop(Token::FLOATLITERAL);
        auto lit = make_shared<FloatLiteralAST>(keyword->getFloatVal());
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::STRLITERAL: {
        tok->pop(Token::STRLITERAL);
        auto lit = make_shared<StrLiteralAST>(keyword->getText());
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::CHARLITERAL: {
        tok->pop(Token::CHARLITERAL);
        auto lit = make_shared<CharLiteralAST>(keyword->getCharVal());
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::NIL: {
        tok->pop(Token::NIL);
        auto lit = make_shared<NilLiteralAST>();
        lit->start = keyword;
        lit->end = keyword;
        return lit;
    }
    case Token::LCURLY: {
        // SET
        auto set = make_shared<SetLiteralAST>();
        tok->pop(Token::LCURLY);
        if (!tok->peek(Token::RCURLY)) {
            shared_ptr<ExprAST> first = parseExpr();
            shared_ptr<ExprAST> second = nullptr;
            if (tok->peek(Token::RANGE)) {
                tok->pop(Token::RANGE);
                second = parseExpr();
            }
            set->elements.push_back(make_pair(first, second));
            while (tok->peek(Token::COMMA)) {
                tok->pop(Token::COMMA);
                first = parseExpr();
                second = nullptr;
                if (tok->peek(Token::RANGE)) {
                    tok->pop(Token::RANGE);
                    second = parseExpr();
                }
                set->elements.push_back(make_pair(first, second));
            }
        }
        set->start = keyword;
        set->end = tok->pop(Token::RCURLY);
        return set;
    }
    case Token::LPAREN: {
        tok->pop(Token::LPAREN);
        auto expr = parseExpr();
        expr->start = keyword;
        expr->end = tok->pop(Token::RPAREN);
        return expr;
    }
    case Token::TILDE: {
        // NOT
        tok->pop(Token::TILDE);
        auto expr = parseFactor();
        auto neg = make_shared<UnExprAST>("~", expr);
        neg->start = keyword;
        neg->end = tok->peek();
        return neg;
    }
    default:
        break;
    }
    return make_shared<NilLiteralAST>();
}

shared_ptr<DesignatorAST> Parser::parseDesignator() {
    auto start = tok->peek();
    auto qid = parseQualIdent();
    auto des = make_shared<DesignatorAST>(qid);
    des->start = start;
    bool end = false;
    while (tok->peekAny({Token::DOT, Token::LSQUARE, Token::CARET, Token::LPAREN}) && !end) {
        shared_ptr<Token> keyword = tok->peekAny({Token::DOT, Token::LSQUARE, Token::CARET, Token::LPAREN});
        switch(keyword->getKind()) {
        case Token::DOT: {
            // QUALIFIER
            // Ensure identifier is a record or pointer
            auto sym = symbolTable.findSymbol(qid->name);
            if (!sym) {
                throw ParserException("Not a symbol: " + qid->name, keyword->getLocation());
            }
            auto symDecl = dynamic_pointer_cast<VarDeclAST>(sym);
            if (!symDecl) {
                throw ParserException("Not a variable: " + qid->name, keyword->getLocation());
            }
            if (!dynamic_pointer_cast<RecordTypeAST>(symDecl->type) &&
                !dynamic_pointer_cast<PointerTypeAST>(symDecl->type)) {
                throw ParserException("Not a record or pointer: " + qid->name, keyword->getLocation());
            }
            tok->pop(Token::DOT);
            auto ident = tok->pop(Token::IDENTIFIER);
            auto part = make_shared<DesignatorIdentPartAST>(ident->getText());
            part->start = keyword;
            part->end = ident;
            des->parts.push_back(part);
            break;
        }
        case Token::LSQUARE: {
            // ARRAY INDEX
            // Ensure identifier is an array and that expr type is integer
            auto part = make_shared<DesignatorArrayPartAST>();
            part->start = tok->pop(Token::LSQUARE);
            part->exprs.push_back(parseExpr());
            while (tok->peek(Token::COMMA)) {
                tok->pop(Token::COMMA);
                part->exprs.push_back(parseExpr());
            }
            part->end = tok->pop(Token::RSQUARE);
            des->parts.push_back(part);
            break;
        }
        case Token::CARET: {
            // POINTER DEREF
            // Ensure identifier is a pointer
            auto sym = symbolTable.findSymbol(qid->name);
            if (!sym) {
                throw ParserException("Not a symbol: " + qid->name, keyword->getLocation());
            }
            auto symDecl = dynamic_pointer_cast<VarDeclAST>(sym);
            if (!symDecl) {
                throw ParserException("Not a variable: " + qid->name, keyword->getLocation());
            }
            auto type = dynamic_pointer_cast<PointerTypeAST>(symDecl->type);
            if (!type) {
                throw ParserException("Not a pointer: " + qid->name, keyword->getLocation());
            }
            auto part = make_shared<DesignatorDerefPartAST>();
            part->start = keyword;
            part->end = tok->pop(Token::CARET);
            des->parts.push_back(part);
            break;
        }
        case Token::LPAREN: {
            // Type Guard v(T)
            // Ensure v is of type RECORD or POINTER and T is an extension of static type of v
            // NOTE: If it's *not* a type guard, it *could* be a procedure call, terminate the
            //       designator early, a higher level will check for that case.
            auto sym = symbolTable.findSymbol(qid->name);
            if (!sym) {
                end = true;
                break;
            }
            auto symDecl = dynamic_pointer_cast<VarDeclAST>(sym);
            if (!symDecl) {
                end = true;
                break;
            }
            if (!dynamic_pointer_cast<RecordTypeAST>(symDecl->type) &&
                !dynamic_pointer_cast<PointerTypeAST>(symDecl->type)) {
                end = true;
                break;
            }
            tok->pop(Token::LPAREN);
            auto part = make_shared<DesignatorCastPartAST>(parseQualIdent());
            part->start = keyword;
            part->end = tok->pop(Token::RPAREN);
            des->parts.push_back(part);
            break;
        }
        default:
            break;
        }
    }
    des->end = tok->peek();
    return des;
}

shared_ptr<IdentDefAST> Parser::parseIdentDef() {
    auto ident = tok->pop(Token::IDENTIFIER);
    auto next  = tok->peek(Token::OPERATOR);
    Export exprt = Export::NO;
    if (next) {
        tok->pop(Token::OPERATOR);
        if (next->getText() == "*") {
            exprt = Export::YES;
        } else if (next->getText() == "-") {
            exprt = Export::RO;
        } else {
            throw new ParserException("Expected export indicator * or -, got " + next->getText(), next->getLocation());
        }
    }
    return make_shared<IdentDefAST>(ident->getText(), exprt);
}

shared_ptr<QualIdentAST> Parser::parseQualIdent() {
    auto qid = make_shared<QualIdentAST>();
    qid->start = tok->peek();
    shared_ptr<Token> name = tok->pop(Token::IDENTIFIER);
    qid->name = name->getText();
    auto sym = symbolTable.findSymbol(qid->name);
    qid->module = (shared_ptr<ModuleAST>) dynamic_pointer_cast<ModuleAST>(sym);
    if (qid->module) {
        tok->pop(Token::DOT);
        name = tok->pop(Token::IDENTIFIER);
        qid->name = name->getText();
    }
    qid->end = name;
    return qid;
}

int Parser::getPrecedence(shared_ptr<Token> op) {
    if (op) {
        int prec = precedence[op->getText()];
        if (prec > 0) {
            return prec;
        }
    }
    return -1;
}
