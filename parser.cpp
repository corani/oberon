#include <vector>
#include <algorithm>
#include <utility>
#include "parser.h"

using namespace std;

const char* ParserException::what() const noexcept {
    cout << msg << " at " << loc << endl;
    return msg.c_str();
}

Parser::Parser() : lexer(nullptr), currentToken(nullptr) {
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

void Parser::push(shared_ptr<Token> token) {
    tokens.push(currentToken);
    currentToken = token;
}

shared_ptr<Token> Parser::pop() {
    shared_ptr<Token> old = currentToken;
    if (tokens.empty()) {
        currentToken = lexer->nextToken();
    } else {
        currentToken = tokens.top();
        tokens.pop();
    }
    return old;
}

shared_ptr<Token> Parser::peek() {
    return currentToken;
}

shared_ptr<Token> Parser::pop(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return pop();
    }
    throw ParserException("Expected " + kind_to_string(kind) + ", got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Parser::peek(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return currentToken;
    }
    return nullptr;
}

shared_ptr<Token> Parser::pop(vector<Token::Kind> kinds) {
    string str_kinds;
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return pop();
        }
        str_kinds += kind_to_string(kind) + ", ";
    }
    throw ParserException("Expected one of " + str_kinds + "got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Parser::peek(vector<Token::Kind> kinds) {
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return currentToken;
        }
    }
    return nullptr;
}

void Parser::enterScope() {
    scope++;
    if (scope >= symbols.capacity()) {
        symbols.push_back({});
    }
}

void Parser::leaveScope() {
    if (scope >= 0) {
        symbols[scope].clear();
        scope--;
    }
}

void Parser::newSymbol(string name, shared_ptr<DeclAST> ast) {
    symbols.at(scope).push_back(make_pair(name, ast));
}

shared_ptr<DeclAST> Parser::findSymbol(string name) {
    for (int i = scope; i > 0; i--) {
        auto sv = symbols[i];
        for (auto p : sv) {
            if (p.first == name) {
                return p.second;
            }
        }
    };
    return nullptr;
}

shared_ptr<ModuleAST> Parser::parseModule(shared_ptr<Lexer> _lexer) {
    lexer = _lexer;
    pop(); // Prime the pump

    auto start = pop(Token::MODULE);
    auto name = pop(Token::IDENTIFIER);
    pop(Token::SEMICOLON);

    auto module = make_shared<ModuleAST>(name->getText());
    enterScope();
    if (module->name != "STD") enterScope();
    newSymbol(module->name, module);

    if (peek(Token::IMPORT)) {
        parseImport(module->imports);
    }

    while (auto keyword = peek({ Token::TYPE, Token::CONST, Token::VAR })) {
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

    while (auto keyword = peek({ Token::PROCEDURE, Token::EXTERN, Token::SEMICOLON })) {
        switch(keyword->getKind()) {
        case Token::PROCEDURE:
            parseProcDecl(module->decls);
            break;
        case Token::EXTERN:
            parseExternDecl(module->decls);
            break;
        default:
            pop();
            break;
        }
    }

    if (peek(Token::BEGIN)) {
        pop(Token::BEGIN);
        parseStatementSeq(module->stmts);
    }

    pop(Token::END);
    // TODO: Warning if names don't match
    pop(Token::IDENTIFIER);
    auto end = pop(Token::DOT);

    module->start = start;
    module->end = end;

    leaveScope();

    return module;
}

void Parser::parseImport(vector<pair<string, string>> &imports) {
    pop(Token::IMPORT);
    do {
        pair<string, string> import;
        auto tok = pop(Token::IDENTIFIER);
        import.first = tok->getText();

        if (peek(Token::ASSIGNMENT)) {
            pop(Token::ASSIGNMENT);
            tok = pop(Token::IDENTIFIER);
            import.second = tok->getText();
        } else {
            import.second = "";
        }
        if (peek(Token::COMMA)) {
            pop(Token::COMMA);
        }
        imports.push_back(import);
        newSymbol(import.first, nullptr);
    } while (!peek(Token::SEMICOLON));
    pop(Token::SEMICOLON);
}

void Parser::parseTypeDecl(vector<shared_ptr<DeclAST>> &decls) {
    pop(Token::TYPE);
    do {
        auto decl = make_shared<TypeDeclAST>();
        decl->start = peek();
        decl->ident = parseIdentDef();
        shared_ptr<Token> tok = pop(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '=', got " + tok->getText(), tok->getLocation());
        }
        decl->type = parseType();
        decl->end = pop(Token::SEMICOLON);
        newSymbol(decl->ident->name, decl);
        decls.push_back(decl);
    } while (peek(Token::IDENTIFIER));
}

void Parser::parseConstDecl(vector<shared_ptr<DeclAST>> &decls) {
    pop(Token::CONST);
    do {
        auto decl = make_shared<ConstDeclAST>();
        decl->start = peek();
        decl->ident = parseIdentDef();
        shared_ptr<Token> tok = pop(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '='. got " + tok->getText(), tok->getLocation());
        }
        decl->expr = parseConstExpr();
        decl->end = pop(Token::SEMICOLON);
        newSymbol(decl->ident->name, decl);
        decls.push_back(decl);
    } while (peek(Token::IDENTIFIER));
}

void Parser::parseVarDecl(vector<shared_ptr<DeclAST>> &decls) {
    pop(Token::VAR);
    do {
        vector<shared_ptr<VarDeclAST>> vars;
        auto start = peek();
        auto decl = make_shared<VarDeclAST>();
        decl->ident = parseIdentDef();
        vars.push_back(decl);
        while (peek(Token::COMMA)) {
            pop(Token::COMMA);
            decl = make_shared<VarDeclAST>();
            decl->ident = parseIdentDef();
            vars.push_back(decl);
        }
        pop(Token::COLON);
        auto type = parseType();
        auto end = pop(Token::SEMICOLON);

        for (auto var : vars) {
            var->start = start;
            var->end = end;
            var->type = type;
            newSymbol(var->ident->name, var);
            decls.push_back(var);
        }
    } while (peek(Token::IDENTIFIER));
}

void Parser::parseExternDecl(vector<shared_ptr<DeclAST>> &decls) {
    auto start = pop(Token::EXTERN);
    auto ident = parseIdentDef();
    auto _extern = make_shared<ExternDeclAST>(ident);
    _extern->ret = parseFormalParams(_extern->params);
    _extern->start = start;
    _extern->end = peek();
    newSymbol(ident->name, _extern);
    decls.push_back(_extern);
}

void Parser::parseForwardDecl(shared_ptr<Token> start, vector<shared_ptr<DeclAST>> &decls) {
    pop(Token::CARET);
    if (peek(Token::LPAREN)) {
        parseReceiver();
    }
    auto ident = parseIdentDef();
    auto forward = make_shared<ForwardDeclAST>(ident);
    forward->ret = parseFormalParams(forward->params);
    forward->start = start;
    forward->end = peek();
    newSymbol(ident->name, forward);
    decls.push_back(forward);
}

void Parser::parseProcDecl(vector<shared_ptr<DeclAST>> &decls) {
    auto start = pop(Token::PROCEDURE);
    if (currentToken->getKind() == Token::CARET) {
        parseForwardDecl(start, decls);
    } else {
        if (peek(Token::LPAREN)) {
            parseReceiver();
        }
        auto ident = parseIdentDef();
        auto proc = make_shared<ProcDeclAST>(ident);
        newSymbol(ident->name, proc);

        enterScope();
        proc->ret = parseFormalParams(proc->params);
        pop(Token::SEMICOLON);

        while (auto keyword = peek({ Token::TYPE, Token::CONST, Token::VAR })) {
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

        while (peek(Token::PROCEDURE)) {
            parseProcDecl(proc->decls);
        }

        if (peek(Token::BEGIN)) {
            pop(Token::BEGIN);
            parseStatementSeq(proc->stmts);
        }

        leaveScope();

        pop(Token::END);
        auto end = pop(Token::IDENTIFIER);

        proc->start = start;
        proc->end = end;

        decls.push_back(proc);
    }
}

void Parser::parseStatementSeq(vector<shared_ptr<StatementAST>> &stmts) {
    while (1) {
        // To allow semicolon after the last statement
        if (peek({Token::END, Token::UNTIL, Token::ELSIF, Token::ELSE, Token::PIPE})) {
            break;
        }
        auto keyword = peek({ Token::IDENTIFIER, Token::IF, Token::CASE, Token::WHILE, Token::REPEAT, Token::FOR, Token::LOOP, Token::WITH, Token::EXIT, Token::RETURN });
        if (!keyword) {
            keyword = pop();
            throw ParserException("Expected statement, got " + kind_to_string(keyword->getKind()), keyword->getLocation());
        }
        switch(keyword->getKind()) {
        case Token::IDENTIFIER: {
            auto des = parseDesignator();
            if (peek(Token::ASSIGNMENT)) {
                // Assignment
                auto sym = findSymbol(des->qid->name);
                auto var = dynamic_cast<VarDeclAST *>(sym.get());
                if (var) {
                    auto assign = make_shared<AssignStatementAST>(des);
                    assign->start = keyword;
                    pop(Token::ASSIGNMENT);
                    assign->expr = parseExpr();
                    assign->end = peek();
                    stmts.push_back(assign);
                } else {
                    throw ParserException("Modifiable variable expected", keyword->getLocation());
                }
            } else if (peek(Token::LPAREN)) {
                // Call with parameters
                auto call = make_shared<CallStatementAST>(des);
                call->start = keyword;
                pop(Token::LPAREN);
                if (!peek(Token::RPAREN)) {
                    call->args.push_back(parseExpr());
                    while (peek(Token::COMMA)) {
                        pop(Token::COMMA);
                        call->args.push_back(parseExpr());
                    }
                }
                call->end = pop(Token::RPAREN);
                stmts.push_back(call);
            } else {
                // Naked call
                auto call = make_shared<CallStatementAST>(des);
                call->start = keyword;
                call->end = peek();
                stmts.push_back(call);
            }
            break;
        }
        case Token::IF: {
            auto start = pop(Token::IF);
            auto cond = parseExpr();
            auto _if = make_shared<IfStatementAST>(cond);
            _if->start = start;
            auto root = _if;
            pop(Token::THEN);
            parseStatementSeq(_if->thenStmts);
            while (peek(Token::ELSIF)) {
                start = pop(Token::ELSIF);
                cond = parseExpr();
                auto _newif = make_shared<IfStatementAST>(cond);
                _newif->start = start;
                _if->end = start;
                pop(Token::THEN);
                parseStatementSeq(_newif->thenStmts);
                _if->elseStmts.push_back(_newif);
                _if = _newif;
            }
            if (peek(Token::ELSE)) {
                pop(Token::ELSE);
                parseStatementSeq(_if->elseStmts);
            }
            _if->end = pop(Token::END);
            stmts.push_back(root);
            break;
        }
        case Token::CASE: {
            pop(Token::CASE);
            auto cond = parseExpr();
            auto _case = make_shared<CaseStatementAST>(cond);
            _case->start = keyword;
            pop(Token::OF);
            parseCase(_case->clauses);
            while (peek(Token::PIPE)) {
                pop(Token::PIPE);
                parseCase(_case->clauses);
            }
            if (peek(Token::ELSE)) {
                pop(Token::ELSE);
                parseStatementSeq(_case->elseStmts);
            }
            _case->end = pop(Token::END);
            stmts.push_back(_case);
            break;
        }
        case Token::WHILE: {
            pop(Token::WHILE);
            auto cond = parseExpr();
            auto _while = make_shared<WhileStatementAST>(cond);
            _while->start = keyword;
            pop(Token::DO);
            parseStatementSeq(_while->stmts);
            _while->end = pop(Token::END);
            stmts.push_back(_while);
            break;
        }
        case Token::REPEAT: {
            auto repeat = make_shared<RepeatStatementAST>();
            repeat->start = pop(Token::REPEAT);
            parseStatementSeq(repeat->stmts);
            pop(Token::UNTIL);
            repeat->cond = parseExpr();
            repeat->end = peek();
            stmts.push_back(repeat);
            break;
        }
        case Token::FOR: {
            auto _for = make_shared<ForStatementAST>();
            _for->start = pop(Token::FOR);
            auto iden = pop(Token::IDENTIFIER);
            _for->iden = iden->getText();
            pop(Token::ASSIGNMENT);
            _for->from = parseExpr();
            pop(Token::TO);
            _for->to = parseExpr();
            if (peek(Token::BY)) {
                pop(Token::BY);
                _for->by = parseConstExpr();
            } else {
                _for->by = nullptr;
            }
            pop(Token::DO);
            parseStatementSeq(_for->stmts);
            _for->end = pop(Token::END);
            stmts.push_back(_for);
            break;
        }
        case Token::LOOP: {
            auto loop = make_shared<LoopStatementAST>();
            loop->start = pop(Token::LOOP);
            parseStatementSeq(loop->stmts);
            loop->end = pop(Token::END);
            stmts.push_back(loop);
            break;
        }
        case Token::WITH: {
            auto with = make_shared<WithStatementAST>();
            with->start = pop(Token::WITH);
            parseWithGuard(with->clauses);
            while (peek(Token::PIPE)) {
                pop(Token::PIPE);
                parseWithGuard(with->clauses);
            }
            if (peek(Token::ELSE)) {
                pop(Token::ELSE);
                parseStatementSeq(with->elseStmts);
            }
            with->end = pop(Token::END);
            stmts.push_back(with);
            break;
        }
        case Token::EXIT: {
            auto exit = make_shared<ExitStatementAST>();
            exit->start = pop(Token::EXIT);
            exit->end = exit->start;
            stmts.push_back(exit);
            break;
        }
        case Token::RETURN: {
            auto _return = make_shared<ReturnStatementAST>();
            _return->start = pop(Token::RETURN);
            if (!peek({ Token::SEMICOLON, Token::END })) {
                _return->expr = parseExpr();
            }
            _return->end = peek();
            stmts.push_back(_return);
            break;
        }
        default:
            break;
        }
        if (!peek(Token::SEMICOLON)) {
            break;
        }
        while (peek(Token::SEMICOLON)) {
            pop(Token::SEMICOLON);
        }
    }
}

void Parser::parseWithGuard(vector<shared_ptr<WithClauseAST>> &clauses) {
    auto clause = make_shared<WithClauseAST>();
    clause->name = parseQualIdent();
    pop(Token::COLON);
    clause->type = parseQualIdent();
    pop(Token::DO);
    parseStatementSeq(clause->stmts);
    clauses.push_back(clause);
}

void Parser::parseCase(vector<shared_ptr<CaseClauseAST>> &clauses) {
    shared_ptr<CaseClauseAST> clause = make_shared<CaseClauseAST>();
    clause->start = peek();
    pair<shared_ptr<ExprAST>, shared_ptr<ExprAST>> item;
    item.first = parseConstExpr();
    if (peek(Token::RANGE)) {
        pop(Token::RANGE);
        item.second = parseConstExpr();
    } else {
        item.second = nullptr;
    }
    clause->when.push_back(item);

    while (peek(Token::COMMA)) {
        pop(Token::COMMA);
        item.first = parseConstExpr();
        if (peek(Token::RANGE)) {
            pop(Token::RANGE);
            item.second = parseConstExpr();
        } else {
            item.second = nullptr;
        }
        clause->when.push_back(item);
    }
    pop(Token::COLON);
    parseStatementSeq(clause->stmts);

    clause->end = peek();

    clauses.push_back(clause);
}

shared_ptr<ReceiverAST> Parser::parseReceiver() {
    auto receiver = make_shared<ReceiverAST>();
    receiver->start = pop(Token::LPAREN);
    if (peek(Token::VAR)) {
        pop(Token::VAR);
        receiver->byRef = true;
    }
    auto name = pop(Token::IDENTIFIER);
    pop(Token::COLON);
    auto type = pop(Token::IDENTIFIER);
    receiver->name = name->getText();
    receiver->type = type->getText();
    receiver->end = pop(Token::RPAREN);

    auto var = make_shared<VarDeclAST>();
    var->ident = make_shared<IdentDefAST>(name->getText());

    auto sym = findSymbol(type->getText());
    if (sym) {
        auto typeDecl = dynamic_cast<TypeDeclAST *>(sym.get());
        if (typeDecl) {
            var->type = typeDecl->type;
        }
    }
    newSymbol(name->getText(), var);

    return receiver;
}

shared_ptr<QualIdentAST> Parser::parseFormalParams(vector<shared_ptr<VarDeclAST>> &params) {
    pop(Token::LPAREN);
    while (!peek(Token::RPAREN)) {
        bool byRef = false;
        vector<string> names;
        if (peek(Token::VAR)) {
            pop(Token::VAR);
            byRef = true;
        }
        auto iden = pop(Token::IDENTIFIER);
        names.push_back(iden->getText());
        while(peek(Token::COMMA)) {
            pop(Token::COMMA);
            iden = pop(Token::IDENTIFIER);
            names.push_back(iden->getText());
        }
        pop(Token::COLON);
        auto type = parseType();
        for (string name : names) {
            auto param = make_shared<VarDeclAST>();
            param->start = iden;
            param->end = peek();
            param->ident = make_shared<IdentDefAST>(name);
            param->type = type;
            param->byRef = byRef;
            newSymbol(name, param);
            params.push_back(param);
        }
        if (peek(Token::SEMICOLON)) {
            pop(Token::SEMICOLON);
        }
    }
    pop(Token::RPAREN);
    if (peek(Token::COLON)) {
        pop(Token::COLON);
        return parseQualIdent();
    } else {
        return nullptr;
    }
}

shared_ptr<TypeAST> Parser::parseType() {
    auto keyword = pop({ Token::IDENTIFIER, Token::ARRAY, Token::RECORD, Token::POINTER, Token::PROCEDURE });
    switch(keyword->getKind()) {
    case Token::IDENTIFIER: {
        push(keyword);
        auto type = make_shared<BasicTypeAST>(parseQualIdent());
        type->start = keyword;
        type->end = keyword;
        return type;
    }
    case Token::ARRAY: {
        auto type = make_shared<ArrayTypeAST>();
        type->start = keyword;
        if (!peek(Token::OF)) {
            parseConstExpr();
            while (peek(Token::COMMA)) {
                pop(Token::COMMA);
                parseConstExpr();
            }
        }
        pop(Token::OF);
        type->arrayOf = parseType();
        type->end = peek();
        return type;
    }
    case Token::RECORD: {
        auto type = make_shared<RecordTypeAST>();
        type->start = keyword;
        if (peek(Token::LPAREN)) {
            pop(Token::LPAREN);
            type->base = parseQualIdent();
            pop(Token::RPAREN);
            auto base = findSymbol(type->base->name);
            TypeDeclAST *decl = dynamic_cast<TypeDeclAST *>(base.get());
            if (decl) {
                type->baseType = decl->type;
            }
        }
        while (1) {
            vector<shared_ptr<IdentDefAST>> idents;
            idents.push_back(parseIdentDef());
            while (peek(Token::COMMA)) {
                pop(Token::COMMA);
                idents.push_back(parseIdentDef());
            }
            pop(Token::COLON);
            auto ft = parseType();
            for (auto ident : idents) {
                type->fields.push_back(make_pair(ident, ft));
            }
            while (peek(Token::SEMICOLON)) {
                pop(Token::SEMICOLON);
            }
            if (peek(Token::END)) {
                break;
            } else {
                auto tok = pop();
                throw ParserException("Expected ';' or 'END' after RECORD FieldList, got " + tok->getText(), tok->getLocation());
            }
        }
        type->end = pop(Token::END);
        return type;
    }
    case Token::POINTER: {
        auto type = make_shared<PointerTypeAST>();
        type->start = keyword;
        pop(Token::TO);
        type->pointee = parseType();
        type->end = peek();
        return type;
    }
    case Token::PROCEDURE: {
        auto type = make_shared<ProcedureTypeAST>();
        type->start = keyword;
        type->ret = parseFormalParams(type->params);
        type->end = peek();
        return type;
    }
    default:
        return nullptr;
    }
    return nullptr;
}

shared_ptr<ExprAST> Parser::parseConstExpr() {
    auto expr = parseExpr();
    if (expr) {
        expr->isConst = true;
    }
    return expr;
}

shared_ptr<ExprAST> Parser::parseExpr() {
    auto start = peek();
    shared_ptr<ExprAST> LHS = parseUnaryExpr();
    if (!LHS) {
        return nullptr;
    }
    shared_ptr<ExprAST> lexpr = parseBinOpRHS(LHS, 0);
    shared_ptr<ExprAST> rexpr = nullptr;
    if (peek(Token::RELATION)) {
        auto rel = pop(Token::RELATION);

        LHS = parseUnaryExpr();
        if (!LHS) {
            return nullptr;
        }
        rexpr = parseBinOpRHS(LHS, 0);
        auto binExpr = make_shared<BinExprAST>(rel->getText(), lexpr, rexpr);
        binExpr->start = start;
        binExpr->end = peek();
        return binExpr;
    }
    return lexpr;
}

shared_ptr<ExprAST> Parser::parseUnaryExpr() {
    auto factor = parseFactor();
    if (factor) {
        return factor;
    }
    auto op = pop(Token::OPERATOR);
    auto operand = parseUnaryExpr();
    if (!op) {
        return nullptr;
    }
    return make_shared<UnExprAST>(op->getText(), operand);
}

shared_ptr<ExprAST> Parser::parseBinOpRHS(shared_ptr<ExprAST> LHS, int exprPrec) {
    while (1) {
        auto op = peek(Token::OPERATOR);
        int opPrec = getPrecedence(op);

        if (opPrec < exprPrec) {
            return LHS;
        }

        pop(Token::OPERATOR);

        shared_ptr<ExprAST> RHS = parseUnaryExpr();
        if (!RHS) {
            return nullptr;
        }

        auto nextOp = peek(Token::OPERATOR);
        int nextPrec = getPrecedence(nextOp);
        if (opPrec < nextPrec) {
            RHS = parseBinOpRHS(RHS, opPrec + 1);
            if (!RHS) {
                return nullptr;
            }
        }

        LHS = make_shared<BinExprAST>(op->getText(), LHS, RHS);
    }
    return nullptr;
}

shared_ptr<ExprAST> Parser::parseFactor() {
    auto tok = peek({ Token::IDENTIFIER, Token::BOOLLITERAL, Token::INTLITERAL, Token::FLOATLITERAL, Token::STRLITERAL, Token::CHARLITERAL, Token::NIL,
                        Token::LCURLY, Token::LPAREN, Token::TILDE });
    if (!tok) {
        return nullptr;
    }
    switch(tok->getKind()) {
    case Token::IDENTIFIER: {
        // Variable/Constant or Procedure call
        auto start = peek();
        auto des = parseDesignator();
        if (peek(Token::LPAREN)) {
            auto call = make_shared<CallStatementAST>(des);
            call->start = start;
            pop(Token::LPAREN);
            if (!peek(Token::RPAREN)) {
                call->args.push_back(parseExpr());
                while (peek(Token::COMMA)) {
                    pop(Token::COMMA);
                    call->args.push_back(parseExpr());
                }
            }
            call->end = pop(Token::RPAREN);
            auto callExpr = make_shared<CallExprAST>(call);
            callExpr->start = call->start;
            callExpr->end = call->end;
            return callExpr;
        }
        auto iden = make_shared<IdentifierAST>(des);
        iden->start = start;
        iden->end = peek();
        return iden;
    }
    case Token::BOOLLITERAL:
        tok = pop(Token::BOOLLITERAL);
        return make_shared<BoolLiteralAST>(tok->getBoolVal());
    case Token::INTLITERAL:
        tok = pop(Token::INTLITERAL);
        return make_shared<IntLiteralAST>(tok->getIntVal());
    case Token::FLOATLITERAL:
        tok = pop(Token::FLOATLITERAL);
        return make_shared<FloatLiteralAST>(tok->getFloatVal());
    case Token::STRLITERAL:
        tok = pop(Token::STRLITERAL);
        return make_shared<StrLiteralAST>(tok->getText());
    case Token::CHARLITERAL:
        tok = pop(Token::CHARLITERAL);
        return make_shared<CharLiteralAST>(tok->getCharVal());
    case Token::NIL:
        tok = pop(Token::NIL);
        return make_shared<NilLiteralAST>();
    case Token::LCURLY:
        // SET
        pop(Token::LCURLY);
        if (!peek(Token::RCURLY)) {
            parseExpr();
            if (peek(Token::RANGE)) {
                pop(Token::RANGE);
                parseExpr();
            }
            while (peek(Token::COMMA)) {
                pop(Token::COMMA);
                parseExpr();
                if (peek(Token::RANGE)) {
                    pop(Token::RANGE);
                    parseExpr();
                }
            }
        }
        pop(Token::RCURLY);
        break;
    case Token::LPAREN: {
        auto start = pop(Token::LPAREN);
        auto expr = parseExpr();
        expr->start = start;
        expr->end = pop(Token::RPAREN);
        return expr;
    }
    case Token::TILDE:
        // NOT
        pop(Token::TILDE);
        parseFactor();
        break;
    default:
        break;
    }
    return make_shared<NilLiteralAST>();
}

shared_ptr<DesignatorAST> Parser::parseDesignator() {
    auto start = peek();
    auto qid = parseQualIdent();
    auto des = make_shared<DesignatorAST>(qid);
    des->start = start;
    while (peek({ Token::DOT, Token::LSQUARE, Token::CARET /*, Token::LPAREN*/ })) {
        auto tok = pop({ Token::DOT, Token::LSQUARE, Token::CARET/*, Token::LPAREN*/ });
        switch(tok->getKind()) {
        case Token::DOT:
            // QUALIFIER
            pop(Token::IDENTIFIER);
            break;
        case Token::LSQUARE:
            // ARRAY INDEX
            // Ensure identifier is an array
            parseExpr();
            while (peek(Token::COMMA)) {
                pop(Token::COMMA);
                parseExpr();
            }
            pop(Token::RSQUARE);
            break;
        case Token::CARET:
            // POINTER DEREF
            break;
/*      case Token::LPAREN:
            // Type Guard v(T)
            // Ensure v is of type RECORD or POINTER and T is an extension of static type of v
            pop(Token::IDENTIFIER);
            if (peek(Token::DOT)) {
                pop(Token::DOT);
                pop(Token::IDENTIFIER);
            }
            pop(Token::RPAREN);
            break;*/
        default:
            break;
        }
    }
    return des;
}

shared_ptr<IdentDefAST> Parser::parseIdentDef() {
    auto ident = pop(Token::IDENTIFIER);
    auto next  = peek(Token::OPERATOR);
    Export exprt = Export::NO;
    if (next) {
        pop(Token::OPERATOR);
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
    qid->start = peek();
    shared_ptr<Token> module = nullptr;
    shared_ptr<Token> name = pop(Token::IDENTIFIER);
    if (peek(Token::DOT)) {
        pop(Token::DOT);
        module = name;
        name = pop(Token::IDENTIFIER);
    }
    qid->end = name;
    qid->name = name->getText();
    if (module) {
        qid->module = module->getText();
    }
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
