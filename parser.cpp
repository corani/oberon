#include <vector>
#include <algorithm>
#include "parser.h"

using namespace std;

const char* ParserException::what() const noexcept {
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

shared_ptr<Token> Parser::accept() {
    shared_ptr<Token> old = currentToken;
    currentToken = lexer->nextToken();
    return old;
}

shared_ptr<Token> Parser::expect() {
    return currentToken;
}

shared_ptr<Token> Parser::accept(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return accept();
    }
    throw ParserException("Expected " + kind_to_string(kind) + ", got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Parser::expect(Token::Kind kind) {
    if (currentToken->getKind() == kind) {
        return currentToken;
    }
    return nullptr;
}

shared_ptr<Token> Parser::accept(vector<Token::Kind> kinds) {
    string str_kinds;
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return accept();
        }
        str_kinds += kind_to_string(kind) + ", ";
    }
    throw ParserException("Expected one of " + str_kinds + "got " + kind_to_string(currentToken->getKind()), currentToken->getLocation());
}

shared_ptr<Token> Parser::expect(vector<Token::Kind> kinds) {
    for (Token::Kind kind : kinds) {
        if (currentToken->getKind() == kind) {
            return currentToken;
        }
    }
    return nullptr;
}

shared_ptr<ModuleAST> Parser::parseModule(shared_ptr<Lexer> _lexer) {
    lexer = _lexer;
    accept(); // Prime the pump

    accept(Token::MODULE);
    auto name = accept(Token::IDENTIFIER);
    accept(Token::SEMICOLON);

    auto module = make_shared<ModuleAST>(name->getText());

    if (expect(Token::IMPORT)) {
        parseImport(module->imports);
    }

    while (auto keyword = expect({ Token::TYPE, Token::CONST, Token::VAR })) {
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

    while (auto keyword = expect({ Token::PROCEDURE, Token::EXTERN })) {
        switch(keyword->getKind()) {
        case Token::PROCEDURE:
            parseProcDecl(module->decls);
            break;
        case Token::EXTERN:
            parseExternDecl(module->decls);
            break;
        default:
            break;
        }
    }

    if (expect(Token::BEGIN)) {
        accept(Token::BEGIN);
        parseStatementSeq(module->stmts);
    }

    accept(Token::END);
    // TODO: Warning if names don't match
    accept(Token::IDENTIFIER);
    accept(Token::DOT);

    return module;
}

void Parser::parseImport(vector<pair<string, string>> &imports) {
    accept(Token::IMPORT);
    do {
        pair<string, string> import;
        auto tok = accept(Token::IDENTIFIER);
        import.first = tok->getText();

        if (expect(Token::ASSIGNMENT)) {
            accept(Token::ASSIGNMENT);
            tok = accept(Token::IDENTIFIER);
            import.second = tok->getText();
        } else {
            import.second = "";
        }
        if (expect(Token::COMMA)) {
            accept(Token::COMMA);
        }
        imports.push_back(import);
    } while (!expect(Token::SEMICOLON));
    accept(Token::SEMICOLON);
}

void Parser::parseTypeDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::TYPE);
    do {
        parseIdentDef();
        shared_ptr<Token> tok = accept(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '=', got " + tok->getText(), tok->getLocation());
        }
        parseType();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseConstDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::CONST);
    do {
        parseIdentDef();
        shared_ptr<Token> tok = accept(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '='. got " + tok->getText(), tok->getLocation());
        }
        parseConstExpr();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseVarDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::VAR);
    do {
        parseIdentDef();
        while (expect(Token::COMMA)) {
            accept(Token::COMMA);
            parseIdentDef();
        }
        accept(Token::COLON);
        parseType();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseExternDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::EXTERN);
    auto ident = parseIdentDef();
    parseFormalParams();
    decls.push_back(make_shared<ExternDeclAST>(ident));
}

void Parser::parseForwardDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::CARET);
    if (expect(Token::LPAREN)) {
        parseReceiver();
    }
    auto ident = parseIdentDef();
    parseFormalParams();
    decls.push_back(make_shared<ForwardDeclAST>(ident));
}

void Parser::parseProcDecl(vector<shared_ptr<DeclAST>> &decls) {
    accept(Token::PROCEDURE);
    if (currentToken->getKind() == Token::CARET) {
        parseForwardDecl(decls);
    } else {
        if (expect(Token::LPAREN)) {
            parseReceiver();
        }
        auto ident = parseIdentDef();
        parseFormalParams();
        accept(Token::SEMICOLON);

        auto proc = make_shared<ProcDeclAST>(ident);

        while (auto keyword = expect({ Token::TYPE, Token::CONST, Token::VAR })) {
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

        while (expect(Token::PROCEDURE)) {
            parseProcDecl(proc->decls);
        }

        if (expect(Token::BEGIN)) {
            accept(Token::BEGIN);
            parseStatementSeq(proc->stmts);
        }

        accept(Token::END);
        accept(Token::IDENTIFIER);
        accept(Token::SEMICOLON);

        decls.push_back(proc);
    }
}

void Parser::parseStatementSeq(vector<shared_ptr<StatementAST>> &stmts) {
    while (1) {
        auto keyword = expect({ Token::IDENTIFIER, Token::IF, Token::CASE, Token::WHILE, Token::REPEAT, Token::FOR, Token::LOOP, Token::WITH, Token::EXIT, Token::RETURN });
        if (!keyword) {
            keyword = accept();
            throw ParserException("Expected statement, got " + kind_to_string(keyword->getKind()), keyword->getLocation());
        }
        switch(keyword->getKind()) {
        case Token::IDENTIFIER: {
            auto des = parseDesignator();
            if (expect(Token::ASSIGNMENT)) {
                auto assign = make_shared<AssignStatementAST>(des);
                // Assignment
                accept(Token::ASSIGNMENT);
                assign->expr = parseExpr();
                stmts.push_back(assign);
            } else if (expect(Token::LPAREN)) {
                // Call with parameters
                auto call = make_shared<CallStatementAST>(des);
                accept(Token::LPAREN);
                if (!expect(Token::RPAREN)) {
                    call->args.push_back(parseExpr());
                    while (expect(Token::COMMA)) {
                        accept(Token::COMMA);
                        call->args.push_back(parseExpr());
                    }
                }
                accept(Token::RPAREN);
                stmts.push_back(call);
            } else {
                // Naked call
                stmts.push_back(make_shared<CallStatementAST>(des));
            }
            break;
        }
        case Token::IF: {
            accept(Token::IF);
            auto cond = parseExpr();
            auto _if = make_shared<IfStatementAST>(cond);
            auto root = _if;
            accept(Token::THEN);
            parseStatementSeq(_if->thenStmts);
            while (expect(Token::ELSIF)) {
                accept(Token::ELSIF);
                cond = parseExpr();
                auto _newif = make_shared<IfStatementAST>(cond);
                accept(Token::THEN);
                parseStatementSeq(_newif->thenStmts);
                _if->elseStmts.push_back(_newif);
                _if = _newif;
            }
            if (expect(Token::ELSE)) {
                accept(Token::ELSE);
                parseStatementSeq(_if->elseStmts);
            }
            accept(Token::END);
            stmts.push_back(root);
            break;
        }
        case Token::CASE: {
            accept(Token::CASE);
            auto cond = parseExpr();
            auto _case = make_shared<CaseStatementAST>(cond);
            accept(Token::OF);
            parseCase(_case->clauses);
            while (expect(Token::PIPE)) {
                accept(Token::PIPE);
                parseCase(_case->clauses);
            }
            if (expect(Token::ELSE)) {
                accept(Token::CASE);
                parseStatementSeq(_case->elseStmts);
            }
            accept(Token::END);
            stmts.push_back(_case);
            break;
        }
        case Token::WHILE: {
            accept(Token::WHILE);
            auto cond = parseExpr();
            auto _while = make_shared<WhileStatementAST>(cond);
            accept(Token::DO);
            parseStatementSeq(_while->stmts);
            accept(Token::END);
            stmts.push_back(_while);
            break;
        }
        case Token::REPEAT: {
            auto repeat = make_shared<RepeatStatementAST>();
            accept(Token::REPEAT);
            parseStatementSeq(repeat->stmts);
            accept(Token::UNTIL);
            repeat->cond = parseExpr();
            stmts.push_back(repeat);
            break;
        }
        case Token::FOR: {
            auto _for = make_shared<ForStatementAST>();
            accept(Token::FOR);
            auto iden = accept(Token::IDENTIFIER);
            _for->iden = iden->getText();
            accept(Token::ASSIGNMENT);
            _for->from = parseExpr();
            accept(Token::TO);
            _for->to = parseExpr();
            if (expect(Token::BY)) {
                accept(Token::BY);
                _for->by = parseConstExpr();
            } else {
                _for->by = nullptr;
            }
            accept(Token::DO);
            parseStatementSeq(_for->stmts);
            accept(Token::END);
            stmts.push_back(_for);
            break;
        }
        case Token::LOOP: {
            auto loop = make_shared<LoopStatementAST>();
            accept(Token::LOOP);
            parseStatementSeq(loop->stmts);
            accept(Token::END);
            stmts.push_back(loop);
            break;
        }
        case Token::WITH: {
            auto with = make_shared<WithStatementAST>();
            accept(Token::WITH);
            auto name = accept(Token::IDENTIFIER);
            accept(Token::COLON);
            auto type = accept(Token::IDENTIFIER);
            accept(Token::DO);
            auto clause = make_shared<WithClauseAST>();
            clause->name = name->getText();
            clause->type = type->getText();
            parseStatementSeq(clause->stmts);
            with->clauses.push_back(clause);
            while (expect(Token::PIPE)) {
                accept(Token::PIPE);
                name = accept(Token::IDENTIFIER);
                accept(Token::COLON);
                type = accept(Token::IDENTIFIER);
                accept(Token::DO);
                clause = make_shared<WithClauseAST>();
                clause->name = name->getText();
                clause->type = type->getText();
                parseStatementSeq(clause->stmts);
                with->clauses.push_back(clause);
            }
            if (expect(Token::ELSE)) {
                accept(Token::ELSE);
                parseStatementSeq(with->elseStmts);
            }
            accept(Token::END);
            stmts.push_back(with);
            break;
        }
        case Token::EXIT: {
            accept(Token::EXIT);
            stmts.push_back(make_shared<ExitStatementAST>());
            break;
        }
        case Token::RETURN: {
            accept(Token::RETURN);
            auto _return = make_shared<ReturnStatementAST>();
            if (!expect({ Token::SEMICOLON, Token::END })) {
                _return->expr = parseExpr();
            }
            stmts.push_back(_return);
            break;
        }
        default:
            break;
        }
        if (!expect(Token::SEMICOLON)) {
            break;
        }
        accept(Token::SEMICOLON);
    }
}

void Parser::parseCase(vector<shared_ptr<CaseClauseAST>> &clauses) {
    shared_ptr<CaseClauseAST> clause = make_shared<CaseClauseAST>();
    pair<shared_ptr<ExprAST>, shared_ptr<ExprAST>> item;
    item.first = parseConstExpr();
    if (expect(Token::RANGE)) {
        accept(Token::RANGE);
        item.second = parseConstExpr();
    } else {
        item.second = nullptr;
    }
    clause->when.push_back(item);

    while (expect(Token::COMMA)) {
        accept(Token::COMMA);
        item.first = parseConstExpr();
        if (expect(Token::RANGE)) {
            accept(Token::RANGE);
            item.second = parseConstExpr();
        } else {
            item.second = nullptr;
        }
        clause->when.push_back(item);
    }
    accept(Token::COLON);
    parseStatementSeq(clause->stmts);

    clauses.push_back(clause);
}

void Parser::parseReceiver() {
    accept(Token::LPAREN);
    if (expect(Token::VAR)) {
        accept(Token::VAR);
    }
    accept(Token::IDENTIFIER);
    accept(Token::COLON);
    accept(Token::IDENTIFIER);
    accept(Token::RPAREN);
}

void Parser::parseFormalParams() {
    accept(Token::LPAREN);
    while (!expect(Token::RPAREN)) {
        if (expect(Token::VAR)) {
            accept(Token::VAR);
        }
        accept(Token::IDENTIFIER);
        while(expect(Token::COMMA)) {
            accept(Token::COMMA);
            accept(Token::IDENTIFIER);
        }
        accept(Token::COLON);
        parseType();
        if (expect(Token::SEMICOLON)) {
            accept(Token::SEMICOLON);
        }
    }
    accept(Token::RPAREN);
    if (expect(Token::COLON)) {
        accept(Token::COLON);
        accept(Token::IDENTIFIER);
    }
}

void Parser::parseType() {
    auto keyword = accept({ Token::IDENTIFIER, Token::ARRAY, Token::RECORD, Token::POINTER, Token::PROCEDURE });
    switch(keyword->getKind()) {
    case Token::IDENTIFIER:
        break;
    case Token::ARRAY:
        if (!expect(Token::OF)) {
            parseConstExpr();
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseConstExpr();
            }
        }
        accept(Token::OF);
        parseType();
        break;
    case Token::RECORD:
        if (expect(Token::LPAREN)) {
            accept(Token::LPAREN);
            accept(Token::IDENTIFIER);
            accept(Token::RPAREN);
        }
        while (1) {
            parseIdentDef();
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseIdentDef();
            }
            accept(Token::COLON);
            parseType();
            if (expect(Token::SEMICOLON)) {
                accept(Token::SEMICOLON);
            } else if (expect(Token::END)) {
                break;
            } else {
                auto tok = accept();
                throw ParserException("Expected ';' or 'END' after RECORD FieldList, got " + tok->getText(), tok->getLocation());
            }
        }
        accept(Token::END);
        break;
    case Token::POINTER:
        accept(Token::TO);
        parseType();
        break;
    case Token::PROCEDURE:
        parseFormalParams();
        break;
    default:
        break;
    }
}

shared_ptr<ExprAST> Parser::parseConstExpr() {
    return parseExpr();
}

shared_ptr<ExprAST> Parser::parseExpr() {
    shared_ptr<ExprAST> LHS = parseUnaryExpr();
    if (!LHS) {
        return nullptr;
    }
    shared_ptr<ExprAST> lexpr = parseBinOpRHS(LHS, 0);
    shared_ptr<ExprAST> rexpr = nullptr;
    if (expect(Token::RELATION)) {
        accept(Token::RELATION);

        LHS = parseUnaryExpr();
        if (!LHS) {
            return nullptr;
        }
        rexpr = parseBinOpRHS(LHS, 0);
    }
    return nullptr;
}

shared_ptr<ExprAST> Parser::parseUnaryExpr() {
    auto factor = parseFactor();
    if (factor) {
        return factor;
    }
    auto op = accept(Token::OPERATOR);
    auto operand = parseUnaryExpr();
    if (!op) {
        return nullptr;
    }
    return make_shared<UnExprAST>(op->getText(), operand);
}

shared_ptr<ExprAST> Parser::parseBinOpRHS(shared_ptr<ExprAST> LHS, int exprPrec) {
    while (1) {
        auto op = expect(Token::OPERATOR);
        int opPrec = getPrecedence(op);

        if (opPrec < exprPrec) {
            return LHS;
        }

        accept(Token::OPERATOR);

        shared_ptr<ExprAST> RHS = parseUnaryExpr();
        if (!RHS) {
            return nullptr;
        }

        auto nextOp = expect(Token::OPERATOR);
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
    auto tok = expect({ Token::IDENTIFIER, Token::BOOLLITERAL, Token::INTLITERAL, Token::FLOATLITERAL, Token::STRLITERAL, Token::CHARLITERAL, Token::NIL,
                        Token::LCURLY, Token::LPAREN, Token::TILDE });
    if (!tok) {
        return nullptr;
    }
    switch(tok->getKind()) {
    case Token::IDENTIFIER:
        // Variable/Constant or Procedure call
        parseDesignator();
        if (expect(Token::LPAREN)) {
            accept(Token::LPAREN);
            parseExpr();
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseExpr();
            }
            accept(Token::RPAREN);
        }
        break;
    case Token::BOOLLITERAL:
        tok = accept(Token::BOOLLITERAL);
        return make_shared<BoolLiteralAST>(tok->getBoolVal());
    case Token::INTLITERAL:
        tok = accept(Token::INTLITERAL);
        return make_shared<IntLiteralAST>(tok->getIntVal());
    case Token::FLOATLITERAL:
        tok = accept(Token::FLOATLITERAL);
        return make_shared<FloatLiteralAST>(tok->getFloatVal());
    case Token::STRLITERAL:
        tok = accept(Token::STRLITERAL);
        return make_shared<StrLiteralAST>(tok->getText());
    case Token::CHARLITERAL:
        tok = accept(Token::CHARLITERAL);
        return make_shared<CharLiteralAST>(tok->getCharVal());
    case Token::NIL:
        tok = accept(Token::NIL);
        return make_shared<NilLiteralAST>();
    case Token::LCURLY:
        // SET
        accept(Token::LCURLY);
        if (!expect(Token::RCURLY)) {
            parseExpr();
            if (expect(Token::RANGE)) {
                accept(Token::RANGE);
                parseExpr();
            }
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseExpr();
                if (expect(Token::RANGE)) {
                    accept(Token::RANGE);
                    parseExpr();
                }
            }
        }
        accept(Token::RCURLY);
        break;
  case Token::LPAREN:
        accept(Token::LPAREN);
        parseExpr();
        accept(Token::RPAREN);
        break;
    case Token::TILDE:
        // NOT
        accept(Token::TILDE);
        parseFactor();
        break;
    default:
        break;
    }
    return make_shared<FactorAST>();
}

shared_ptr<DesignatorAST> Parser::parseDesignator() {
    auto name = accept(Token::IDENTIFIER);
    auto des = make_shared<DesignatorAST>(name->getText());
    if (expect(Token::DOT)) {
        accept(Token::DOT);
        accept(Token::IDENTIFIER);
    }
    while (expect({ Token::DOT, Token::LSQUARE, Token::CARET /*, Token::LPAREN*/ })) {
        auto tok = accept({ Token::DOT, Token::LSQUARE, Token::CARET/*, Token::LPAREN*/ });
        switch(tok->getKind()) {
        case Token::DOT:
            // QUALIFIER
            accept(Token::IDENTIFIER);
            break;
        case Token::LSQUARE:
            // ARRAY INDEX
            parseExpr();
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseExpr();
            }
            accept(Token::RSQUARE);
            break;
        case Token::CARET:
            // POINTER DEREF
            break;
/*      case Token::LPAREN:
            accept(Token::IDENTIFIER);
            if (expect(Token::DOT)) {
                accept(Token::DOT);
                accept(Token::IDENTIFIER);
            }
            accept(Token::RPAREN);
            break;*/
        default:
            break;
        }
    }
    return des;
}

shared_ptr<IdentDefAST> Parser::parseIdentDef() {
    auto ident = accept(Token::IDENTIFIER);
    auto next  = expect(Token::OPERATOR);
    Export exprt = Export::NO;
    if (next) {
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

int Parser::getPrecedence(shared_ptr<Token> op) {
    if (op) {
        int prec = precedence[op->getText()];
        if (prec > 0) {
            return prec;
        }
    }
    return -1;
}
