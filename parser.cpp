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
    accept(Token::IDENTIFIER);
    accept(Token::SEMICOLON);

    if (expect(Token::IMPORT)) {
        parseImport();
    }

    while (auto keyword = expect({ Token::TYPE, Token::CONST, Token::VAR })) {
        switch (keyword->getKind()) {
        case Token::TYPE:
            parseTypeDecl();
            break;
        case Token::CONST:
            parseConstDecl();
            break;
        case Token::VAR:
            parseVarDecl();
            break;
        }
    }

    while (auto keyword = expect({ Token::PROCEDURE, Token::EXTERN })) {
        switch(keyword->getKind()) {
        case Token::PROCEDURE:
            parseProcDecl();
            break;
        case Token::EXTERN:
            parseExternDecl();
            break;
        }
    }

    if (expect(Token::BEGIN)) {
        accept(Token::BEGIN);
        parseStatementSeq();
    }

    accept(Token::END);
    accept(Token::IDENTIFIER);
    accept(Token::DOT);

    return nullptr;
}

void Parser::parseImport() {
    accept(Token::IMPORT);
    do {
        accept(Token::IDENTIFIER);
        if (expect(Token::ASSIGNMENT)) {
            accept(Token::ASSIGNMENT);
            accept(Token::IDENTIFIER);
        }
        if (expect(Token::COMMA)) {
            accept(Token::COMMA);
        }
    } while (!expect(Token::SEMICOLON));
    accept(Token::SEMICOLON);
}

void Parser::parseTypeDecl() {
    accept(Token::TYPE);
    do {
        accept(Token::IDENTIFIER);
        shared_ptr<Token> tok = accept(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '=', got " + tok->getText(), tok->getLocation());
        }
        parseType();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseConstDecl() {
    accept(Token::CONST);
    do {
        accept(Token::IDENTIFIER);
        shared_ptr<Token> tok = accept(Token::RELATION);
        if (tok->getText() != "=") {
            throw ParserException("Expected '='. got " + tok->getText(), tok->getLocation());
        }
        parseConstExpr();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseVarDecl() {
    accept(Token::VAR);
    do {
        accept(Token::IDENTIFIER);
        while (expect(Token::COMMA)) {
            accept(Token::COMMA);
            accept(Token::IDENTIFIER);
        }
        accept(Token::COLON);
        parseType();
        accept(Token::SEMICOLON);
    } while (expect(Token::IDENTIFIER));
}

void Parser::parseExternDecl() {
    accept(Token::EXTERN);
    accept(Token::IDENTIFIER);
    parseFormalParams();
}

void Parser::parseForwardDecl() {
    accept(Token::CARET);
    if (expect(Token::LPAREN)) {
        parseReceiver();
    }
    accept(Token::IDENTIFIER);
    parseFormalParams();
}

void Parser::parseProcDecl() {
    accept(Token::PROCEDURE);
    if (currentToken->getKind() == Token::CARET) {
        parseForwardDecl();
    } else {
        if (expect(Token::LPAREN)) {
            parseReceiver();
        }
        accept(Token::IDENTIFIER);
        parseFormalParams();
        accept(Token::SEMICOLON);

        while (auto keyword = expect({ Token::TYPE, Token::CONST, Token::VAR })) {
            switch (keyword->getKind()) {
                case Token::TYPE:
                    parseTypeDecl();
                    break;
                case Token::CONST:
                    parseConstDecl();
                    break;
                case Token::VAR:
                    parseVarDecl();
                    break;
            }
        }

        while (expect(Token::PROCEDURE)) {
            parseProcDecl();
        }
        
        if (expect(Token::BEGIN)) {
            accept(Token::BEGIN);
            parseStatementSeq();
        }

        accept(Token::END);
        accept(Token::IDENTIFIER);
        accept(Token::SEMICOLON);
    }
}

void Parser::parseStatementSeq() {
    while (1) {
        auto keyword = expect({ Token::IDENTIFIER, Token::IF, Token::CASE, Token::WHILE, Token::REPEAT, Token::FOR, Token::LOOP, Token::WITH, Token::EXIT, Token::RETURN });
        if (!keyword) {
            keyword = accept();
            throw ParserException("Expected statement, got " + kind_to_string(keyword->getKind()), keyword->getLocation());
        }
        switch(keyword->getKind()) {
        case Token::IDENTIFIER:
            parseDesignator();
            if (expect(Token::ASSIGNMENT)) {
                // Assignment
                accept(Token::ASSIGNMENT);
                parseExpr();
            } else if (expect(Token::LPAREN)) {
                // Call with parameters
                accept(Token::LPAREN);
                if (!expect(Token::RPAREN)) {
                    parseExpr();
                    while (expect(Token::COMMA)) {
                        accept(Token::COMMA);
                        parseExpr();
                    }
                }
                accept(Token::RPAREN);
            } else {
                // Naked call
            }
            break;
        case Token::IF:
            accept(Token::IF);
            parseExpr();
            accept(Token::THEN);
            parseStatementSeq();
            while (expect(Token::ELSIF)) {
                accept(Token::ELSIF);
                parseExpr();
                accept(Token::THEN);
                parseStatementSeq();
            }
            if (expect(Token::ELSE)) {
                accept(Token::ELSE);
                parseStatementSeq();
            }
            accept(Token::END);
            break;
        case Token::CASE:
            accept(Token::CASE);
            parseExpr();
            accept(Token::OF);
            parseCase();
            while (expect(Token::PIPE)) {
                accept(Token::PIPE);
                parseCase();
            }
            if (expect(Token::ELSE)) {
                accept(Token::CASE);
                parseStatementSeq();
            }
            accept(Token::END);
            break;
        case Token::WHILE:
            accept(Token::WHILE);
            parseExpr();
            accept(Token::DO);
            parseStatementSeq();
            accept(Token::END);
            break;
        case Token::REPEAT:
            accept(Token::REPEAT);
            parseStatementSeq();
            accept(Token::UNTIL);
            parseExpr();
            break;
        case Token::FOR:
            accept(Token::FOR);
            accept(Token::IDENTIFIER);
            accept(Token::ASSIGNMENT);
            parseExpr();
            accept(Token::TO);
            parseExpr();
            if (expect(Token::BY)) {
                accept(Token::BY);
                parseConstExpr();
            }
            accept(Token::DO);
            parseStatementSeq();
            accept(Token::END);
            break;
        case Token::LOOP:
            accept(Token::LOOP);
            parseStatementSeq();
            accept(Token::END);
            break;
        case Token::WITH:
            accept(Token::WITH);
            accept(Token::IDENTIFIER);
            accept(Token::COLON);
            accept(Token::IDENTIFIER);
            accept(Token::DO);
            parseStatementSeq();
            while (expect(Token::PIPE)) {
                accept(Token::PIPE);
                accept(Token::IDENTIFIER);
                accept(Token::COLON);
                accept(Token::IDENTIFIER);
                accept(Token::DO);
                parseStatementSeq();
            }
            if (expect(Token::ELSE)) {
                accept(Token::ELSE);
                parseStatementSeq();
            }
            accept(Token::END);
            break;
        case Token::EXIT:
            accept(Token::EXIT);
            break;
        case Token::RETURN:
            accept(Token::RETURN);
            if (!expect({ Token::SEMICOLON, Token::END })) {
                parseExpr();
            }
            break;
        }
        if (!expect(Token::SEMICOLON)) {
            break;
        }
        accept(Token::SEMICOLON);
    }
}

void Parser::parseCase() {
    parseConstExpr();
    if (expect(Token::RANGE)) {
        accept(Token::RANGE);
        parseConstExpr();
    }
    while (expect(Token::COMMA)) {
        accept(Token::COMMA);
        parseConstExpr();
        if (expect(Token::RANGE)) {
            accept(Token::RANGE);
            parseConstExpr();
        }
    }
    accept(Token::COLON);
    parseStatementSeq();
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
        while (!expect(Token::END)) {
            accept(Token::IDENTIFIER);
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                accept(Token::IDENTIFIER);
            }
            accept(Token::COLON);
            parseType();
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

shared_ptr<FactorAST> Parser::parseFactor() {
    auto tok = expect({ Token::IDENTIFIER, Token::INTLITERAL, Token::FLOATLITERAL, Token::STRLITERAL, Token::CHARLITERAL, Token::NIL, Token::LCURLY, Token::LPAREN, Token::TILDE });
    if (!tok) {
        return nullptr;
    }
    switch(tok->getKind()) {
    case Token::IDENTIFIER:
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
    case Token::INTLITERAL:
    case Token::FLOATLITERAL:
    case Token::STRLITERAL:
    case Token::CHARLITERAL:
    case Token::NIL:
        accept();
        break;
    case Token::LCURLY:
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
        accept(Token::TILDE);
        parseFactor();
        break;
    }
    return make_shared<FactorAST>();
}

void Parser::parseDesignator() {
    accept(Token::IDENTIFIER);
    if (expect(Token::DOT)) {
        accept(Token::DOT);
        accept(Token::IDENTIFIER);
    }
    while (expect({ Token::DOT, Token::LSQUARE, Token::CARET /*, Token::LPAREN*/ })) {
        auto tok = accept({ Token::DOT, Token::LSQUARE, Token::CARET, Token::LPAREN });
        switch(tok->getKind()) {
        case Token::DOT:
            accept(Token::IDENTIFIER);
            break;
        case Token::LSQUARE:
            parseExpr();
            while (expect(Token::COMMA)) {
                accept(Token::COMMA);
                parseExpr();
            }
            accept(Token::RSQUARE);
            break;
        case Token::CARET:
            break;
        case Token::LPAREN:
            accept(Token::IDENTIFIER);
            if (expect(Token::DOT)) {
                accept(Token::DOT);
                accept(Token::IDENTIFIER);
            }
            accept(Token::RPAREN);
            break;
        }
    }
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
