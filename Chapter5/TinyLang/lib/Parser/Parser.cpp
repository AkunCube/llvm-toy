#include "tinylang/Parser/Parser.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

using namespace tinylang;

static bool isFirstOfExpression(const Token &token) {
  return token.isOneof(tok::TokenKind::plus, tok::TokenKind::minus,
                       tok::TokenKind::l_paren, tok::TokenKind::KW_NOT,
                       tok::TokenKind::identifier,
                       tok::TokenKind::integer_literal);
}

static bool isRelational(const Token &token) {
  return token.isOneof(tok::TokenKind::equal, tok::TokenKind::hash,
                       tok::TokenKind::less, tok::TokenKind::lessequal,
                       tok::TokenKind::greater, tok::TokenKind::greaterequal);
}

static bool isAddOperator(const Token &token) {
  return token.isOneof(tok::TokenKind::plus, tok::TokenKind::minus,
                       tok::TokenKind::KW_OR);
}

static bool isMulOperator(const Token &token) {
  return token.isOneof(tok::TokenKind::star, tok::TokenKind::slash,
                       tok::TokenKind::KW_DIV, tok::TokenKind::KW_MOD,
                       tok::TokenKind::KW_AND);
}

static bool isFollowFactor(const Token &token) {
  return isRelational(token) || isAddOperator(token) || isMulOperator(token) ||
         token.isOneof(tok::TokenKind::comma, tok::TokenKind::r_paren,
                       tok::TokenKind::KW_DO, tok::TokenKind::KW_THEN,
                       tok::TokenKind::KW_ELSE, tok::TokenKind::KW_END,
                       tok::TokenKind::semi);
}

static OperatorInfo fromTok(Token tok) {
  return OperatorInfo(tok.getLocation(), tok.getKind());
}

Parser::Parser(Lexer &lexer, Sema &actions) : lexer(lexer), actions(actions) {
  advance();
}

ModuleDeclaration *Parser::parse() {
  ModuleDeclaration *module = nullptr;
  bool parseResult = parseCompilationUnit(module);
  llvm::errs() << "Parsing " << (parseResult ? "failed" : "succeeded") << "\n";
  return module;
}

bool Parser::parseCompilationUnit(ModuleDeclaration *&decl) {
  auto errorhandler = [&] { return skipUntil(); };

  if (consume(tok::KW_MODULE)) {
    return errorhandler();
  }
  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }

  decl = actions.actOnModuleDeclaration(token.getLocation(),
                                        token.getIdentifier());
  EnterDeclScope s(actions, decl);
  advance();
  if (consume(tok::TokenKind::semi)) {
    return errorhandler();
  }
  while (token.isOneof(tok::TokenKind::KW_FROM, tok::TokenKind::KW_IMPORT)) {
    if (parseImport()) {
      return errorhandler();
    }
  }

  DeclList decls;
  StmtList stmts;
  if (parseBlock(decls, stmts)) {
    return errorhandler();
  }
  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }
  actions.actOnModuleDeclaration(decl, token.getLocation(),
                                 token.getIdentifier(), decls, stmts);
  advance();
  if (consume(tok::TokenKind::period)) {
    return errorhandler();
  }
  return false;
}

bool Parser::parseImport() {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::KW_BEGIN, tok::TokenKind::KW_CONST,
                     tok::TokenKind::KW_END, tok::TokenKind::KW_FROM,
                     tok::TokenKind::KW_IMPORT, tok::TokenKind::KW_PROCEDURE,
                     tok::TokenKind::KW_VAR);
  };
  IdentList idents;
  StringRef moduleName;
  if (token.is(tok::TokenKind::KW_FROM)) {
    advance();
    if (expect(tok::TokenKind::identifier)) {
      return errorhandler();
    }
    moduleName = token.getIdentifier();
    advance();
  }

  if (consume(tok::TokenKind::KW_IMPORT)) {
    return errorhandler();
  }
  if (parseIdentList(idents)) {
    return errorhandler();
  }
  if (expect(tok::TokenKind::semi)) {
    return errorhandler();
  }
  actions.actOnImport(moduleName, idents);
  advance();
  return false;
}

bool Parser::parseBlock(DeclList &decls, StmtList &stmts) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::identifier); };

  while (token.isOneof(tok::TokenKind::KW_CONST, tok::TokenKind::KW_VAR,
                       tok::TokenKind::KW_PROCEDURE)) {
    if (parseDeclaration(decls)) {
      return errorhandler();
    }
  }
  if (token.is(tok::TokenKind::KW_BEGIN)) {
    advance();
    if (parseStatementSequence(stmts)) {
      return errorhandler();
    }
  }
  if (consume(tok::TokenKind::KW_END)) {
    return errorhandler();
  }
  return false;
}

bool Parser::parseDeclaration(DeclList &decls) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::KW_BEGIN, tok::TokenKind::KW_CONST,
                     tok::TokenKind::KW_END, tok::TokenKind::KW_PROCEDURE,
                     tok::TokenKind::KW_VAR);
  };

  if (token.is(tok::TokenKind::KW_CONST)) {
    advance();
    while (token.is(tok::TokenKind::identifier)) {
      if (parseConstantDeclaration(decls)) {
        return errorhandler();
      }
      if (consume(tok::TokenKind::semi)) {
        return errorhandler();
      }
    }
  } else if (token.is(tok::TokenKind::KW_VAR)) {
    advance();
    while (token.is(tok::TokenKind::identifier)) {
      if (parseVariableDeclaration(decls)) {
        return errorhandler();
      }
      if (consume(tok::TokenKind::semi)) {
        return errorhandler();
      }
    }
  } else if (token.is(tok::TokenKind::KW_PROCEDURE)) {
    if (parseProcedureDeclaration(decls)) {
      return errorhandler();
    }
    if (consume(tok::TokenKind::semi)) {
      return errorhandler();
    }
  } else if (token.is(tok::TokenKind::KW_TYPE)) {
    advance();
    while (token.is(tok::TokenKind::identifier)) {
      if (parseTypeDeclaration(decls)) {
        return errorhandler();
      }
      if (consume(tok::TokenKind::semi)) {
        return errorhandler();
      }
    }
  } else {
    // ERROR
    return errorhandler();
  }

  return false;
}

bool Parser::parseConstantDeclaration(DeclList &decls) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::semi); };

  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }

  SMLoc loc = token.getLocation();
  StringRef name = token.getIdentifier();
  advance();
  if (consume(tok::TokenKind::equal)) {
    return errorhandler();
  }
  Expr *e = nullptr;
  if (parseExpression(e)) {
    return errorhandler();
  }
  actions.actOnConstantDeclaration(decls, loc, name, e);
  return false;
}

bool Parser::parseVariableDeclaration(DeclList &decls) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::semi); };

  Decl *decl = nullptr;
  IdentList ids;
  if (parseIdentList(ids)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::colon)) {
    return errorhandler();
  }
  if (parseQualident(decl)) {
    errorhandler();
  }
  actions.actOnVariableDeclaration(decls, ids, decl);
  return false;
}

bool Parser::parseProcedureDeclaration(DeclList &parentDecls) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::semi); };

  if (consume(tok::TokenKind::KW_PROCEDURE)) {
    return errorhandler();
  }
  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }

  ProcedureDeclaration *procDecl = actions.actOnProcedureDeclaration(
      token.getLocation(), token.getIdentifier());

  EnterDeclScope s(actions, procDecl);
  FormalParamList params;

  Decl *retType = nullptr;
  advance();
  if (token.is(tok::TokenKind::l_paren)) {
    if (parseFormalParameters(params, retType)) {
      return errorhandler();
    }
  }
  actions.actOnProcedureHeading(procDecl, params, retType);
  if (consume(tok::TokenKind::semi)) {
    return errorhandler();
  }
  DeclList decls;
  StmtList stmts;
  if (parseBlock(decls, stmts)) {
    return errorhandler();
  }
  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }
  actions.actOnProcedureDeclaration(procDecl, token.getLocation(),
                                    token.getIdentifier(), decls, stmts);
  parentDecls.push_back(procDecl);
  advance();
  return false;
}

bool Parser::parseFormalParameters(FormalParamList &params, Decl *&retType) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::semi); };

  if (consume(tok::TokenKind::l_paren)) {
    return errorhandler();
  }
  if (token.isOneof(tok::TokenKind::KW_VAR, tok::TokenKind::identifier)) {
    if (parseFormalParameterList(params)) {
      return errorhandler();
    }
  }
  if (consume(tok::TokenKind::r_paren)) {
    return errorhandler();
  }
  if (token.is(tok::TokenKind::colon)) {
    advance();
    if (parseQualident(retType)) {
      return errorhandler();
    }
  }
  return false;
}

bool Parser::parseFormalParameterList(FormalParamList &params) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::r_paren); };

  if (parseFormalParameter(params)) {
    return errorhandler();
  }
  while (token.is(tok::TokenKind::semi)) {
    advance();
    if (parseFormalParameter(params)) {
      return errorhandler();
    }
  }
  return false;
}

bool Parser::parseFormalParameter(FormalParamList &params) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::r_paren, tok::TokenKind::semi);
  };

  IdentList ids;
  Decl *decl = nullptr;
  bool isVar = false;
  if (token.is(tok::TokenKind::KW_VAR)) {
    isVar = true;
    advance();
  }
  if (parseIdentList(ids)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::colon)) {
    return errorhandler();
  }
  if (parseQualident(decl)) {
    return errorhandler();
  }
  actions.actOnFormalParameterDeclaration(params, ids, decl, isVar);
  return false;
}

bool Parser::parseStatementSequence(StmtList &stmts) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::KW_ELSE, tok::TokenKind::KW_END);
  };

  if (parseStatement(stmts)) {
    return errorhandler();
  }
  while (token.is(tok::TokenKind::semi)) {
    advance();
    if (parseStatement(stmts)) {
      return errorhandler();
    }
  }
  return false;
}

bool Parser::parseStatement(StmtList &stmts) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::semi, tok::TokenKind::KW_ELSE,
                     tok::TokenKind::KW_END);
  };

  if (token.is(tok::TokenKind::identifier)) {
    Decl *decl = nullptr;
    Expr *e = nullptr;
    SMLoc loc = token.getLocation();
    if (parseQualident(decl)) {
      return errorhandler();
    }
    if (token.is(tok::TokenKind::colonequal)) {
      advance();
      if (parseExpression(e)) {
        return errorhandler();
      }
      actions.actOnAssignment(stmts, loc, decl, e);
    } else if (token.is(tok::TokenKind::l_paren)) {
      ExprList exprs;
      advance();
      if (isFirstOfExpression(token)) {
        if (parseExpList(exprs)) {
          return errorhandler();
        }
      }
      if (consume(tok::TokenKind::r_paren)) {
        return errorhandler();
      }
      actions.actOnProcCall(stmts, loc, decl, exprs);
    }
  } else if (token.is(tok::TokenKind::KW_IF)) {
    if (parseIfStatement(stmts)) {
      return errorhandler();
    }
  } else if (token.is(tok::TokenKind::KW_WHILE)) {
    if (parseWhileStatement(stmts)) {
      return errorhandler();
    }
  } else if (token.is(tok::TokenKind::KW_RETURN)) {
    if (parseReturnStatement(stmts)) {
      return errorhandler();
    }
  } else {
    // ERROR
    return errorhandler();
  }
  return false;
}

bool Parser::parseIfStatement(StmtList &stmts) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::semi, tok::TokenKind::KW_ELSE,
                     tok::TokenKind::KW_END);
  };

  Expr *cond = nullptr;

  StmtList ifStmts, elseStmts;
  SMLoc loc = token.getLocation();
  if (consume(tok::TokenKind::KW_IF)) {
    return errorhandler();
  }
  if (parseExpression(cond)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::KW_THEN)) {
    return errorhandler();
  }
  if (parseStatementSequence(ifStmts)) {
    return errorhandler();
  }
  if (token.is(tok::TokenKind::KW_ELSE)) {
    advance();
    if (parseStatementSequence(elseStmts)) {
      return errorhandler();
    }
  }
  if (consume(tok::TokenKind::KW_END)) {
    return errorhandler();
  }
  actions.actOnIfStatement(stmts, loc, cond, ifStmts, elseStmts);
  return false;
}

bool Parser::parseWhileStatement(StmtList &stmts) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::semi, tok::TokenKind::KW_ELSE,
                     tok::TokenKind::KW_END);
  };

  Expr *cond = nullptr;
  StmtList WhileStmts;
  SMLoc Loc = token.getLocation();
  if (consume(tok::TokenKind::KW_WHILE)) {
    return errorhandler();
  }
  if (parseExpression(cond)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::KW_DO)) {
    return errorhandler();
  }
  if (parseStatementSequence(WhileStmts)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::KW_END)) {
    return errorhandler();
  }
  actions.actOnWhileStatement(stmts, Loc, cond, WhileStmts);
  return false;
}

bool Parser::parseReturnStatement(StmtList &Stmts) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::semi, tok::TokenKind::KW_ELSE,
                     tok::TokenKind::KW_END);
  };

  Expr *e = nullptr;
  SMLoc loc = token.getLocation();
  if (consume(tok::TokenKind::KW_RETURN)) {
    return errorhandler();
  }
  if (isFirstOfExpression(token)) {
    if (parseExpression(e)) {
      return errorhandler();
    }
  }
  actions.actOnReturnStatement(Stmts, loc, e);
  return false;
}

bool Parser::parseExpList(ExprList &exprs) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::r_paren); };

  Expr *e = nullptr;
  if (parseExpression(e)) {
    return errorhandler();
  }
  if (e) {
    exprs.push_back(e);
  }

  while (token.is(tok::TokenKind::comma)) {
    advance();
    e = nullptr;
    if (parseExpression(e)) {
      return errorhandler();
    }
    if (e) {
      exprs.push_back(e);
    }
  }
  return false;
}

bool Parser::parseExpression(Expr *&e) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::r_paren, tok::TokenKind::comma,
                     tok::TokenKind::semi, tok::TokenKind::KW_DO,
                     tok::TokenKind::KW_ELSE, tok::TokenKind::KW_END,
                     tok::TokenKind::KW_THEN);
  };
  if (parseSimpleExpression(e)) {
    return errorhandler();
  }
  if (isRelational(token)) {
    OperatorInfo op = fromTok(token);
    Expr *right = nullptr;
    if (parseRelation(op)) {
      return errorhandler();
    }
    if (parseSimpleExpression(right)) {
      return errorhandler();
    }
    e = actions.actOnExpression(e, right, op);
  }
  return false;
}

bool Parser::parseRelation(OperatorInfo &op) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::l_paren, tok::TokenKind::plus,
                     tok::TokenKind::minus, tok::TokenKind::KW_NOT,
                     tok::TokenKind::identifier,
                     tok::TokenKind::integer_literal);
  };

  if (isRelational(token)) {
    op = fromTok(token);
    advance();
    return false;
  }
  return errorhandler();
}

bool Parser::parseSimpleExpression(Expr *&e) {
  auto errorhandler = [&] {
    return skipUntil(
        tok::TokenKind::hash, tok::TokenKind::r_paren, tok::TokenKind::comma,
        tok::TokenKind::semi, tok::TokenKind::less, tok::TokenKind::lessequal,
        tok::TokenKind::equal, tok::TokenKind::greater,
        tok::TokenKind::greaterequal, tok::TokenKind::KW_DO,
        tok::TokenKind::KW_ELSE, tok::KW_END, tok::TokenKind::KW_THEN);
  };

  OperatorInfo prefixOp;
  if (token.isOneof(tok::TokenKind::plus, tok::TokenKind::minus)) {
    prefixOp = fromTok(token);
    advance();
  }
  if (parseTerm(e)) {
    return errorhandler();
  }
  while (isAddOperator(token)) {
    OperatorInfo op;
    Expr *right = nullptr;
    if (parseAddOperator(op)) {
      return errorhandler();
    }
    if (parseTerm(right)) {
      return errorhandler();
    }
    e = actions.actOnSimpleExpression(e, right, op);
  }

  if (!prefixOp.isUnspecified()) {
    e = actions.actOnPrefixExpression(e, prefixOp);
  }
  return false;
}

bool Parser::parseAddOperator(OperatorInfo &op) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::l_paren, tok::TokenKind::KW_NOT,
                     tok::TokenKind::identifier,
                     tok::TokenKind::integer_literal);
  };

  if (isAddOperator(token)) {
    op = fromTok(token);
    advance();
    return false;
  }

  return errorhandler();
}

bool Parser::parseTerm(Expr *&e) {
  auto errorhandler = [&] {
    return skipUntil(
        tok::TokenKind::hash, tok::TokenKind::r_paren, tok::TokenKind::plus,
        tok::TokenKind::comma, tok::TokenKind::minus, tok::TokenKind::semi,
        tok::TokenKind::less, tok::TokenKind::lessequal, tok::TokenKind::equal,
        tok::TokenKind::greater, tok::TokenKind::greaterequal,
        tok::TokenKind::KW_DO, tok::TokenKind::KW_ELSE, tok::KW_END,
        tok::TokenKind::KW_OR, tok::TokenKind::KW_THEN);
  };

  if (parseFactor(e)) {
    return errorhandler();
  }

  while (isMulOperator(token)) {
    OperatorInfo op;
    Expr *right = nullptr;
    if (parseMulOperator(op)) {
      return errorhandler();
    }
    if (parseFactor(right)) {
      return errorhandler();
    }
    e = actions.actOnTerm(e, right, op);
  }
  return false;
}

bool Parser::parseMulOperator(OperatorInfo &op) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::l_paren, tok::TokenKind::KW_NOT,
                     tok::TokenKind::identifier,
                     tok::TokenKind::integer_literal);
  };

  if (isMulOperator(token)) {
    op = fromTok(token);
    advance();
    return false;
  }

  return errorhandler();
}

bool Parser::parseFactor(Expr *&e) {
  auto errorhandler = [&] {
    return skipUntil(
        tok::TokenKind::hash, tok::TokenKind::r_paren, tok::TokenKind::star,
        tok::TokenKind::plus, tok::TokenKind::comma, tok::TokenKind::minus,
        tok::TokenKind::slash, tok::TokenKind::semi, tok::TokenKind::less,
        tok::TokenKind::lessequal, tok::TokenKind::equal,
        tok::TokenKind::greater, tok::TokenKind::greaterequal,
        tok::TokenKind::KW_AND, tok::TokenKind::KW_DIV, tok::TokenKind::KW_DO,
        tok::TokenKind::KW_ELSE, tok::KW_END, tok::TokenKind::KW_MOD,
        tok::TokenKind::KW_OR, tok::TokenKind::KW_THEN);
  };

  if (token.is(tok::TokenKind::integer_literal)) {
    e = actions.actOnIntegerLiteral(token.getLocation(),
                                    token.getLiteralData());
    advance();
    return false;
  }

  if (token.is(tok::TokenKind::l_paren)) {
    advance();
    if (parseExpression(e)) {
      return errorhandler();
    }
    if (consume(tok::TokenKind::r_paren)) {
      return errorhandler();
    }
    return false;
  }

  if (token.is(tok::TokenKind::identifier)) {
    Decl *decl = nullptr;
    ExprList exprs;
    if (parseQualident(decl)) {
      return errorhandler();
    }
    if (token.is(tok::TokenKind::l_paren)) {
      advance();
      if (isFirstOfExpression(token)) {
        if (parseExpList(exprs)) {
          return errorhandler();
        }
      }
      if (consume(tok::TokenKind::r_paren)) {
        return errorhandler();
      }
      e = actions.actOnFunctionCall(decl, exprs);
      return false;
    }

    if (isFollowFactor(token)) {
      e = actions.actOnVariable(decl);
      return false;
    }
    return errorhandler();
  }

  if (token.is(tok::TokenKind::KW_NOT)) {
    OperatorInfo op = fromTok(token);
    advance();
    if (parseFactor(e)) {
      return errorhandler();
    }
    e = actions.actOnPrefixExpression(e, op);
    return false;
  }

  return errorhandler();
}

bool Parser::parseQualident(Decl *&d) {
  auto errorhandler = [&] {
    return skipUntil(
        tok::TokenKind::hash, tok::TokenKind::r_paren, tok::TokenKind::star,
        tok::TokenKind::plus, tok::TokenKind::comma, tok::TokenKind::minus,
        tok::TokenKind::slash, tok::TokenKind::semi, tok::TokenKind::less,
        tok::TokenKind::lessequal, tok::TokenKind::equal,
        tok::TokenKind::greater, tok::TokenKind::greaterequal,
        tok::TokenKind::KW_AND, tok::TokenKind::KW_DIV, tok::TokenKind::KW_DO,
        tok::TokenKind::KW_ELSE, tok::KW_END, tok::TokenKind::KW_MOD,
        tok::TokenKind::KW_OR, tok::TokenKind::KW_THEN);
  };

  d = nullptr;
  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }

  d = actions.actOnQualIdentPart(d, token.getLocation(), token.getIdentifier());
  advance();
  while (token.is(tok::TokenKind::period) && llvm::isa<ModuleDeclaration>(d)) {
    advance();
    if (expect(tok::TokenKind::identifier)) {
      return errorhandler();
    }
    d = actions.actOnQualIdentPart(d, token.getLocation(),
                                   token.getIdentifier());
    advance();
  }

  return false;
}

bool Parser::parseIdentList(IdentList &ids) {
  auto errorhandler = [&] {
    return skipUntil(tok::TokenKind::colon, tok::TokenKind::semi);
  };

  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }
  ids.push_back(std::make_pair(token.getLocation(), token.getIdentifier()));
  advance();
  while (token.is(tok::TokenKind::comma)) {
    advance();
    if (expect(tok::TokenKind::identifier)) {
      return errorhandler();
    }
    ids.push_back(std::make_pair(token.getLocation(), token.getIdentifier()));
    advance();
  }

  return false;
}

bool Parser::parseTypeDeclaration(DeclList &decls) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::semi); };

  if (expect(tok::TokenKind::identifier)) {
    return errorhandler();
  }

  SMLoc loc = token.getLocation();
  StringRef name = token.getIdentifier();

  if (consume(tok::TokenKind::equal)) {
    return errorhandler();
  }

  if (token.is(tok::TokenKind::KW_RECORD)) {
    FieldList fields;
    advance();
    if (parseFieldList(fields)) {
      return errorhandler();
    }
    if (consume(tok::TokenKind::KW_END)) {
      return errorhandler();
    }
    actions.actOnRecordTypeDeclaration(decls, loc, name, fields);
    return false;
  }

  if (token.is(tok::TokenKind::KW_POINTER)) {
    advance();
    if (consume(tok::TokenKind::KW_TO)) {
      return errorhandler();
    }
    Decl *baseType = nullptr;
    if (parseQualident(baseType)) {
      return errorhandler();
    }
    actions.actOnPointerTypeDeclaration(decls, loc, name, baseType);
    return false;
  }

  if (token.is(tok::TokenKind::KW_ARRAY)) {
    // TYPE arrayType = ARRAY '[' expr ]' OF <type>;
    advance();
    if (consume(tok::TokenKind::l_square)) {
      return errorhandler();
    }
    Expr *expr = nullptr;
    if (parseExpression(expr)) {
      return errorhandler();
    }
    if (consume(tok::TokenKind::r_square)) {
      return errorhandler();
    }
    if (consume(tok::TokenKind::KW_OF)) {
      return errorhandler();
    }
    Decl *baseType = nullptr;
    if (parseQualident(baseType)) {
      return errorhandler();
    }
    actions.actOnArrayTypeDeclaration(decls, loc, name, expr, baseType);
    return false;
  }

  if (token.is(tok::TokenKind::identifier)) {
    // Alias type: TYPE aliasType = <type>;
    Decl *baseType = nullptr;
    if (parseQualident(baseType)) {
      return errorhandler();
    }
    actions.actOnAliasTypeDeclaration(decls, loc, name, baseType);
    return false;
  }
  return errorhandler();
}

bool Parser::parseFieldList(FieldList &fields) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::KW_END); };
  if (parseField(fields)) {
    return errorhandler();
  }
  while (token.is(tok::TokenKind::semi)) {
    advance();
    if (parseField(fields)) {
      return errorhandler();
    }
  }

  return false;
}

bool Parser::parseField(FieldList &fields) {
  auto errorhandler = [&] { return skipUntil(tok::TokenKind::KW_END); };

  IdentList ids;
  Decl *typeDecl = nullptr;
  if (parseIdentList(ids)) {
    return errorhandler();
  }
  if (consume(tok::TokenKind::colon)) {
    return errorhandler();
  }
  if (parseQualident(typeDecl)) {
    return errorhandler();
  }
  actions.actOnFieldDeclaration(fields, ids, typeDecl);
  return false;
}
