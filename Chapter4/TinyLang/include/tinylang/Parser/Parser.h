#ifndef TINYLANG_PARSER_PARSER_H
#define TINYLANG_PARSER_PARSER_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Lexer/Token.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"

namespace tinylang {

class Parser {

public:
  Parser(Lexer &lexer, Sema &actions);
  ModuleDeclaration *parse();

private:
  Lexer &lexer;
  Token token;
  Sema &actions;

  DiagnosticsEngine &getDiagEngine() const { return lexer.getDiagEngine(); }

  void advance() { lexer.next(token); }

  bool expect(tok::TokenKind expectedKind) {
    if (token.is(expectedKind)) {
      return false; // No error.
    }

    const char *expectedName = tok::getPunctuatorSpelling(expectedKind);
    if (!expectedName) {
      expectedName = tok::getKeywordSpelling(expectedKind);
    }

    llvm::StringRef actualName(token.getLocation().getPointer(),
                               token.getLength());
    getDiagEngine().report(token.getLocation(), diag::err_expected,
                           expectedName, actualName);
    return true;
  }

  bool consume(tok::TokenKind expectedKind) {
    if (token.is(expectedKind)) {
      advance();
      return false; // No error.
    }
    return true;
  }

  template <typename... Tokens> bool skipUntil(Tokens &&...Toks) {
    while (true) {
      if ((... || token.is(Toks))) {
        return false;
      }

      if (token.is(tok::eof)) {
        return true;
      }
      advance();
    }
  }

  [[nodiscard]] bool parseCompilationUnit(ModuleDeclaration *&decl);
  bool parseImport();
  bool parseBlock(DeclList &decls, StmtList &stmts);
  bool parseDeclaration(DeclList &decls);
  bool parseConstantDeclaration(DeclList &decls);
  bool parseVariableDeclaration(DeclList &decls);
  bool parseProcedureDeclaration(DeclList &parentDecls);
  bool parseFormalParameters(FormalParamList &params, Decl *&retType);
  bool parseFormalParameterList(FormalParamList &params);
  bool parseFormalParameter(FormalParamList &params);
  bool parseStatementSequence(StmtList &stmts);
  bool parseStatement(StmtList &stmts);
  bool parseIfStatement(StmtList &stmts);
  bool parseWhileStatement(StmtList &stmts);
  bool parseReturnStatement(StmtList &stmts);
  bool parseExpList(ExprList &exprs);
  bool parseExpression(Expr *&e);
  bool parseRelation(OperatorInfo &op);
  bool parseSimpleExpression(Expr *&e);
  bool parseAddOperator(OperatorInfo &op);
  bool parseTerm(Expr *&e);
  bool parseMulOperator(OperatorInfo &op);
  bool parseFactor(Expr *&e);
  bool parseQualident(Decl *&d);
  bool parseIdentList(IdentList &ids);
};

} // namespace tinylang
#endif // TINYLANG_PARSER_PARSER_H
