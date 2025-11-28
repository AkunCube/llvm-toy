#ifndef PARSER_H
#define PARSER_H

#include "AST.h"
#include "Lexer.h"
#include "llvm/Support/raw_ostream.h"

class Parser {
public:
  Parser(Lexer &lexer) : lexer(lexer), _hasError(false) { advance(); }
  AST *parse();
  bool hasError() const { return _hasError; }

private:
  Lexer &lexer;
  Token token;
  bool _hasError;

  AST *parseCalc();
  Expr *parseExpr();
  Expr *parseTerm();
  Expr *parseFactor();

  void error() {
    llvm::errs() << "Unexpected: " << token.getText() << "\n";
    _hasError = true;
  }

  void advance() { lexer.next(token); }

  bool expect(Token::TokenKind kind) {
    if (!token.is(kind)) {
      error();
      return true;
    }
    return false;
  }

  bool consume(Token::TokenKind kind) {
    if (expect(kind)) {
      return true;
    }
    advance();
    return false;
  }
};

#endif // PARSER_H
