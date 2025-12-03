#include "Parser.h"
#include "AST.h"
#include "Lexer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

static BinaryOp::Operator getBinOp(Token::TokenKind kind) {
  switch (kind) {
    case Token::TokenKind::plus:
      return BinaryOp::Operator::Plus;
    case Token::TokenKind::minus:
      return BinaryOp::Operator::Minus;
    case Token::TokenKind::star:
      return BinaryOp::Operator::Mul;
    case Token::TokenKind::slash:
      return BinaryOp::Operator::Div;
    default:
      llvm::report_fatal_error("Invalid binary operator");
  }
}

AST *Parser::parse() {
  AST *res = parseCalc();
  expect(Token::TokenKind::eoi);
  return res;
}

AST *Parser::parseCalc() {
  Expr *expr = nullptr;
  llvm::SmallVector<llvm::StringRef, 8> vars;
  if (token.is(Token::TokenKind::KW_with)) {
    advance();
    if (expect(Token::TokenKind::ident)) {
      goto _error;
    }
    vars.push_back(token.getText());
    advance();
    while (token.is(Token::TokenKind::comma)) {
      advance();
      if (expect(Token::TokenKind::ident)) {
        goto _error;
      }
      vars.push_back(token.getText());
      advance();
    }
    if (consume(Token::TokenKind::colon)) {
      goto _error;
    }
  }
  expr = parseExpr();
  if (vars.empty()) {
    return expr;
  }
  return new WithDecl(vars, expr);

_error:
  while (!token.is(Token::TokenKind::eoi)) {
    advance();
  }
  return nullptr;
}

Expr *Parser::parseExpr() {
  Expr *left = parseTerm();
  while (token.isOneof(Token::TokenKind::plus, Token::TokenKind::minus)) {
    auto op = getBinOp(token.getKind());
    advance();
    Expr *right = parseTerm();
    left = new BinaryOp(op, left, right);
  }
  return left;
}

Expr *Parser::parseTerm() {
  Expr *left = parseFactor();
  while (token.isOneof(Token::TokenKind::star, Token::TokenKind::slash)) {
    auto op = getBinOp(token.getKind());
    advance();
    Expr *right = parseFactor();
    left = new BinaryOp(op, left, right);
  }
  return left;
}

Expr *Parser::parseFactor() {
  Expr *res = nullptr;
  switch (token.getKind()) {
    case Token::TokenKind::number:
      res = new Factor(Factor::ValueKind::Number, token.getText());
      advance();
      break;
    case Token::TokenKind::ident:
      res = new Factor(Factor::ValueKind::Ident, token.getText());
      advance();
      break;
    case Token::TokenKind::l_paren:
      advance();
      res = parseExpr();
      if (!consume(Token::TokenKind::r_paren)) {
        break;
      }
    default:
      if (!res) {
        error();
      }
  }

  while (!token.isOneof(Token::TokenKind::r_paren, Token::TokenKind::star,
                        Token::TokenKind::plus, Token::TokenKind::minus,
                        Token::TokenKind::slash, Token::TokenKind::eoi)) {
    advance();
  }

  return res;
}
