#ifndef LEXER_H
#define LEXER_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"

class Lexer;

class Token {
  friend class Lexer;

public:
  enum class TokenKind {
    eoi,
    unknown,
    ident,
    number,
    comma,
    colon,
    plus,
    minus,
    star,
    slash,
    l_paren,
    r_paren,
    KW_with
  };

public:
  TokenKind getKind() const { return kind; }
  llvm::StringRef getText() const { return text; }
  bool is(TokenKind k) const { return kind == k; }
  bool isOneof(TokenKind k1, TokenKind k2) const { return is(k1) || is(k2); }

  template <typename... Ts>
  bool isOneof(TokenKind k1, TokenKind k2, Ts... ks) const {
    return is(k1) || isOneof(k2, ks...);
  }

private:
  TokenKind kind;
  llvm::StringRef text;
};

class Lexer {

public:
  Lexer(const llvm::StringRef &buffer) {
    bufferStart = buffer.begin();
    bufferPtr = bufferStart;
  }

  void next(Token &token);

private:
  const char *bufferStart;
  const char *bufferPtr;

  void formToken(Token &result, const char *tokenEnd, Token::TokenKind kind);
};

#endif // LEXER_H
