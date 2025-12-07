#ifndef TINYLANG_LEXER_TOKEN_H
#define TINYLANG_LEXER_TOKEN_H

#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinylang {
class Lexer;

class Token {
  friend class Lexer;

public:
  tok::TokenKind getKind() const { return kind; }
  void setKind(tok::TokenKind k) { kind = k; }
  bool is(tok::TokenKind k) const { return kind == k; }
  bool isNot(tok::TokenKind k) const { return kind != k; }

  template <typename... Tokens> bool isOneof(Tokens &&...tokens) const {
    return (... || is(tokens));
  }

  const char *getName() const { return tok::getTokenName(kind); }
  SMLoc getLocation() const { return SMLoc::getFromPointer(ptr); }
  size_t getLength() const { return length; }
  StringRef getIdentifier() const {
    assert(is(tok::identifier) && "Not an identifier token");
    return StringRef(ptr, length);
  }
  StringRef getLiteralData() const {
    assert(isOneof(tok::integer_literal, tok::string_literal) &&
           "Not a literal token");
    return StringRef(ptr, length);
  }

private:
  /// The location of the token.
  const char *ptr;
  /// The length of the token.
  size_t length;
  /// Kind - The actual flavor of token this is.
  tok::TokenKind kind;
};

} // namespace tinylang

#endif // TINYLANG_LEXER_TOKEN_H
