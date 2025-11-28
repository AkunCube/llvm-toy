#include "Lexer.h"

namespace charinfo {
LLVM_READNONE inline bool isWhiteSpace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}

LLVM_READNONE inline bool isDighter(char c) { return c >= '0' && c <= '9'; }

LLVM_READNONE inline bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
} // namespace charinfo

static constexpr llvm::StringRef kWith = "with";

void Lexer::next(Token &token) {
  while (*bufferPtr && charinfo::isWhiteSpace(*bufferPtr)) {
    ++bufferPtr;
  }

  if (!*bufferPtr) {
    token.kind = Token::TokenKind::eoi;
    return;
  }

  if (charinfo::isLetter(*bufferPtr)) {
    const char *end = bufferPtr + 1;
    while (charinfo::isLetter(*end)) {
      ++end;
    }
    llvm::StringRef name(bufferPtr, end - bufferPtr);
    Token::TokenKind kind =
        name == kWith ? Token::TokenKind::KW_with : Token::TokenKind::ident;
    formToken(token, end, Token::TokenKind::ident);
    return;
  }

  if (charinfo::isDighter(*bufferPtr)) {
    const char *end = bufferPtr + 1;
    while (charinfo::isDighter(*end)) {
      ++end;
    }
    formToken(token, end, Token::TokenKind::number);
    return;
  }

  switch (*bufferPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, bufferPtr + 1, tok);                                      \
    break

    CASE('+', Token::TokenKind::plus);
    CASE('-', Token::TokenKind::minus);
    CASE('*', Token::TokenKind::star);
    CASE('/', Token::TokenKind::slash);
    CASE('(', Token::TokenKind::l_paren);
    CASE(')', Token::TokenKind::r_paren);
    CASE(':', Token::TokenKind::colon);
    CASE(',', Token::TokenKind::comma);
#undef CASE
    default:
      formToken(token, bufferPtr + 1, Token::TokenKind::unknown);
  }
}

void Lexer::formToken(Token &result, const char *tokenEnd,
                      Token::TokenKind kind) {
  result.kind = kind;
  result.text = llvm::StringRef(bufferPtr, tokenEnd - bufferPtr);
  bufferPtr = tokenEnd;
}
