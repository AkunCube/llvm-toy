#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include <utility>

using namespace tinylang;

void KeywordFilter::addKeyword(StringRef keyword, tok::TokenKind kind) {
  hashTable.insert(std::make_pair(keyword, kind));
}

void KeywordFilter::addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(StringRef(#NAME), tok::KW_##NAME);
#include "tinylang/Basic/TokenKinds.def"
}

namespace charinfo {
LLVM_READNONE inline bool isVerticalWhitespace(char ch) {
  return ch == '\r' || ch == '\n';
}

LLVM_READNONE inline bool isHorizontalWhitespace(char ch) {
  return ch == ' ' || ch == '\t' || ch == '\f' || ch == '\v';
}

LLVM_READNONE inline bool isWhitespace(char ch) {
  return isHorizontalWhitespace(ch) || isVerticalWhitespace(ch);
}

LLVM_READNONE inline bool isDigit(char ch) { return ch >= '0' && ch <= '9'; }

LLVM_READNONE inline bool isHexDight(char ch) {
  return isDigit(ch) || (ch >= 'A' && ch <= 'F');
}

LLVM_READNONE inline bool isIdentifierHead(char ch) {
  return ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
}

LLVM_READNONE inline bool isIdentifierBody(char ch) {
  return isIdentifierHead(ch) || isDigit(ch);
}

} // namespace charinfo

void Lexer::next(Token &result) {
  while (*curPtr && charinfo::isWhitespace(*curPtr)) {
    ++curPtr;
  }

  if (!*curPtr) {
    result.setKind(tok::TokenKind::eof);
    return;
  }

  if (charinfo::isIdentifierHead(*curPtr)) {
    identifier(result);
    return;
  }

  if (charinfo::isDigit(*curPtr)) {
    number(result);
    return;
  }

  if (*curPtr == '"' || *curPtr == '\'') {
    string(result);
    return;
  }

  // Handle punctuators.
  switch (*curPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(result, curPtr + 1, tok);                                        \
    break;
    CASE('=', tok::equal);
    CASE('#', tok::hash);
    CASE('+', tok::plus);
    CASE('-', tok::minus);
    CASE('*', tok::star);
    CASE('/', tok::slash);
    CASE(',', tok::comma);
    CASE('.', tok::period);
    CASE(';', tok::semi);
    CASE(')', tok::r_paren);
    CASE('^', tok::caret);
    CASE('[', tok::l_square);
    CASE(']', tok::r_square);
#undef CASE
    case '(':
      if (*(curPtr + 1) == '*') {
        comment();
        next(result);
      } else {
        formToken(result, curPtr + 1, tok::TokenKind::l_paren);
      }
      break;
    case ':':
      if (*(curPtr + 1) == '=') {
        formToken(result, curPtr + 2, tok::TokenKind::colonequal);
      } else {
        formToken(result, curPtr + 1, tok::TokenKind::colon);
      }
      break;
    case '<':
      if (*(curPtr + 1) == '=') {
        formToken(result, curPtr + 2, tok::TokenKind::lessequal);
      } else {
        formToken(result, curPtr + 1, tok::TokenKind::less);
      }
      break;
    case '>':
      if (*(curPtr + 1) == '=') {
        formToken(result, curPtr + 2, tok::TokenKind::greaterequal);
      } else {
        formToken(result, curPtr + 1, tok::TokenKind::greater);
      }
      break;
    default:
      result.setKind(tok::TokenKind::unknown);
  }
}

void Lexer::identifier(Token &result) {
  const char *start = curPtr;
  const char *end = curPtr + 1;
  while (*end && charinfo::isIdentifierBody(*end)) {
    ++end;
  }
  StringRef name(start, end - start);
  formToken(result, end,
            keywordFilter.getKeywordKind(name, tok::TokenKind::identifier));
}

void Lexer::number(Token &result) {
  const char *end = curPtr + 1;
  tok::TokenKind kind = tok::TokenKind::unknown;
  bool isHex = false;
  while (*end) {
    if (!charinfo::isHexDight(*end)) {
      break;
    }
    if (!charinfo::isDigit(*end)) {
      isHex = true;
    }
    ++end;
  }

  switch (*end) {
    case 'H': /* hex number must ends with a `H`. */
      kind = tok::TokenKind::integer_literal;
      ++end;
      break;
    default:
      if (isHex) {
        diagEngine.report(getLoc(), diag::err_hex_digit_in_decimal);
      }
      kind = tok::TokenKind::integer_literal;
      break;
  }
  formToken(result, end, kind);
}

void Lexer::string(Token &result) {
  const char *end = curPtr + 1;
  char quoteChar = *curPtr;
  while (*end && *end != quoteChar && !charinfo::isVerticalWhitespace(*end)) {
    ++end;
  }
  if (charinfo::isVerticalWhitespace(*end)) {
    diagEngine.report(getLoc(), diag::err_unterminated_char_or_string);
  }
  formToken(result, end + 1, tok::TokenKind::string_literal);
}

void Lexer::comment() {
  const char *end = curPtr + 2;
  unsigned level = 1;
  while (*end && level != 0) {
    // Check for nested comment.
    if (*end == '(' && *(end + 1) == '*') {
      ++level;
      end += 2;
      continue;
    }

    if (*end == '*' && *(end + 1) == ')') {
      --level;
      end += 2;
      continue;
    }
    ++end;
  }

  if (!*end) {
    diagEngine.report(getLoc(), diag::err_unterminated_block_comment);
  }
  curPtr = end;
}

void Lexer::formToken(Token &result, const char *TokEnd, tok::TokenKind Kind) {
  size_t length = TokEnd - curPtr;
  result.ptr = curPtr;
  result.length = length;
  result.setKind(Kind);
  curPtr = TokEnd;
}
