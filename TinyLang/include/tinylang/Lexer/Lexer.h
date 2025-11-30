#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SourceMgr.h"
#include <llvm/Support/SMLoc.h>

namespace tinylang {
class KeywordFilter {
public:
  void addKeywords();

  tok::TokenKind
  getKeywordKind(StringRef keyword,
                 tok::TokenKind defaultTokenCode = tok::unknown) const {
    auto it = hashTable.find(keyword);
    if (it != hashTable.end()) {
      return it->second;
    }
    return defaultTokenCode;
  }

private:
  StringMap<tok::TokenKind> hashTable;
  void addKeyword(StringRef keyword, tok::TokenKind kind);
};

class Lexer {
public:
  Lexer(SourceMgr &sourceMgr, DiagnosticsEngine &diagEngine)
      : sourceMgr(sourceMgr), diagEngine(diagEngine) {
    curBufId = sourceMgr.getMainFileID();
    curBuf = sourceMgr.getMemoryBuffer(curBufId)->getBuffer();
    curPtr = curBuf.begin();
    keywordFilter.addKeywords();
  }

  DiagnosticsEngine &getDiagEngine() const { return diagEngine; }
  void next(Token &result);
  StringRef getBuffer() const { return curBuf; }

private:
  SourceMgr &sourceMgr;
  DiagnosticsEngine &diagEngine;

  const char *curPtr;
  StringRef curBuf;
  /// This is the current buffer index we're lexing from as managed by the
  /// SourceMgr object.
  unsigned curBufId;

  KeywordFilter keywordFilter;

  void identifier(Token &result);
  void number(Token &result);
  void string(Token &result);
  void comment();
  SMLoc getLoc() const { return SMLoc::getFromPointer(curPtr); }
  void formToken(Token &result, const char *TokEnd, tok::TokenKind Kind);
};

} // namespace tinylang

#endif // TINYLANG_LEXER_LEXER_H
