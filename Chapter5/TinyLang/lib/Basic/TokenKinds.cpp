#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/ErrorHandling.h"

using namespace tinylang;

static llvm::SmallVector<const char *> tokNames{
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "tinylang/Basic/TokenKinds.def"
};

const char *tok::getTokenName(tok::TokenKind kind) {
  if (kind < tokNames.size())
    return tokNames[kind];
  llvm_unreachable("unknown TokenKind");
  return nullptr;
}

const char *tok::getPunctuatorSpelling(tok::TokenKind kind) {
  switch (kind) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return SP;
#include "tinylang/Basic/TokenKinds.def"
    default:
      break;
  }
  return nullptr;
}

const char *tok::getKeywordSpelling(TokenKind kind) {
  switch (kind) {
#define KEYWORD(ID, FLAG)                                                      \
  case KW##ID:                                                                 \
    return #ID;
    default:
      break;
  }
  return nullptr;
}
