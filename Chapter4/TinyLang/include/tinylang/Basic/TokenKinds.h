#ifndef TINYLANG_BASIC_TOKENKINDS_H
#define TINYLANG_BASIC_TOKENKINDS_H

#include "llvm/Support/Compiler.h"

namespace tinylang {
namespace tok {
enum TokenKind {
#define TOK(ID) ID,
#include "TokenKinds.def"
};

const char *getTokenName(TokenKind kind) LLVM_READNONE;
const char *getPunctuatorSpelling(TokenKind kind) LLVM_READNONE;
const char *getKeywordSpelling(TokenKind kind) LLVM_READNONE;

} // namespace tok
} // namespace tinylang

#endif // TINYLANG_BASIC_TOKENKINDS_H
