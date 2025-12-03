#ifndef TINYLANG_BASIC_DIAGNOSTIC_H
#define TINYLANG_BASIC_DIAGNOSTIC_H

#include "tinylang/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
#include <utility>

namespace tinylang {
namespace diag {
enum {
#define DIAG(ID, Level, Msg) ID,
#include "tinylang/Basic/Diagnostic.def"
};
} // namespace diag

class DiagnosticsEngine {
public:
  DiagnosticsEngine(SourceMgr &srcMgr) : srcMgr(srcMgr), numErrors(0) {}
  unsigned getNumErrors() const { return numErrors; }

  template <typename... Args>
  void report(SMLoc loc, unsigned diagID, Args &&...arguments) {
    std::string msg = llvm::formatv(getDiagnosticText(diagID),
                                    std::forward<Args>(arguments)...)
                          .str();
    SourceMgr::DiagKind kind = getDiagnosticKind(diagID);
    srcMgr.PrintMessage(loc, kind, msg);
    numErrors += (kind == SourceMgr::DK_Error);
  }

private:
  SourceMgr &srcMgr;
  unsigned numErrors;

  static const char *getDiagnosticText(unsigned diagID);
  static SourceMgr::DiagKind getDiagnosticKind(unsigned diagID);
};

} // namespace tinylang

#endif // TINYLANG_BASIC_DIAGNOSTIC_H
