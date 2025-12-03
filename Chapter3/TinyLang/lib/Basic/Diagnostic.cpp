#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"

using namespace tinylang;

static llvm::SmallVector<const char *> diagMessages{
#define DIAG(ID, Level, Msg) Msg,
#include "tinylang/Basic/Diagnostic.def"
};

static llvm::SmallVector<SourceMgr::DiagKind> diagnosticKind{
#define DIAG(ID, Level, Msg) SourceMgr::DK_##Level,
#include "tinylang/Basic/Diagnostic.def"
};

const char *DiagnosticsEngine::getDiagnosticText(unsigned diagID) {
  assert(diagID < diagMessages.size() && "Invalid diagnostic ID");
  return diagMessages[diagID];
}

SourceMgr::DiagKind DiagnosticsEngine::getDiagnosticKind(unsigned diagID) {
  assert(diagID < diagnosticKind.size() && "Invalid diagnostic ID");
  return diagnosticKind[diagID];
}
