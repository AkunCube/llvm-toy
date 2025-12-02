#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Parser/Parser.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <system_error>

using namespace tinylang;

int main(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);
  llvm::SmallVector<const char *, 256> argVec(argv + 1, argv + argc);
  for (const char *file : argVec) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileorErr =
        llvm::MemoryBuffer::getFile(file);
    if (std::error_code ec = fileorErr.getError()) {
      llvm::errs() << "Error reading file " << file << ": " << ec.message()
                   << "\n";
      continue;
    }
    llvm::SourceMgr srcMgr;
    DiagnosticsEngine diagEngine(srcMgr);
    srcMgr.AddNewSourceBuffer(std::move(*fileorErr), llvm::SMLoc());
    Lexer lexer(srcMgr, diagEngine);
    Sema sema(diagEngine);
    Parser parser(lexer, sema);
    parser.parse();
  }
  return 0;
}
