#ifndef TINYLANG_CODEGEN_CODEGENERATOR_H
#define TINYLANG_CODEGEN_CODEGENERATOR_H

#include "tinylang/AST/AST.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Target/TargetMachine.h"
#include <string>
namespace tinylang {
class CodeGenerator {

public:
  static CodeGenerator *create(llvm::LLVMContext &ctx, llvm::TargetMachine *tm);
  std::unique_ptr<llvm::Module> run(ModuleDeclaration *module,
                                    std::string fileName);

protected:
  CodeGenerator(llvm::LLVMContext &ctx, llvm::TargetMachine *tm)
      : context(ctx), targetMachine(tm) {}

private:
  llvm::LLVMContext &context;
  llvm::TargetMachine *targetMachine;
};

} // namespace tinylang

#endif // TINYLANG_CODEGEN_CODEGENERATOR_H
