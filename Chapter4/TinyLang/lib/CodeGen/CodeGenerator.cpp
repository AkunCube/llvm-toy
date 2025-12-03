#include "tinylang/CodeGen/CodeGenerator.h"
#include "tinylang/CodeGen/CGModule.h"
#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace tinylang;

CodeGenerator *CodeGenerator::create(llvm::LLVMContext &ctx,
                                     llvm::TargetMachine *tm) {
  return new CodeGenerator(ctx, tm);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(ModuleDeclaration *module,
                                                 std::string fileName) {
  std::unique_ptr<llvm::Module> m =
      std::make_unique<llvm::Module>(fileName, context);
  m->setTargetTriple(targetMachine->getTargetTriple().getTriple());
  m->setDataLayout(targetMachine->createDataLayout());
  CGModule cgModule(m.get());
  cgModule.run(module);
  return m;
}
