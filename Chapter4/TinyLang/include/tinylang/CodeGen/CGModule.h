#ifndef TINYLANG_CODEGEN_CGMODULE_H
#define TINYLANG_CODEGEN_CGMODULE_H

#include "tinylang/AST/AST.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

namespace tinylang {

class CGModule {

public:
  CGModule(llvm::Module *module) : module(module) { initialize(); }
  void initialize();
  llvm::LLVMContext &getLLVMContext() const { return module->getContext(); }
  llvm::Module *getModule() const { return module; }
  ModuleDeclaration *getModuleDeclaration() const { return astModule; }

  llvm::Type *convertType(TypeDeclaration *type);
  std::string mangleName(Decl *decl);
  llvm::GlobalObject *getGlobal(Decl *) const;
  bool addGlobal(Decl *decl, llvm::GlobalObject *global);

  void run(ModuleDeclaration *astModule);

  llvm::Type *voidType;
  llvm::Type *int1Type;
  llvm::Type *int32Type;
  llvm::Type *int64Type;
  llvm::Constant *int32Zero;

private:
  llvm::Module *module;
  ModuleDeclaration *astModule;
  llvm::DenseMap<Decl *, llvm::GlobalObject *> declMap;
};

} // namespace tinylang

#endif // TINYLANG_CODEGEN_CGMODULE_H
