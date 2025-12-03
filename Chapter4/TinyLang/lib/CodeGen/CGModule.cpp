#include "tinylang/CodeGen/CGModule.h"
#include "tinylang/AST/AST.h"
#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <string>

using namespace tinylang;

void CGModule::initialize() {
  voidType = llvm::Type::getVoidTy(getLLVMContext());
  int1Type = llvm::Type::getInt1Ty(getLLVMContext());
  int32Type = llvm::Type::getInt32Ty(getLLVMContext());
  int64Type = llvm::Type::getInt64Ty(getLLVMContext());
  int32Zero = llvm::ConstantInt::get(int32Type, 0, /*isSigned*/ true);
}

llvm::Type *CGModule::convertType(TypeDeclaration *type) {
  if (type->getName() == "INTEGER") {
    return int64Type;
  }
  if (type->getName() == "BOOLEAN") {
    return int1Type;
  }
  llvm_unreachable("Unsupported type");
}

std::string CGModule::mangleName(Decl *decl) {
  std::string mangledName("_t");
  llvm::SmallVector<llvm::StringRef, 4> parts;
  while (decl) {
    parts.push_back(decl->getName());
    decl = decl->getEnclosingDecl();
  }

  while (!parts.empty()) {
    llvm::StringRef name = parts.pop_back_val();
    mangledName.append(llvm::Twine(name.size()).concat(name).str());
  }
  return mangledName;
}

llvm::GlobalObject *CGModule::getGlobal(Decl *decl) const {
  return declMap.lookup(decl);
}

bool CGModule::addGlobal(Decl *decl, llvm::GlobalObject *global) {
  return declMap.insert({decl, global}).second;
}

void CGModule::run(ModuleDeclaration *astModule) {
  for (auto decl : astModule->getDecls()) {
    if (auto *varDecl = llvm::dyn_cast<VariableDeclaration>(decl)) {
      llvm::GlobalVariable *var = new llvm::GlobalVariable(
          *module, convertType(varDecl->getType()), false,
          llvm::GlobalValue::PrivateLinkage, nullptr, mangleName(varDecl));
      (void)(addGlobal(varDecl, var));
      continue;
    }

    if (auto *proc = llvm::dyn_cast<ProcedureDeclaration>(decl)) {
      CGProcedure CGP(*this);
      CGP.run(proc);
    }
  }
}
