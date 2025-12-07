#ifndef TINYLANG_CODEGEN_CGPROCEDURE_H
#define TINYLANG_CODEGEN_CGPROCEDURE_H

#include "tinylang/AST/AST.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/ValueHandle.h"

namespace llvm {
class Function;
}

namespace tinylang {

class CGProcedure {
public:
  CGProcedure(CGModule &cgModule)
      : cgModule(cgModule), builder(cgModule.getLLVMContext()),
        currBB(nullptr) {}

  void run(ProcedureDeclaration *proc);

private:
  struct BasicBlockDef {
    /// Maps the variable (or formal parameter) to its definition.
    llvm::DenseMap<Decl *, llvm::TrackingVH<llvm::Value>> defs;
    /// Set of incompleted phi instructions.
    llvm::DenseMap<llvm::PHINode *, Decl *> incompletePhis;
    /// Block is sealed, that is, no more predecessors will be added.
    bool sealed;

    BasicBlockDef() : sealed(false) {}
  };

  CGModule &cgModule;
  llvm::IRBuilder<> builder;
  llvm::BasicBlock *currBB;
  ProcedureDeclaration *proc;
  llvm::FunctionType *fType;
  llvm::Function *func;
  llvm::DenseMap<FormalParameterDeclaration *, llvm::Argument *> formalParams;
  llvm::DenseMap<llvm::BasicBlock *, BasicBlockDef> currentDef;

  void writeLocalVariable(llvm::BasicBlock *BB, Decl *decl, llvm::Value *val);
  llvm::Value *readLocalVariable(llvm::BasicBlock *BB, Decl *decl);
  llvm::Value *readLocalVariableRecursive(llvm::BasicBlock *BB, Decl *decl);
  llvm::PHINode *addEmptyPhi(llvm::BasicBlock *BB, Decl *decl);
  llvm::Value *addPhiOperands(llvm::BasicBlock *BB, Decl *decl,
                              llvm::PHINode *phi);
  llvm::Value *optimizePhi(llvm::PHINode *phi);
  void sealBlock(llvm::BasicBlock *BB);

  void writeVariable(llvm::BasicBlock *BB, Decl *decl, llvm::Value *val);
  llvm::Value *readVariable(llvm::BasicBlock *BB, Decl *decl);

  llvm::Type *mapType(Decl *decl, bool honorReference = true);
  llvm::FunctionType *createFunctionType(ProcedureDeclaration *proc);
  llvm::Function *createFunction(ProcedureDeclaration *proc,
                                 llvm::FunctionType *fnType);

protected:
  void setCurr(llvm::BasicBlock *BB) {
    assert(BB && "Cannot set current basic block to null");
    currBB = BB;
    builder.SetInsertPoint(currBB);
  }

  llvm::Value *emitInfixExpr(InfixExpression *e);
  llvm::Value *emitPrefixExpr(PrefixExpression *e);
  llvm::Value *emitExpr(Expr *e);

  void emitStmt(AssignmentStatement *stmt);
  void emitStmt(ProcedureCallStatement *stmt);
  void emitStmt(IfStatement *stmt);
  void emitStmt(WhileStatement *stmt);
  void emitStmt(ReturnStatement *stmt);
  void emit(const StmtList &stmts);
};

} // namespace tinylang

#endif // TINYLANG_CODEGEN_CGPROCEDURE_H
