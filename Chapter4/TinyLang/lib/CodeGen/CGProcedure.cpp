#include "tinylang/CodeGen/CGProcedure.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TypeSize.h"

using namespace tinylang;

llvm::Type *CGProcedure::mapType(Decl *decl, bool honorReference) {
  if (auto *fp = dyn_cast<FormalParameterDeclaration>(decl)) {
    if (fp->isVar() && honorReference) {
      return llvm::PointerType::get(cgModule.getLLVMContext(), 0);
    }
    return cgModule.convertType(fp->getType());
  }

  if (auto *var = dyn_cast<VariableDeclaration>(decl)) {
    return cgModule.convertType(var->getType());
  }
  return cgModule.convertType(cast<TypeDeclaration>(decl));
}

llvm::FunctionType *
CGProcedure::createFunctionType(ProcedureDeclaration *proc) {
  llvm::Type *resultType = cgModule.voidType;
  if (proc->getReturnType()) {
    resultType = mapType(proc->getReturnType());
  }
  llvm::SmallVector<llvm::Type *, 8> paramTypes;
  for (auto fp : proc->getFormalParams()) {
    paramTypes.push_back(mapType(fp));
  }
  return llvm::FunctionType::get(resultType, paramTypes, false);
}

llvm::Function *CGProcedure::createFunction(ProcedureDeclaration *proc,
                                            llvm::FunctionType *fnType) {
  auto *fn =
      llvm::Function::Create(fnType, llvm::GlobalValue::ExternalLinkage,
                             cgModule.mangleName(proc), cgModule.getModule());
  // Give parameters a name.
  for (auto [fp, arg] : llvm::zip(proc->getFormalParams(), fn->args())) {
    if (fp->isVar()) {
      llvm::AttrBuilder attr(cgModule.getLLVMContext());
      // TODO: what is this?
      llvm::TypeSize size =
          cgModule.getModule()->getDataLayout().getTypeStoreSize(
              cgModule.convertType(fp->getType()));
      attr.addDereferenceableAttr(size);
      attr.addAttribute(llvm::Attribute::NoCapture);
      arg.addAttrs(attr);
    }
    arg.setName(fp->getName());
  }
  return fn;
}

llvm::Value *CGProcedure::emitInfixExpr(InfixExpression *e) {
  llvm::Value *left = emitExpr(e->getLHS());
  llvm::Value *right = emitExpr(e->getRHS());
  llvm::Value *result = nullptr;
  switch (e->getOperatorInfo().getKind()) {
    case tok::TokenKind::plus:
      result = builder.CreateNSWAdd(left, right);
      break;
    case tok::TokenKind::minus:
      result = builder.CreateNSWSub(left, right);
      break;
    case tok::TokenKind::star:
      result = builder.CreateNSWMul(left, right);
      break;
    case tok::TokenKind::KW_DIV:
      result = builder.CreateSDiv(left, right);
      break;
    case tok::TokenKind::KW_MOD:
      result = builder.CreateSRem(left, right);
      break;
    case tok::TokenKind::equal:
      result = builder.CreateICmpEQ(left, right);
      break;
    case tok::TokenKind::hash:
      result = builder.CreateICmpNE(left, right);
      break;
    case tok::lessequal:
      result = builder.CreateICmpSLE(left, right);
      break;
    case tok::greater:
      result = builder.CreateICmpSGT(left, right);
      break;
    case tok::greaterequal:
      result = builder.CreateICmpSGE(left, right);
      break;
    case tok::TokenKind::KW_AND:
      result = builder.CreateAnd(left, right);
      break;
    case tok::TokenKind::KW_OR:
      result = builder.CreateOr(left, right);
      break;
    case tok::TokenKind::slash:
      LLVM_FALLTHROUGH;
    default:
      llvm_unreachable("Wrong operator");
  }
  return result;
}

llvm::Value *CGProcedure::emitPrefixExpr(PrefixExpression *e) {
  llvm::Value *result = emitExpr(e->getExpr());
  switch (e->getOperatorInfo().getKind()) {
    case tok::TokenKind::plus:
      break;
    case tok::TokenKind::minus:
      result = builder.CreateNeg(result);
    case tok::TokenKind::KW_NOT:
      result = builder.CreateNot(result);
    default:
      llvm_unreachable("Wrong operator");
  }
  return result;
}

llvm::Value *CGProcedure::emitExpr(Expr *e) {
  if (auto *infix = dyn_cast<InfixExpression>(e)) {
    return emitInfixExpr(infix);
  }
  if (auto *prefix = dyn_cast<PrefixExpression>(e)) {
    return emitPrefixExpr(prefix);
  }
  if (auto *var = dyn_cast<VariableAccess>(e)) {
    Decl *decl = var->getDecl();
    return readVariable(currBB, decl);
  }
  if (auto *constAccess = dyn_cast<ConstantAccess>(e)) {
    return emitExpr(constAccess->geDecl()->getExpr());
  }
  if (auto *intLit = dyn_cast<IntegerLiteral>(e)) {
    return llvm::ConstantInt::get(cgModule.int64Type, intLit->getValue());
  }
  if (auto *boolLit = dyn_cast<BooleanLiteral>(e)) {
    return llvm::ConstantInt::get(cgModule.int1Type, boolLit->getValue());
  }
  llvm_unreachable("Unsupported expression");
}

void CGProcedure::emitStmt(AssignmentStatement *stmt) {
  llvm::Value *val = emitExpr(stmt->getExpr());
  writeVariable(currBB, stmt->getVar(), val);
}

void CGProcedure::emitStmt(ProcedureCallStatement *stmt [[maybe_unused]]) {
  llvm::report_fatal_error("not implemented");
}

void CGProcedure::emitStmt(IfStatement *stmt) {
  bool hasElse = !stmt->getElseStmts().empty();
  auto *ifBB =
      llvm::BasicBlock::Create(cgModule.getLLVMContext(), "if.body", func);
  auto *elseBB = hasElse ? llvm::BasicBlock::Create(cgModule.getLLVMContext(),
                                                    "else.body", func)
                         : nullptr;
  auto *endBB =
      llvm::BasicBlock::Create(cgModule.getLLVMContext(), "after.if", func);
  llvm::Value *cond = emitExpr(stmt->getCond());
  builder.CreateCondBr(cond, ifBB, hasElse ? elseBB : endBB);
  sealBlock(currBB);
  setCurr(ifBB);
  emit(stmt->getIfStmts());
  if (!currBB->getTerminator()) {
    builder.CreateBr(endBB);
  }
  sealBlock(currBB);
  if (hasElse) {
    setCurr(elseBB);
    emit(stmt->getElseStmts());
    if (!currBB->getTerminator()) {
      builder.CreateBr(endBB);
    }
    sealBlock(currBB);
  }
  setCurr(endBB);
}

void CGProcedure::emitStmt(WhileStatement *stmt) {
  auto *whileCondBB =
      llvm::BasicBlock::Create(cgModule.getLLVMContext(), "while.cond", func);
  auto *whileBodyBB =
      llvm::BasicBlock::Create(cgModule.getLLVMContext(), "while.body", func);
  auto *whileEndBB =
      llvm::BasicBlock::Create(cgModule.getLLVMContext(), "after.while", func);

  builder.CreateBr(whileCondBB);
  sealBlock(currBB);
  setCurr(whileBodyBB);
  llvm::Value *cond = emitExpr(stmt->getCond());
  builder.CreateCondBr(cond, whileBodyBB, whileEndBB);
  setCurr(whileBodyBB);
  emit(stmt->getWhileStmts());
  builder.CreateBr(whileCondBB);
  sealBlock(whileCondBB);
  sealBlock(currBB);
  setCurr(whileEndBB);
}

void CGProcedure::emitStmt(ReturnStatement *stmt) {
  if (stmt->getRetVal()) {
    builder.CreateRet(emitExpr(stmt->getRetVal()));
  } else {
    builder.CreateRetVoid();
  }
}

void CGProcedure::emit(const StmtList &stmts) {
  for (Stmt *s : stmts) {
    if (auto *stmt = dyn_cast<AssignmentStatement>(s)) {
      emitStmt(stmt);
    } else if (auto *stmt = dyn_cast<ProcedureCallStatement>(s)) {
      emitStmt(stmt);
    } else if (auto *stmt = dyn_cast<IfStatement>(s)) {
      emitStmt(stmt);
    } else if (auto *stmt = dyn_cast<WhileStatement>(s)) {
      emitStmt(stmt);
    } else if (auto *stmt = dyn_cast<ReturnStatement>(s)) {
      emitStmt(stmt);
    } else {
      llvm_unreachable("Unknown statement");
    }
  }
}
