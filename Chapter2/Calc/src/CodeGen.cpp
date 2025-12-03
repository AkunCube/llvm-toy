#include "CodeGen.h"
#include "AST.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

class ToIRVisitor : public ASTVisitor {
public:
  ToIRVisitor(Module *m) : m(m), builder(m->getContext()) {
    voidTy = builder.getVoidTy();
    int32Ty = Type::getInt32Ty(m->getContext());
    ptrTy = PointerType::getUnqual(m->getContext());
    int32Zero = builder.getInt32(0);
    value = nullptr;
  }

  void run(AST *tree) {
    FunctionType *mainFty = FunctionType::get(int32Ty, {int32Ty, ptrTy}, false);
    Function *main =
        Function::Create(mainFty, GlobalValue::ExternalLinkage, "main", m);
    BasicBlock *bb = BasicBlock::Create(m->getContext(), "entry", main);
    builder.SetInsertPoint(bb);
    if (!tree) {
      return;
    }
    tree->accept(*this);

    FunctionType *calcWriteFnTy = FunctionType::get(voidTy, {int32Ty}, false);
    Function *calcWriteFn = Function::Create(
        calcWriteFnTy, GlobalValue::ExternalLinkage, "calc_write", m);
    builder.CreateCall(calcWriteFnTy, calcWriteFn, {value});
    builder.CreateRet(int32Zero);
  }

  void visit(WithDecl &node) override {
    FunctionType *readFty = FunctionType::get(int32Ty, {ptrTy}, false);
    Function *readF =
        Function::Create(readFty, GlobalValue::ExternalLinkage, "calc_read", m);
    for (llvm::StringRef var : node) {
      Constant *strText = ConstantDataArray::getString(m->getContext(), var);
      GlobalVariable *str = new GlobalVariable(
          *m, strText->getType(), true, GlobalValue::PrivateLinkage, strText,
          Twine(var).concat(".str"));
      CallInst *call = builder.CreateCall(readFty, readF, {str});
      nameMap[var] = call;
    }
    node.getExpr()->accept(*this);
  }

  void visit(Factor &node) override {
    if (node.getKind() == Factor::ValueKind::Ident) {
      value = nameMap[node.getVal()];
    } else {
      int intVal = 0;
      node.getVal().getAsInteger(10, intVal);
      value = ConstantInt::get(int32Ty, intVal, true);
    }
  }

  void visit(BinaryOp &node) override {
    node.getLeft()->accept(*this);
    Value *left = value;
    node.getRight()->accept(*this);
    Value *right = value;

    switch (node.getOperator()) {
      case BinaryOp::Operator::Plus:
        value = builder.CreateNSWAdd(left, right);
        break;
      case BinaryOp::Operator::Minus:
        value = builder.CreateNSWSub(left, right);
        break;
      case BinaryOp::Operator::Mul:
        value = builder.CreateNSWMul(left, right);
        break;
      case BinaryOp::Operator::Div:
        value = builder.CreateSDiv(left, right);
        break;
    }
  }

private:
  Module *m;
  IRBuilder<> builder;
  Type *voidTy;
  Type *int32Ty;
  PointerType *ptrTy;
  Constant *int32Zero;
  Value *value;
  StringMap<Value *> nameMap;
};

} // namespace

void CodeGen::compile(AST *tree) {
  LLVMContext ctx;
  Module *m = new Module("calc.expr", ctx);
  ToIRVisitor toIR(m);
  toIR.run(tree);
  m->print(outs(), nullptr);
}
