#include "Sema.h"
#include "AST.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/ADT/StringRef.h>

namespace {
class DeclCheck : public ASTVisitor {
public:
  bool hasError() const { return _hasError; }
  DeclCheck() : _hasError(false) {}
  void visit(Factor &node) override {
    if (node.getKind() == Factor::ValueKind::Ident) {
      if (!scope.contains(node.getVal())) {
        error(ErrorType::Not, node.getVal());
      }
    }
  }

  void visit(BinaryOp &node) override {
    if (node.getLeft()) {
      node.getLeft()->accept(*this);
    } else {
      _hasError = true;
    }
    if (node.getRight()) {
      node.getRight()->accept(*this);
    } else {
      _hasError = true;
    }
  }
  void visit(WithDecl &node) override {
    for (llvm::StringRef var : node) {
      if (!scope.insert(var).second) {
        error(ErrorType::Twice, var);
      }
    }

    if (node.getExpr()) {
      node.getExpr()->accept(*this);
    }
  }

private:
  enum class ErrorType { Twice, Not };
  llvm::StringSet<> scope;
  bool _hasError;

  void error(ErrorType et, llvm::StringRef v) {
    llvm::errs() << "Variable " << v << " "
                 << (et == ErrorType::Twice ? "already" : "not")
                 << " declared\n";
    _hasError = true;
  }
};
} // namespace

bool Sema::semantic(AST *tree) {
  if (!tree) {
    return false;
  }
  DeclCheck check;
  tree->accept(check);
  return check.hasError();
}
