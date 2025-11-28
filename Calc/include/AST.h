#ifndef AST_H
#define AST_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

class AST;
class Expr;
class Factor;
class BinaryOp;
class WithDecl;

class ASTVisitor {
public:
  virtual void visit(AST &){};
  virtual void visit(Expr &){};
  virtual void visit(Factor &) = 0;
  virtual void visit(BinaryOp &) = 0;
  virtual void visit(WithDecl &) = 0;
};

class AST {
public:
  virtual ~AST() = default;
  virtual void accept(ASTVisitor &v) = 0;
};

class Expr : public AST {
public:
  Expr() = default;
};

class Factor : public Expr {
public:
  enum class ValueKind { Ident, Number };
  Factor(ValueKind kind, llvm::StringRef val) : kind(kind), val(val) {}
  ValueKind getKind() const { return kind; }
  llvm::StringRef getVal() const { return val; }
  void accept(ASTVisitor &v) override { v.visit(*this); }

private:
  ValueKind kind;
  llvm::StringRef val;
};

class BinaryOp : public Expr {
public:
  enum class Operator { Plus, Minus, Mul, Div };
  BinaryOp(Operator op, Expr *left, Expr *right)
      : left(left), right(right), op(op) {}
  Expr *getLeft() const { return left; }
  Expr *getRight() const { return right; }
  Operator getOperator() const { return op; }
  void accept(ASTVisitor &v) override { v.visit(*this); }

private:
  Expr *left;
  Expr *right;
  Operator op;
};

class WithDecl : public AST {
  using VarVector = llvm::SmallVector<llvm::StringRef, 8>;

public:
  WithDecl(llvm::SmallVector<llvm::StringRef, 8> &vars, Expr *expr)
      : vars(vars), expr(expr) {}
  Expr *getExpr() const { return expr; }
  VarVector::const_iterator begin() const { return vars.begin(); }
  VarVector::const_iterator end() const { return vars.end(); }
  void accept(ASTVisitor &v) override { v.visit(*this); }

private:
  VarVector vars;
  Expr *expr;
};

#endif // AST_H
