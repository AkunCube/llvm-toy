#ifndef TINYLANG_AST_AST_H
#define TINYLANG_AST_AST_H

#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <string>
#include <utility>
#include <vector>

namespace tinylang {

class Decl;
class FormalParameterDeclaration;
class Expr;
class Stmt;

using DeclList = std::vector<Decl *>;
using FormalParamList = std::vector<FormalParameterDeclaration *>;
using ExprList = std::vector<Expr *>;
using StmtList = std::vector<Stmt *>;

class Ident {
public:
  Ident(SMLoc loc, StringRef name) : loc(loc), name(name) {}
  SMLoc getLocation() const { return loc; }
  const StringRef &getName() const { return name; }

private:
  SMLoc loc;
  StringRef name;
};

using IdentList = std::vector<std::pair<SMLoc, StringRef>>;

class Decl {
public:
  enum DeclKind { DK_Module, DK_Const, DK_Type, DK_Var, DK_Param, DK_Proc };
  Decl(DeclKind kind, Decl *enclosingDecl, SMLoc loc, StringRef name)
      : kind(kind), enclosingDecl(enclosingDecl), loc(loc), name(name) {}
  DeclKind getKind() const { return kind; }
  Decl *getEnclosingDecl() const { return enclosingDecl; }
  SMLoc getLocation() const { return loc; }
  StringRef getName() const { return name; }

private:
  const DeclKind kind;

protected:
  Decl *enclosingDecl;
  SMLoc loc;
  StringRef name;
};

class ModuleDeclaration : public Decl {
public:
  ModuleDeclaration(Decl *enclosingDecl, SMLoc loc, StringRef name)
      : Decl(DK_Module, enclosingDecl, loc, name) {}

  ModuleDeclaration(Decl *enclosingDecl, SMLoc loc, StringRef name, DeclList &d,
                    StmtList &s)
      : Decl(DK_Module, enclosingDecl, loc, name), decls(d), stmts(s) {}

  const DeclList &getDecls() const { return decls; }
  void setDecls(DeclList &d) { decls = d; }
  const StmtList &getStmts() const { return stmts; }
  void setStmts(StmtList &s) { stmts = s; }

  static bool classof(const Decl *decl) { return decl->getKind() == DK_Module; }

private:
  DeclList decls;
  StmtList stmts;
};

class ConstantDeclaration : public Decl {
public:
  ConstantDeclaration(Decl *enclosingDecl, SMLoc loc, StringRef name, Expr *e)
      : Decl(DK_Const, enclosingDecl, loc, name), e(e) {}

  Expr *getExpr() { return e; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Const; }

private:
  Expr *e;
};

class TypeDeclaration : public Decl {
public:
  TypeDeclaration(Decl *enclosingDecL, SMLoc loc, StringRef name)
      : Decl(DK_Type, enclosingDecL, loc, name) {}

  static bool classof(const Decl *D) { return D->getKind() == DK_Type; }
};

class VariableDeclaration : public Decl {
public:
  VariableDeclaration(Decl *enclosingDecL, SMLoc loc, StringRef name,
                      TypeDeclaration *type)
      : Decl(DK_Var, enclosingDecL, loc, name), type(type) {}

  TypeDeclaration *getType() { return type; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Var; }

private:
  TypeDeclaration *type;
};

class FormalParameterDeclaration : public Decl {
public:
  FormalParameterDeclaration(Decl *enclosingDecL, SMLoc loc, StringRef name,
                             TypeDeclaration *Ty, bool isVar)
      : Decl(DK_Param, enclosingDecL, loc, name), type(Ty), _isVar(isVar) {}

  TypeDeclaration *getType() { return type; }
  bool isVar() { return _isVar; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Param; }

private:
  TypeDeclaration *type;
  bool _isVar;
};

class ProcedureDeclaration : public Decl {
public:
  ProcedureDeclaration(Decl *enclosingDecL, SMLoc loc, StringRef name)
      : Decl(DK_Proc, enclosingDecL, loc, name) {}

  ProcedureDeclaration(Decl *enclosingDecL, SMLoc loc, StringRef name,
                       FormalParamList &params, TypeDeclaration *returnType,
                       DeclList &d, StmtList &s)
      : Decl(DK_Proc, enclosingDecL, loc, name), params(params),
        returnType(returnType), decls(d), stmts(s) {}

  const FormalParamList &getFormalParams() const { return params; }
  void setFormalParams(FormalParamList &p) { params = p; }
  TypeDeclaration *getReturnType() const { return returnType; }
  void setReturnType(TypeDeclaration *retType) { returnType = retType; }

  const DeclList &getDecls() const { return decls; }
  void setDecls(DeclList &d) { decls = d; }
  const StmtList &getStmts() const { return stmts; }
  void setStmts(StmtList &s) { stmts = s; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Proc; }

private:
  FormalParamList params;
  TypeDeclaration *returnType;
  DeclList decls;
  StmtList stmts;
};

class OperatorInfo {
public:
  OperatorInfo() : loc(), kind(tok::unknown), _isUnspecified(true) {}
  OperatorInfo(SMLoc loc, tok::TokenKind kind, bool isUnspecified = false)
      : loc(loc), kind(kind), _isUnspecified(isUnspecified) {}

  SMLoc getLocation() const { return loc; }
  tok::TokenKind getKind() const { return static_cast<tok::TokenKind>(kind); }
  bool isUnspecified() const { return _isUnspecified; }

private:
  SMLoc loc;
  uint32_t kind : 16;
  uint32_t _isUnspecified : 1;
};

class Expr {
public:
  enum ExprKind {
    EK_Infix,
    EK_Prefix,
    EK_Int,
    EK_Bool,
    EK_Var,
    EK_Const,
    EK_Func,
  };
  ExprKind getKind() const { return kind; }
  TypeDeclaration *getType() const { return type; }
  void setType(TypeDeclaration *ty) { type = ty; }
  bool isConstant() const { return _isConstant; }

protected:
  Expr(ExprKind kind, TypeDeclaration *type, bool isConstant)
      : kind(kind), type(type), _isConstant(isConstant) {}

private:
  const ExprKind kind;
  TypeDeclaration *type;
  bool _isConstant;
};

class InfixExpression : public Expr {
public:
  InfixExpression(Expr *lhs, Expr *rhs, OperatorInfo op, TypeDeclaration *type,
                  bool isConst)
      : Expr(EK_Infix, type, isConst), lhs(lhs), rhs(rhs), op(op) {}

  Expr *getLHS() const { return lhs; }
  Expr *getRHS() const { return rhs; }
  const OperatorInfo &getOperatorInfo() const { return op; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Infix; }

private:
  Expr *lhs;
  Expr *rhs;
  const OperatorInfo op;
};

class PrefixExpression : public Expr {
public:
  PrefixExpression(Expr *e, OperatorInfo op, TypeDeclaration *type,
                   bool isConst)
      : Expr(EK_Prefix, type, isConst), e(e), op(op) {}

  Expr *getExpr() const { return e; }
  const OperatorInfo &getOperatorInfo() const { return op; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Prefix; }

private:
  Expr *e;
  const OperatorInfo op;
};

class IntegerLiteral : public Expr {
public:
  IntegerLiteral(SMLoc loc, const llvm::APInt &value, TypeDeclaration *type)
      : Expr(EK_Int, type, true), loc(loc), value(value) {}

  SMLoc getLocation() const { return loc; }
  const llvm::APInt &getValue() const { return value; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Int; }

private:
  SMLoc loc;
  llvm::APInt value;
};

class BooleanLiteral : public Expr {
public:
  BooleanLiteral(bool value, TypeDeclaration *type)
      : Expr(EK_Bool, type, true), value(value) {}

  bool getValue() const { return value; }
  static bool classof(const Expr *E) { return E->getKind() == EK_Bool; }

private:
  bool value;
};

class VariableAccess : public Expr {
public:
  VariableAccess(VariableDeclaration *var)
      : Expr(EK_Var, var->getType(), false), var(var) {}
  VariableAccess(FormalParameterDeclaration *param)
      : Expr(EK_Var, param->getType(), false), var(param) {}

  Decl *getDecl() const { return var; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Var; }

private:
  Decl *var;
};

class ConstantAccess : public Expr {
public:
  ConstantAccess(ConstantDeclaration *c)
      : Expr(EK_Const, c->getExpr()->getType(), true), c(c) {}

  ConstantDeclaration *getDecl() { return c; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Const; }

private:
  ConstantDeclaration *c;
};

class FunctionCallExpr : public Expr {
public:
  FunctionCallExpr(ProcedureDeclaration *proc, ExprList params)
      : Expr(EK_Func, proc->getReturnType(), false), proc(proc),
        params(params) {}

  ProcedureDeclaration *geDecl() { return proc; }
  const ExprList &getParams() { return params; }

  static bool classof(const Expr *E) { return E->getKind() == EK_Func; }

private:
  ProcedureDeclaration *proc;
  ExprList params;
};

class Stmt {
public:
  enum StmtKind { SK_Assign, SK_ProcCall, SK_If, SK_While, SK_Return };

  StmtKind getKind() const { return kind; }

protected:
  Stmt(StmtKind kind) : kind(kind) {}

private:
  const StmtKind kind;
};

class AssignmentStatement : public Stmt {
public:
  AssignmentStatement(Decl *var, Expr *e) : Stmt(SK_Assign), var(var), e(e) {}

  Decl *getVar() { return var; }
  Expr *getExpr() { return e; }

  static bool classof(const Stmt *S) { return S->getKind() == SK_Assign; }

private:
  Decl *var;
  Expr *e;
};

class ProcedureCallStatement : public Stmt {
public:
  ProcedureCallStatement(ProcedureDeclaration *proc, ExprList &params)
      : Stmt(SK_ProcCall), proc(proc), params(params) {}

  ProcedureDeclaration *getProc() { return proc; }
  const ExprList &getParams() { return params; }

  static bool classof(const Stmt *S) { return S->getKind() == SK_ProcCall; }

private:
  ProcedureDeclaration *proc;
  ExprList params;
};

class IfStatement : public Stmt {
public:
  IfStatement(Expr *Cond, StmtList &ifStmts, StmtList &elseStmts)
      : Stmt(SK_If), cond(Cond), ifStmts(ifStmts), elseStmts(elseStmts) {}

  Expr *getCond() { return cond; }
  const StmtList &getIfStmts() { return ifStmts; }
  const StmtList &getElseStmts() { return elseStmts; }

  static bool classof(const Stmt *S) { return S->getKind() == SK_If; }

private:
  Expr *cond;
  StmtList ifStmts;
  StmtList elseStmts;
};

class WhileStatement : public Stmt {
public:
  WhileStatement(Expr *cond, StmtList &stmts)
      : Stmt(SK_While), cond(cond), stmts(stmts) {}

  Expr *getCond() { return cond; }
  const StmtList &getWhileStmts() { return stmts; }

  static bool classof(const Stmt *S) { return S->getKind() == SK_While; }

private:
  Expr *cond;
  StmtList stmts;
};

class ReturnStatement : public Stmt {
public:
  ReturnStatement(Expr *retVal) : Stmt(SK_Return), retVal(retVal) {}

  Expr *getRetVal() { return retVal; }

  static bool classof(const Stmt *S) { return S->getKind() == SK_Return; }

private:
  Expr *retVal;
};

} // namespace tinylang

#endif // TINYLANG_AST_AST_H
