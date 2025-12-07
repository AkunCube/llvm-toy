#ifndef TINYLANG_SEMA_SEMA_H
#define TINYLANG_SEMA_SEMA_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Sema/Scope.h"
#include <memory>

namespace tinylang {
class Sema {
  friend class EnterDeclScope;

public:
  Sema(DiagnosticsEngine &diagEngine)
      : currScope(nullptr), currDecl(nullptr), diagEngine(diagEngine) {
    initialize();
  }

  void initialize();
  ModuleDeclaration *actOnModuleDeclaration(SMLoc loc, StringRef name);
  void actOnModuleDeclaration(ModuleDeclaration *modDecl, SMLoc loc,
                              StringRef name, DeclList &decls, StmtList &stmts);
  void actOnImport(StringRef moduleName, IdentList &ids);
  void actOnConstantDeclaration(DeclList &decls, SMLoc loc, StringRef name,
                                Expr *e);
  void actOnVariableDeclaration(DeclList &decls, IdentList &ids, Decl *d);
  void actOnFormalParameterDeclaration(FormalParamList &params, IdentList &ids,
                                       Decl *d, bool isVar);
  ProcedureDeclaration *actOnProcedureDeclaration(SMLoc loc, StringRef name);
  void actOnProcedureHeading(ProcedureDeclaration *procDecl,
                             FormalParamList &params, Decl *retType);
  void actOnProcedureDeclaration(ProcedureDeclaration *procDecl, SMLoc loc,
                                 StringRef name, DeclList &decls,
                                 StmtList &stmts);
  void actOnAssignment(StmtList &stmts, SMLoc loc, Decl *d, Expr *e);
  void actOnProcCall(StmtList &stmts, SMLoc loc, Decl *d, ExprList &params);
  void actOnIfStatement(StmtList &stmts, SMLoc loc, Expr *cond,
                        StmtList &ifStmts, StmtList &elseStmts);
  void actOnWhileStatement(StmtList &stmts, SMLoc loc, Expr *cond,
                           StmtList &whileStmts);
  void actOnReturnStatement(StmtList &stmts, SMLoc loc, Expr *retVal);

  Expr *actOnExpression(Expr *left, Expr *right, const OperatorInfo &op);
  Expr *actOnSimpleExpression(Expr *left, Expr *right, const OperatorInfo &op);
  Expr *actOnTerm(Expr *left, Expr *right, const OperatorInfo &op);
  Expr *actOnPrefixExpression(Expr *e, const OperatorInfo &op);
  Expr *actOnIntegerLiteral(SMLoc loc, StringRef literal);
  Expr *actOnVariable(Decl *d);
  Expr *actOnFunctionCall(Decl *d, ExprList &params);
  Decl *actOnQualIdentPart(Decl *prev, SMLoc loc, StringRef name);

private:
  Scope *currScope;
  Decl *currDecl;
  DiagnosticsEngine &diagEngine;

  TypeDeclaration *intType;
  TypeDeclaration *boolType;
  BooleanLiteral *trueLiteral;
  BooleanLiteral *falseLiteral;
  ConstantDeclaration *trueConst;
  ConstantDeclaration *falseConst;

  void enterScope(Decl *decl);
  void leaveScope();
  bool isOperatorForType(tok::TokenKind op, TypeDeclaration *type);
  void checkFormalAndActualParameters(SMLoc loc, const FormalParamList &formals,
                                      const ExprList &actuals);
};

class EnterDeclScope {
public:
  EnterDeclScope(Sema &sema, Decl *decl) : sema(sema) { sema.enterScope(decl); }
  ~EnterDeclScope() { sema.leaveScope(); }

private:
  Sema &sema;
};

} // namespace tinylang
#endif // TINYLANG_SEMA_SEMA_H
