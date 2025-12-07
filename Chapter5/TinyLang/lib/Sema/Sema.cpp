#include "tinylang/Sema/Sema.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Sema/Scope.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

using namespace tinylang;

void Sema::enterScope(Decl *decl) {
  currScope = new Scope(currScope);
  currDecl = decl;
}

void Sema::leaveScope() {
  assert(currScope && "Can't leave non-existing scope");
  assert(currDecl && "Can't leave non-existing decl");
  Scope *parent = currScope->getParent();
  delete currScope;
  currScope = parent;
  currDecl = currDecl->getEnclosingDecl();
}

bool Sema::isOperatorForType(tok::TokenKind op, TypeDeclaration *type) {
  switch (op) {
    case tok::TokenKind::plus:
    case tok::TokenKind::minus:
    case tok::TokenKind::star:
    case tok::TokenKind::KW_DIV:
    case tok::TokenKind::KW_MOD:
      return type == intType;
    case tok::TokenKind::slash:
      return false;
    case tok::TokenKind::KW_AND:
    case tok::TokenKind::KW_OR:
    case tok::TokenKind::KW_NOT:
      return type == boolType;
    default:
      llvm_unreachable("Unknown operator");
  }
}

void Sema::checkFormalAndActualParameters(SMLoc loc,
                                          const FormalParamList &formals,
                                          const ExprList &actuals) {
  if (formals.size() != actuals.size()) {
    diagEngine.report(loc, diag::err_wrong_number_of_parameters);
    return;
  }

  for (const auto &[formal, actual] : llvm::zip(formals, actuals)) {
    if (formal->getType() != actual->getType()) {
      diagEngine.report(
          loc, diag::err_type_of_formal_and_actual_parameter_not_compatible);
    }
    if (formal->isVar() && isa<VariableAccess>(actual)) {
      diagEngine.report(loc, diag::err_var_parameter_requires_var);
    }
  }
}

void Sema::initialize() {
  // Setup global scope.
  currScope = new Scope();
  currDecl = nullptr;
  intType = new TypeDeclaration(currDecl, SMLoc(), "INTEGER");
  boolType = new TypeDeclaration(currDecl, SMLoc(), "BOOLEAN");
  trueLiteral = new BooleanLiteral(true, boolType);
  falseLiteral = new BooleanLiteral(false, boolType);
  trueConst = new ConstantDeclaration(currDecl, SMLoc(), "TRUE", trueLiteral);
  falseConst =
      new ConstantDeclaration(currDecl, SMLoc(), "FALSE", falseLiteral);
  currScope->insert(intType);
  currScope->insert(boolType);
  currScope->insert(trueConst);
  currScope->insert(falseConst);
}

ModuleDeclaration *Sema::actOnModuleDeclaration(SMLoc loc, StringRef name) {
  return new ModuleDeclaration(currDecl, loc, name);
}

void Sema::actOnModuleDeclaration(ModuleDeclaration *modDecl, SMLoc loc,
                                  StringRef name, DeclList &decls,
                                  StmtList &stmts) {
  if (name != modDecl->getName()) {
    diagEngine.report(loc, diag::err_module_identifier_not_equal);
    diagEngine.report(modDecl->getLocation(),
                      diag::note_module_identifier_declaration);
  }
  modDecl->setDecls(decls);
  modDecl->setStmts(stmts);
}

void Sema::actOnImport(StringRef moduleName [[maybe_unused]],
                       IdentList &ids [[maybe_unused]]) {
  diagEngine.report(SMLoc(), diag::err_not_yet_implemented);
}

void Sema::actOnConstantDeclaration(DeclList &decls, SMLoc loc, StringRef name,
                                    Expr *e) {
  assert(currScope && "CurrentScope not set");
  ConstantDeclaration *constDecl =
      new ConstantDeclaration(currDecl, loc, name, e);
  if (currScope->insert(constDecl)) {
    decls.push_back(constDecl);
  } else {
    diagEngine.report(loc, diag::err_symbold_declared);
  }
}

void Sema::actOnVariableDeclaration(DeclList &decls, IdentList &ids, Decl *d) {
  assert(currScope && "CurrentScope not set");
  if (auto type = dyn_cast_or_null<TypeDeclaration>(d)) {
    for (auto &[loc, name] : ids) {
      auto *decl = new VariableDeclaration(currDecl, loc, name, type);
      if (currScope->insert(decl)) {
        decls.push_back(decl);
      } else {
        diagEngine.report(loc, diag::err_symbold_declared);
      }
    }
    return;
  }

  if (!ids.empty()) {
    SMLoc loc = ids.front().first;
    diagEngine.report(loc, diag::err_vardecl_requires_type);
  }
}

void Sema::actOnFormalParameterDeclaration(FormalParamList &params,
                                           IdentList &ids, Decl *d,
                                           bool isVar) {
  assert(currScope && "CurrentScope not set");
  if (auto *type = dyn_cast_or_null<TypeDeclaration>(d)) {
    for (auto &[loc, name] : ids) {
      auto *decl =
          new FormalParameterDeclaration(currDecl, loc, name, type, isVar);
      if (currScope->insert(decl)) {
        params.push_back(decl);
      } else {
        diagEngine.report(loc, diag::err_symbold_declared);
      }
    }

    return;
  }

  if (!ids.empty()) {
    SMLoc loc = ids.front().first;
    diagEngine.report(loc, diag::err_vardecl_requires_type);
  }
}

ProcedureDeclaration *Sema::actOnProcedureDeclaration(SMLoc loc,
                                                      StringRef name) {
  ProcedureDeclaration *proc = new ProcedureDeclaration(currDecl, loc, name);
  if (!currScope->insert(proc)) {
    diagEngine.report(loc, diag::err_symbold_declared);
  }
  return proc;
}

void Sema::actOnProcedureHeading(ProcedureDeclaration *procDecl,
                                 FormalParamList &params, Decl *retType) {
  procDecl->setFormalParams(params);
  auto *retTypeDecl = dyn_cast_or_null<TypeDeclaration>(retType);
  if (!retTypeDecl && retType) {
    diagEngine.report(retType->getLocation(),
                      diag::err_returntype_must_be_type);
  } else {
    procDecl->setReturnType(retTypeDecl);
  }
}

void Sema::actOnProcedureDeclaration(ProcedureDeclaration *procDecl, SMLoc loc,
                                     StringRef name, DeclList &decls,
                                     StmtList &stmts) {
  if (name != procDecl->getName()) {
    diagEngine.report(loc, diag::err_proc_identifier_not_equal);
    diagEngine.report(procDecl->getLocation(),
                      diag::note_proc_identifier_declaration);
  } else {
    procDecl->setDecls(decls);
    procDecl->setStmts(stmts);
  }
  return;
}

void Sema::actOnAssignment(StmtList &stmts, SMLoc loc, Decl *d, Expr *e) {
  if (auto var = dyn_cast_or_null<VariableDeclaration>(d)) {
    if (var->getType() != e->getType()) {
      diagEngine.report(loc, diag::err_types_for_operator_not_compatible);
    } else {
      stmts.push_back(new AssignmentStatement(var, e));
    }
  } else if (auto *fp = dyn_cast_or_null<FormalParameterDeclaration>(d)) {
    if (fp->getType() != e->getType()) {
      diagEngine.report(loc, diag::err_types_for_operator_not_compatible,
                        tok::getPunctuatorSpelling(tok::TokenKind::colonequal));
    } else {
      stmts.push_back(new AssignmentStatement(fp, e));
    }
  } else if (d) {
    diagEngine.report(loc, diag::err_not_yet_implemented, "other assignments");
  }
}

void Sema::actOnProcCall(StmtList &stmts, SMLoc loc, Decl *d,
                         ExprList &params) {
  if (auto proc = dyn_cast_or_null<ProcedureDeclaration>(d)) {
    checkFormalAndActualParameters(loc, proc->getFormalParams(), params);
    stmts.push_back(new ProcedureCallStatement(proc, params));
    return;
  }
  if (d) {
    diagEngine.report(loc, diag::err_procedure_call_on_nonprocedure);
  }
}

void Sema::actOnIfStatement(StmtList &stmts, SMLoc loc, Expr *cond,
                            StmtList &ifStmts, StmtList &elseStmts) {
  assert(cond && "Condition expression is null");
  if (cond->getType() != boolType) {
    diagEngine.report(loc, diag::err_if_expr_must_be_bool);
  }
  stmts.push_back(new IfStatement(cond, ifStmts, elseStmts));
}

void Sema::actOnWhileStatement(StmtList &stmts, SMLoc loc, Expr *cond,
                               StmtList &whileStmts) {
  assert(cond && "Condition expression is null");
  if (cond->getType() != boolType) {
    diagEngine.report(loc, diag::err_while_expr_must_be_bool);
  }
  stmts.push_back(new WhileStatement(cond, whileStmts));
}

void Sema::actOnReturnStatement(StmtList &stmts, SMLoc loc, Expr *retVal) {
  assert(currDecl && "Current declaration is null");
  if (auto *proc = dyn_cast<ProcedureDeclaration>(currDecl)) {
    if (proc->getReturnType() && !retVal) {
      diagEngine.report(loc, diag::err_function_requires_return);
    } else if (!proc->getReturnType() && retVal) {
      diagEngine.report(loc, diag::err_procedure_requires_empty_return);
    } else if (proc->getReturnType() && retVal) {
      if (proc->getReturnType() != retVal->getType()) {
        diagEngine.report(loc, diag::err_function_and_return_type);
      }
    }
    stmts.push_back(new ReturnStatement(retVal));
  }
}

Expr *Sema::actOnExpression(Expr *left, Expr *right, const OperatorInfo &op) {
  if (!left) {
    return right;
  }
  if (!right) {
    return left;
  }

  if (left->getType() != right->getType()) {
    diagEngine.report(op.getLocation(),
                      diag::err_types_for_operator_not_compatible,
                      tok::getPunctuatorSpelling(op.getKind()));
  }
  bool isConst = left->isConstant() && right->isConstant();
  return new InfixExpression(left, right, op, boolType, isConst);
}

Expr *Sema::actOnSimpleExpression(Expr *left, Expr *right,
                                  const OperatorInfo &op) {
  if (!left) {
    return right;
  }
  if (!right) {
    return left;
  }
  if (left->getType() != right->getType() ||
      !isOperatorForType(op.getKind(), left->getType())) {
    diagEngine.report(op.getLocation(),
                      diag::err_types_for_operator_not_compatible,
                      tok::getPunctuatorSpelling(op.getKind()));
  }
  TypeDeclaration *type = left->getType();
  bool isConst = left->isConstant() && right->isConstant();
  if (isConst && op.getKind() == tok::KW_OR) {
    auto *lhs = dyn_cast<BooleanLiteral>(left);
    auto *rhs = dyn_cast<BooleanLiteral>(right);
    return lhs->getValue() || rhs->getValue() ? trueLiteral : falseLiteral;
  }
  return new InfixExpression(left, right, op, type, isConst);
}

Expr *Sema::actOnTerm(Expr *left, Expr *right, const OperatorInfo &op) {
  if (!left) {
    return right;
  }
  if (!right) {
    return left;
  }

  if (left->getType() != right->getType() ||
      !isOperatorForType(op.getKind(), left->getType())) {
    diagEngine.report(op.getLocation(),
                      diag::err_types_for_operator_not_compatible,
                      tok::getPunctuatorSpelling(op.getKind()));
  }
  TypeDeclaration *type = left->getType();
  bool isConst = left->isConstant() && right->isConstant();
  if (isConst && op.getKind() == tok::TokenKind::KW_AND) {
    auto *lhs = dyn_cast<BooleanLiteral>(left);
    auto *rhs = dyn_cast<BooleanLiteral>(right);
    return lhs->getValue() && rhs->getValue() ? trueLiteral : falseLiteral;
  }
  return new InfixExpression(left, right, op, type, isConst);
}

Expr *Sema::actOnPrefixExpression(Expr *e, const OperatorInfo &op) {
  if (!e) {
    return nullptr;
  }
  if (!isOperatorForType(op.getKind(), e->getType())) {
    diagEngine.report(op.getLocation(),
                      diag::err_types_for_operator_not_compatible);
  }

  if (e->isConstant() && op.getKind() == tok::TokenKind::KW_NOT) {
    BooleanLiteral *boolLit = dyn_cast<BooleanLiteral>(e);
    return boolLit->getValue() ? falseLiteral : trueLiteral;
  }

  if (op.getKind() == tok::TokenKind::minus) {
    bool ambiguous = true;
    if (isa<IntegerLiteral, VariableAccess, ConstantAccess>(e)) {
      ambiguous = false;
    } else if (auto *infix = dyn_cast<InfixExpression>(e)) {
      auto kind = infix->getOperatorInfo().getKind();
      if (kind == tok::TokenKind::star || kind == tok::TokenKind::slash) {
        ambiguous = false;
      }
    }
    if (ambiguous) {
      diagEngine.report(op.getLocation(), diag::warn_ambigous_negation);
    }
  }

  return new PrefixExpression(e, op, e->getType(), e->isConstant());
}

Expr *Sema::actOnIntegerLiteral(SMLoc loc, StringRef literal) {
  uint8_t radix = 10;
  if (literal.ends_with("H")) {
    literal = literal.drop_back();
    radix = 16;
  }

  llvm::APInt value(64, literal, radix);
  return new IntegerLiteral(loc, llvm::APSInt(value, false), intType);
}

Expr *Sema::actOnVariable(Decl *d) {
  if (!d) {
    return nullptr;
  }

  if (auto *varDecl = dyn_cast<VariableDeclaration>(d)) {
    return new VariableAccess(varDecl);
  }

  if (auto *paramDecl = dyn_cast<FormalParameterDeclaration>(d)) {
    return new VariableAccess(paramDecl);
  }

  if (auto *constDecl = dyn_cast<ConstantDeclaration>(d)) {
    if (constDecl == trueConst) {
      return trueLiteral;
    }
    if (constDecl == falseConst) {
      return falseLiteral;
    }
    return new ConstantAccess(constDecl);
  }

  return nullptr;
}

Expr *Sema::actOnFunctionCall(Decl *d, ExprList &params) {
  if (!d) {
    return nullptr;
  }

  if (!isa<ProcedureDeclaration>(d)) {
    diagEngine.report(d->getLocation(), diag::err_function_call_on_nonfunction);
    return nullptr;
  }

  auto *procDecl = cast<ProcedureDeclaration>(d);
  checkFormalAndActualParameters(procDecl->getLocation(),
                                 procDecl->getFormalParams(), params);
  if (!procDecl->getReturnType()) {
    diagEngine.report(procDecl->getLocation(),
                      diag::err_function_call_on_nonfunction);
    return nullptr;
  }
  return new FunctionCallExpr(procDecl, params);
}

Decl *Sema::actOnQualIdentPart(Decl *prev, SMLoc loc, StringRef name) {
  if (!prev) {
    if (auto *decl = currScope->lookup(name)) {
      return decl;
    }
    diagEngine.report(loc, diag::err_undeclared_name, name);
    return nullptr;
  }

  if (auto *moduleDecl = dyn_cast<ModuleDeclaration>(prev)) {
    for (auto *decl : moduleDecl->getDecls()) {
      if (decl->getName() == name) {
        return decl;
      }
    }
  }

  diagEngine.report(loc, diag::err_undeclared_name, name);
  return nullptr;
}
