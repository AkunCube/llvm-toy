#ifndef TINYLANG_SEMA_SCOPE_H
#define TINYLANG_SEMA_SCOPE_H

#include "tinylang/Basic/LLVM.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace tinylang {
class Decl;

class Scope {
public:
  Scope(Scope *parent = nullptr) : parent(parent) {}
  bool insert(Decl *decl);
  Decl *lookup(StringRef name);
  Scope *getParent() { return parent; }

private:
  Scope *parent;
  StringMap<Decl *> symbols;
};

} // namespace tinylang

#endif // TINYLANG_SEMA_SCOPE_H
