#include "tinylang/Sema/Scope.h"
#include "tinylang/AST/AST.h"

using namespace tinylang;

bool Scope::insert(Decl *decl) {
  return symbols.insert(std::make_pair(decl->getName(), decl)).second;
}

Decl *Scope::lookup(StringRef name) {
  Scope *current = this;
  while (current) {
    auto it = current->symbols.find(name);
    if (it != current->symbols.end()) {
      return it->second;
    }
    current = current->parent;
  }
  return nullptr;
}
