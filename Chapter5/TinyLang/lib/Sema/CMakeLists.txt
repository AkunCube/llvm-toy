set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(TinyLangSema
  Scope.cpp
  Sema.cpp

  LINK_LIBS
  TinyLangBasic
)
