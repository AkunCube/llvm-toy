set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(TinyLangCodeGen
  CGModule.cpp
  CGProcedure.cpp
  CodeGenerator.cpp

  LINK_LIBS
  TinyLangSema
)
