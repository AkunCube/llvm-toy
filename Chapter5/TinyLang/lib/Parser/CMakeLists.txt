set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(TinyLangParser
  Parser.cpp

  LINK_LIBS
  TinyLangBasic
  TinyLangLexer
)
