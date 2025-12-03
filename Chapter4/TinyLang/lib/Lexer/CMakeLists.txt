set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(TinyLangLexer
  Lexer.cpp

  LINK_LIBS
  TinyLangBasic
)
