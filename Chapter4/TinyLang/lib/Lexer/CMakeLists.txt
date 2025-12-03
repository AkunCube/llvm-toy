add_library(TinyLangLexer
  Lexer.cpp
)

target_link_libraries(TinyLangLexer PRIVATE
  ${llvm_libs}
  TinyLangBasic
)
