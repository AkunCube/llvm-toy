add_library(TinyLangParser
  Parser.cpp
)

target_link_libraries(TinyLangParser PRIVATE
  ${llvm_libs}
  TinyLangBasic
  TinyLangLexer
)
