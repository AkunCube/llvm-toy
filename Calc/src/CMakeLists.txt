add_executable(calc
  Calc.cpp
  Lexer.cpp
  Parser.cpp
)
target_link_libraries(calc PRIVATE ${llvm_libs})
