add_executable(calc
  Calc.cpp
  Lexer.cpp
  Parser.cpp
  Sema.cpp
  CodeGen.cpp
)
target_link_libraries(calc PRIVATE ${llvm_libs})
