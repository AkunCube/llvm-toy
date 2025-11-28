add_executable(calc Calc.cpp)
target_link_libraries(calc PRIVATE ${llvm_libs})