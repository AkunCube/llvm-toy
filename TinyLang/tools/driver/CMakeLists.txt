add_executable(tinylang
  Driver.cpp
)
target_link_libraries(tinylang PRIVATE
  ${llvm_libs}
  TinyLangBasic
)
