add_library(TinyLangSema
  Scope.cpp
  Sema.cpp
)

target_link_libraries(TinyLangSema PRIVATE
  ${llvm_libs}
  TinyLangBasic
)
