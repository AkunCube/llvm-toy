add_library(TinyLangCodeGen
  CGModule.cpp
  CGProcedure.cpp
)

target_link_libraries(TinyLangCodeGen PRIVATE
  ${llvm_libs}
  TinyLangSema
)
