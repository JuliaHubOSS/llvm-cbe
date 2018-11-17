add_custom_target(CBEUnitTests)
set_target_properties(CBEUnitTests PROPERTIES FOLDER "Tests")

set(LLVM_LINK_COMPONENTS
  Analysis
  CBackendCodeGen
  CBackendInfo
  CodeGen
  Core
  MC
  ScalarOpts
  Support
  Target
  TransformUtils
  )

add_unittest(CBEUnitTests CWriterTest
  TypeNameTest.cpp
  )
