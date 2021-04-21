add_custom_target(CBEUnitTests)
set_target_properties(CBEUnitTests PROPERTIES FOLDER "Tests")

if (NOT USE_SYSTEM_LLVM)
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
endif()

add_unittest(CBEUnitTests CWriterTest
  TypeNameTest.cpp
  )

if(USE_SYSTEM_LLVM)
  llvm_map_components_to_libnames(llvm_libs
    CBackendCodeGen
    CBackendInfo
    )
  target_link_libraries(CWriterTest PRIVATE LLVM ${llvm_libs})
endif()
