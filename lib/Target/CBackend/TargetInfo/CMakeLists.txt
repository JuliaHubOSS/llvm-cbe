include_directories( ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/.. )

set(LLVM_LINK_COMPONENTS
  MC
  ScalarOpts
  Support
  Target
  )

add_llvm_library(LLVMCBackendInfo
  CBackendTargetInfo.cpp
  )
