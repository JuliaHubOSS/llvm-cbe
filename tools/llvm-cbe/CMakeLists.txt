# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)

set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  BitReader
  AsmParser
  IRReader
  CBackendInfo
  CBackendCodeGen
  )


add_llvm_tool(llvm-cbe
  llvm-cbe.cpp
  )
export_executable_symbols(llvm-cbe)
