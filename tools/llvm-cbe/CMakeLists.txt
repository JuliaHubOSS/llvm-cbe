# Support plugins.
set(LLVM_NO_DEAD_STRIP 1)

set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Analysis
  AsmParser
  AsmPrinter
  BitReader
  CBackendCodeGen
  CBackendInfo
  CodeGen
  Core
  IRReader
  MC
  ScalarOpts
  SelectionDAG
  Support
  Target
  )


add_llvm_tool(llvm-cbe
  llvm-cbe.cpp
  )
export_executable_symbols(llvm-cbe)
