add_subdirectory(TargetInfo)

set(LLVM_LINK_COMPONENTS
  Analysis
  CBackendInfo
  CodeGen
  Core
  MC
  ScalarOpts
  Support
  Target
  SelectionDAG
  TransformUtils
  )

add_llvm_target(CBackendCodeGen
  CBackend.cpp
  CTargetMachine.cpp
  TopologicalSorter.cpp
  TargetInfo/CBackendTargetInfo.cpp
  )
