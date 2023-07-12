add_llvm_component_group(CBackend)

add_subdirectory(TargetInfo)

add_llvm_target(CBackendCodeGen
  CBackend.cpp
  CTargetMachine.cpp
  TopologicalSorter.cpp

  LINK_COMPONENTS
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

  DEPENDS
  intrinsics_gen

  ADD_TO_COMPONENT
  CBackend
  )
