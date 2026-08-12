IF (NOT DEFINED LLVM_VERSION_MAJOR)
  # Standalone build: requires both LLVM and Clang CMake packages
  project(CBackendPlugin)
  cmake_minimum_required(VERSION 3.20.0)
  find_package(LLVM 20.1 REQUIRED CONFIG)
  find_package(Clang REQUIRED CONFIG)
  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
  include(AddLLVM)
  set(CMAKE_CXX_STANDARD 17)
  include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})
else()
  find_package(Clang REQUIRED CONFIG)
  include_directories(${CLANG_INCLUDE_DIRS})
endif()

# Internal Clang CodeGen headers (ABIInfo.h, TargetInfo.h, etc.) are not
# exported; derive path from the LLVM source include directory.
get_filename_component(_llvm_src "${LLVM_MAIN_INCLUDE_DIR}" DIRECTORY)
include_directories(${_llvm_src}/../clang/lib/CodeGen)

add_llvm_library(CBackendPlugin MODULE
  CBackend.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangCodeGen
  LLVMSupport
)

# Plugins must not have undefined symbols
set_target_properties(CBackendPlugin PROPERTIES
  CXX_VISIBILITY_PRESET hidden
)
