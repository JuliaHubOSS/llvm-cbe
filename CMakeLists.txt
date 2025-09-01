if (NOT DEFINED LLVM_VERSION_MAJOR)
  project(llvm-cbe)
  set (USE_SYSTEM_LLVM 1)
  cmake_minimum_required(VERSION 3.20.0)
  find_package(LLVM 20.1 REQUIRED CONFIG)
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})

  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
  include(AddLLVM)

  set (CMAKE_CXX_STANDARD 17)
endif()

add_subdirectory(lib)
add_subdirectory(tools)

if( LLVM_INCLUDE_TESTS )
  add_subdirectory(unittests)
endif()
