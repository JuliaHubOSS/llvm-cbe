if (NOT DEFINED LLVM_VERSION_MAJOR)
  project(llvm-cbe)
  set (USE_SYSTEM_LLVM 1)
  cmake_minimum_required(VERSION 3.4.3)
  find_package(LLVM REQUIRED CONFIG)
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})

  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
  include(AddLLVM)

  set (CMAKE_CXX_STANDARD 14)
endif()

if(LLVM_VERSION_MAJOR LESS 8)
  message(FATAL_ERROR "This project isn't buldable with LLVM < 8.0.0.")
elseif(LLVM_VERSION_MAJOR LESS 10)
  message(WARNING "Building with LLVM < 10.0.0 is at your own risk.")
endif()

add_subdirectory(lib)
add_subdirectory(tools)

if( LLVM_INCLUDE_TESTS )
  add_subdirectory(unittests)
endif()
