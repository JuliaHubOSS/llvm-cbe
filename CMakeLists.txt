add_subdirectory(lib)
add_subdirectory(tools)

if( LLVM_INCLUDE_TESTS )
  add_subdirectory(unittests)
endif()
