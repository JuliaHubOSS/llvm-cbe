##===- projects/sample/tools/sample/Makefile ---------------*- Makefile -*-===##

#
# Indicate where we are relative to the top of the source tree.
#
LEVEL=../..

#
# Give the name of the tool.
#
TOOLNAME=llvm-cbe

#
# List libraries that we'll need
# We use LIBS because sample is a dynamic library.
#
USEDLIBS = LLVMCBackendCodeGen.a LLVMCBackendInfo.a
LINK_COMPONENTS = all-targets bitreader asmparser irreader


#
# Include Makefile.common so we know what to do.
#
include $(LEVEL)/Makefile.common

