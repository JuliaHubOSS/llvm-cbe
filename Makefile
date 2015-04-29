##===- projects/sample/lib/Makefile ------------------------*- Makefile -*-===##

#
# Relative path to the top of the source tree.
#
LEVEL := ../..
include $(LEVEL)/Makefile.config

#
# List all of the subdirectories that we will compile.
#
DIRS := lib tools

include $(PROJ_SRC_ROOT)/Makefile.rules
