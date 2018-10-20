#===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
#===----------------------------------------------------------------------===//
#
# Makefile for CBE test cases. Currently uses manual file identification.
#
#===----------------------------------------------------------------------===//

LLVMToolDir ?= ../../../build/bin

PYTHON ?= python
GCC ?= gcc
CLANG ?= clang
CPPFLAGS = -Iinclude/ -Wall -Wno-unused-function -Wno-unused-variable -Wno-error=attributes -Wno-attributes -Wno-main -Wno-error=main -Werror
GCCFLAGS = $(CPPFLAGS) -Wno-error=unused-but-set-variable -Wno-unused-but-set-variable
CLANGFLAGS= $(CPPFLAGS) -Wno-error=unused-variable -Wno-unused-variable
FILES := $(patsubst %.c,%,$(filter-out %.cbe.c,$(wildcard *.c)))

all: cexec buildcbe

.PHONY: clean cexec buildcbe buildcbe* tollvm tollvm* all
.SECONDARY:

#Make each level of Optimization in one call
tollvm: tollvmO0 tollvmO1 tollvmO2 tollvmO3
tollvmO0: $(addsuffix _O0.ll,$(FILES))
tollvmO1: $(addsuffix _O1.ll,$(FILES))
tollvmO2: $(addsuffix _O2.ll,$(FILES))
tollvmO3: $(addsuffix _O3.ll,$(FILES))
%_O0.ll: %.c
	$(CLANG) $(CLANGFLAGS) $< -emit-llvm -S -O0 -o $@
%_O1.ll: %.c
	$(CLANG) $(CLANGFLAGS) $< -emit-llvm -S -O1 -o $@
%_O2.ll: %.c
	$(CLANG) $(CLANGFLAGS) $< -emit-llvm -S -O2 -o $@
%_O3.ll: %.c
	$(CLANG) $(CLANGFLAGS) $< -emit-llvm -S -O3 -o $@

tocbe: tocbeO0 tocbeO1 tocbeO2 tocbeO3
tocbeO%: tollvmO%
	rm -rf *_O$*.cbe.c
	for name in $(FILES); do\
		$(LLVMToolDir)/llvm-cbe $${name}_O$*.ll ; \
	done

%_O0.cbe.c: tocbeO0
	@echo built: $@
%_O1.cbe.c: tocbeO1
	@echo built: $@
%_O2.cbe.c: tocbeO2
	@echo built: $@
%_O3.cbe.c: tocbeO3
	@echo built: $@

buildcbe: buildcbeO0 buildcbeO1 buildcbeO2 buildcbeO3
buildcbeO0 : $(addsuffix _O0.cbe,$(FILES))
buildcbeO1 : $(addsuffix _O1.cbe,$(FILES))
buildcbeO2 : $(addsuffix _O2.cbe,$(FILES))
buildcbeO3 : $(addsuffix _O3.cbe,$(FILES))
%.cbe : %.cbe.c
	$(GCC) $(GCCFLAGS) -o $@ -O$(shell echo $* | sed 's/^.*_O\([0-9]\)$$/\1/') $<

#Make C-Based Executables
cexec: $(addsuffix .cexe,$(FILES))
%.cexe: %.c
	$(GCC) $(GCCFLAGS) -o $@ $<

test: all
	LLVMToolDir=$(LLVMToolDir) $(PYTHON) testFile.py

#Remove all .ll, .cbe.c, and .cbe files
clean:
	rm -rf *.ll
	rm -rf *.cbe
	rm -rf *.cexe
	rm -rf *.cbe.c
