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

IDIR = ./cbetest
CC = gcc
FLAGS = -Wall
FILES = testbad test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 test025 test026 test027 test028 test029 test030 test031 test032 test033 test034 test035 test036 test037 test038 test039 test040 test041 test042 test043 test044 test045 test046 test047 test048 test049 test050 test051 test052 test053 test054 test055 test056 test057 test058 test059 test060 test061 test062 test063 test064 test065 test066 test067 test068 test069 test070 test071 test072 test073 test074 test075 test076 test077 test078 test079 test080 test081 test082 test083 test084 test085 test086 test087 test088 test089 test090 test091 test092 test093 test094 test095 test096 test097 test098 test099 test100 test101 test102 test103

LLFILES = $(shell find . -name "*.ll")

all: clean cexec tollvm tocbe buildcbe

run-c: clean cexec 
optO0: clean tollvmO0 tocbe buildcbeO0
optO1: clean tollvmO1 tocbe buildcbeO1
optO2: clean tollvmO2 tocbe buildcbeO2
optO3: clean tollvmO3 tocbe buildcbeO3

.PHONY: tollvm tocbe copy buildcbe 

#Make each level of Optimization in one call
tollvm: 
	for name in $(FILES); do\
		clang -m32 $$name.c -emit-llvm -S -o $$name.ll;\
                clang -m32 $$name.c -emit-llvm -O1 -S -o $$name"_O1.ll";\
                clang -m32 $$name.c -emit-llvm -O2 -S -o $$name"_O2.ll";\
                clang -m32 $$name.c -emit-llvm -O3 -S -o $$name"_O3.ll";\
	done 

tocbe: 
	for name in $(LLFILES); do\
		llvm-cbe $$name ; \
	done 

buildcbe: 
	for name in $(FILES); do\
               $(CC) $$name.cbe.c -o $$name.cbe;\
	       $(CC) $$name"_O1.cbe.c" -o $$name"_O1.cbe";\
               $(CC) $$name"_O2.cbe.c" -o $$name"_O2.cbe";\
               $(CC) $$name"_O3.cbe.c" -o $$name"_O3.cbe";\
	done 

#Make C-Based Executables
cexec: 
	for name in $(FILES); do\
		gcc -o $$name".cexe" $$name".c";\
	done

#Make only non-optimized LLVM IR
tollvmO0: 
	for name in $(FILES); do\
                clang -m32 $$name.c -emit-llvm -S -o $$name.ll;\
	done 

buildcbeO0: 
	for name in $(FILES); do\
	       $(CC) $$name.cbe.c -o $$name.cbe;\
	done 

#Make only level "O1" optimized LLVM IR 
tollvmO1: 
	for name in $(FILES); do\
                clang -m32 $$name.c -emit-llvm -O1 -S -o $$name"_O1.ll";\
	done 

buildcbeO1: 
	for name in $(FILES); do\
	       $(CC) $$name"_O1.cbe.c" -o $$name"_O1.cbe";\
	done 

#Make only level "O2" optimized LLVM IR
tollvmO2: 
	for name in $(FILES); do\
                clang -m32 $$name.c -emit-llvm -O2 -S -o $$name"_O2.ll";\
	done 

buildcbeO2: 
	for name in $(FILES); do\
               $(CC) $$name"_O2.cbe.c" -o $$name"_O2.cbe";\
	done 
	
#Make only level "O3" optimized LLVM IR
tollvmO3: 
	for name in $(FILES); do\
                clang -m32 $$name.c -emit-llvm -O3 -S -o $$name"_O3.ll";\
	done 

buildcbeO3: 
	for name in $(FILES); do\
               $(CC) $$name"_O3.cbe.c" -o $$name"_O3.cbe";\
	done 

#Remove all .ll, .cbe.c, and .cbe files
clean:
	rm -rf *.ll 
	rm -rf *.cbe
	rm -rf *.cexe
	rm -rf *.cbe.c

#Attempt to Automatically find '.c' input files, works every other run
#FULLFILE := $(filter-out cbe.c,$(wildcard *.c))
#FILES = $(basename $(FULLFILE))
