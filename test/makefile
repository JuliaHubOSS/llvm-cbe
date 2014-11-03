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
FILES = testbad test001 test002 test003 test004 test005 test006 test007 test008 test009 test010  test011 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 test025 test026 test027 test028 test029 test030 test031 test032 test033 test034 test035 test036 test037 test038 test039 test040 test041 test042 test043 test044 test045 test046 test047 test048 test049 test050 test051 test052 test053 test054 test055 test056 test057 test058 test059 test060 test061 test062 test063 test064 test065 test066 test067 test068 test069 test070 test071 test072 test073 test074 test075 test076 test077 test078 test079 test080 test081 test082 test083 test084 test085 test086 test087 test088 test089 test090 test091 test092 test093 test094 test095 test096 test097 test098 test099 test100 test101 test102 test103


#Attempt to Automatically find '.c' input files, works every other run
#FULLFILE := $(filter-out cbe.c,$(wildcard *.c))
#FILES = $(basename $(FULLFILE))

LLFILES = $(shell find . -name "*.ll")


all: clean tollvm tocbe buildcbe

.PHONY: tollvm tocbe copy buildcbe 

tollvm: 
	for name in $(FILES); do\
		clang -m32 $$name.c -emit-llvm -S -o $$name.ll;\
	done 
		
tocbe: 
	for name in $(LLFILES); do\
		llvm-cbe $$name ; \
	done 

buildcbe: 
	for name in $(FILES); do\
	       $(CC) $$name.cbe.c -o $$name.cbe;\
	done 

clean:
	rm -rf *.ll 
	rm -rf *.cbe
	rm -rf *.cbe.c
