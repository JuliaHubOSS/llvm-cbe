#===-- CBackend.cpp - Library for converting LLVM code to C --------------===//
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
#===----------------------------------------------------------------------===//
#
# Makefile for CBE test case
#
#===----------------------------------------------------------------------===//

IDIR = ./cbetest
CC = gcc
FLAGS = -Wall
FILES = test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011
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
		gcc $$name.cbe.c -o $$name.cbe;\
	done 

clean:
	rm -rf *.ll 
	rm -rf *.cbe
	rm -rf *.cbe.c




	





