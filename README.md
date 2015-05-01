llvm-cbe
========

resurrected LLVM "C Backend", with improvements


INSTALLATION INSTRUCTIONS
=========================

This version of the LLVM-CBE library works with LLVM 3.5.0. You will have to
compile this version of LLVM before you try to use LLVM-CBE. This
guide will walk you through the compilation and installation of both
tools and show usage statements to verify that the LLVM-CBE library is
compiled correctly.

The library is known to compile on various linux versions (Redhat,
Mageia, Ubuntu, Debian) and Mac OS X.

Step 1: Installing LLVM
=======================

LLVM-CBE relies on specific LLVM internals, and so it is best to use
it with a specific revision of the LLVM development tree. Currently,
llvm-cbe works with the LLVM 3.5.0 release version.

Note: to convert C to LLVM IR to run the tests, you will also need the clang compiler.

As llvm-cbe is a prototype library, we only use it with debugging
enabled. A sample of commands to do that is as follows:

     cd $HOME
     git clone https://github.com/llvm-mirror/llvm
     cd llvm
     git checkout release_35
     ./configure
     make ENABLE_OPTIMIZED=0 DEBUG_SYMBOLS=1

Step 2: Compiling LLVM-CBE
==========================

Sample commands to download and compile llvm-cbe are:

    cd $HOME/llvm/projects
    git clone https://github.com/vtjnash/llvm-cbe.git llvm-cbe
    cd ..
    make ENABLE_OPTIMIZED=0 DEBUG_SYMBOLS=1

Step 3: Usage Examples
======================

If llvm-cbe compiles, you should be able to run it with the following commands.
```
$ cd llvm/lib/test/selectionsort
$ ls
main.c
$ clang -S -emit-llvm main.c
$ ls
main.c main.ll
$ $(HOME)/llvm/Debug+Asserts/bin/llvm-cbe main.ll

```
Compile Generated C-Code and Run
================================
```
$ gcc -o main.cbe main.cbe.c
$ ls
main.c  main.cbe  main.cbe.c  main.ll
$ ./main.cbe
```
