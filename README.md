llvm-cbe
========

resurrected LLVM "C Backend", with improvements


INSTALLATION INSTRUCTIONS
=========================

This version of the LLVM-CBE library works with LLVM 6.0 and 7.0. You will have
to compile this version of LLVM before you try to use LLVM-CBE. This
guide will walk you through the compilation and installation of both
tools and show usage statements to verify that the LLVM-CBE library is
compiled correctly.

The library is known to compile on various Linux versions (Redhat,
Mageia, Ubuntu, Debian), Mac OS X, and Windows (Mingw-w64).

Step 1: Installing LLVM
=======================

LLVM-CBE relies on specific LLVM internals, and so it is best to use
it with a specific revision of the LLVM development tree. Currently,
llvm-cbe works with the LLVM 6.0 and 7.0 release versions and autotools.

Note: to convert C to LLVM IR to run the tests, you will also need a C compiler such as clang.

The first step is to compile LLVM on your machine
(this assumes an in-tree build, but out-of-tree will also work):

     cd $HOME
     git clone https://github.com/llvm-mirror/llvm
     cd llvm
     git checkout release_37
     mkdir build
     cd build
     ../configure
     make

Step 2: Compiling LLVM-CBE
==========================

Next, download and compile llvm-cbe from the same folder:

    cd $HOME/llvm/projects
    git clone https://github.com/JuliaComputing/llvm-cbe
    cd ../build
    make

Step 3: Usage Examples
======================

If llvm-cbe compiles, you should be able to run it with the following commands.
```
$ cd $HOME/llvm/projects/llvm-cbe/test/selectionsort
$ ls
main.c
$ clang -S -emit-llvm main.c
$ ls
main.c main.ll
$ $(HOME)/llvm/build/Debug+Asserts/bin/llvm-cbe main.ll
```

Compile Generated C-Code and Run
================================

```
$ gcc -o main.cbe main.cbe.c
$ ls
main.c  main.cbe  main.cbe.c  main.ll
$ ./main.cbe
```
