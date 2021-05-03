llvm-cbe
========

This LLVM C backend has been resurrected by Julia Computing with various improvements.

Installation instructions
=========================

This version of the LLVM C backend works with LLVM 10.0.

Earlier versions are supported too (LLVM 7 and earlier should work).

Step 1: Installing LLVM
=======================

Either install the LLVM packages on your system:
--------------------------------------------

On macOS, use [pkgsrc](http://pkgsrc.joyent.com/install-on-osx/) and run the following commands:
```
    pkgin in llvm clang
```

On CentOS, install the llvm-devel package:
```
    dnf install llvm-devel clang
```

On Debian and derivatives, install the llvm-dev package via:
```
    apt install llvm-dev clang
```

Or compile LLVM yourself:
-----------------------------
Note: to convert C to LLVM IR to run the tests, you will also need a C compiler using the LLVM infrastructure, such as clang.

The first step is to compile LLVM on your machine
(this assumes an in-tree build, but out-of-tree will also work):


     git clone https://github.com/llvm/llvm-project.git
     cd llvm-project
     git checkout release/8.x
     mkdir llvm/build
     cd llvm/build
     cmake ..
     make

To run tests, you need to build `lli`.


Step 2: Compiling LLVM-CBE
==========================

Now you can download and compile llvm-cbe.

If you built LLVM yourself, put it in the same folder you built LLVM in:

    cd $HOME/llvm-project/llvm/projects
    git clone https://github.com/JuliaComputing/llvm-cbe
    cd ../build
    cmake -S ..
    make llvm-cbe

If you used your distribution's package, put it wherever you feel like:

    git clone https://github.com/JuliaComputing/llvm-cbe
    cd llvm-cbe && mkdir build && cd build
    cmake -S ..
    make llvm-cbe

Step 3: Usage Examples
======================

If llvm-cbe compiles, you should be able to run it with the following commands.
```
$ cd $HOME/llvm-project/llvm/projects/llvm-cbe/test/selectionsort
$ ls
main.c
$ clang -S -emit-llvm -g main.c
$ ls
main.c main.ll
$ $(HOME)/llvm/build/bin/llvm-cbe main.ll
```

You can find options to configure the C backend's output with `llvm-cbe --help`.
Look for options beginning with `--cbe-`.

Compile Generated C Code and Run
================================

```
$ gcc -o main.cbe main.cbe.c
$ ls
main.c  main.cbe  main.cbe.c  main.ll
$ ./main.cbe
```

Running tests
==================

Unit tests:

```sh
    $ cd $HOME/llvm-project/llvm/build
    $ make CBEUnitTests && projects/llvm-cbe/unittests/CWriterTest
```

Note that you need to have passed `-DLLVM_INCLUDE_TESTS=1` to cmake if you used
your distribution's LLVM package. You also will need to install gtest (on Debian
derivatives: `apt install libgtest-dev`).

Other tests:

First, compile llvm-cbe, and install pytest. Then:

```sh
    $ cd $HOME/llvm-project/llvm/projects/llvm-cbe
    $ pytest
```

You might have to adjust the llvm-cbe and lli paths in that configuration.
