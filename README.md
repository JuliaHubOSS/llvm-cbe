llvm-cbe
========

This LLVM C backend has been resurrected by Julia Computing with various improvements.

Installation instructions
=========================

This version of the LLVM C backend works with LLVM 10.0, and has preliminary support for LLVM 11.0.

Step 1: Installing LLVM
=======================

Either install the LLVM packages on your system:
--------------------------------------------

On macOS, use [pkgsrc](http://pkgsrc.joyent.com/install-on-osx/) and run the following commands:
```sh
    ~$ pkgin in llvm clang
```

On CentOS, install the llvm-devel package:
```sh
    ~# dnf install llvm-devel clang
```

On Debian and derivatives, install the llvm-dev package via:
```sh
    ~# apt install llvm-dev clang
```

Or compile LLVM yourself:
-----------------------------
Note: to convert C to LLVM IR to run the tests, you will also need a C compiler using the LLVM infrastructure, such as clang.

The first step is to compile LLVM on your machine
(this assumes an in-tree build, but out-of-tree will also work):

```sh
     ~$ git clone https://github.com/llvm/llvm-project.git
     ~$ cd llvm-project
     llvm-project$ git checkout release/8.x
     llvm-project$ mkdir llvm/build
     llvm-project$ cd llvm/build
     build$ cmake ..
     build$ make
```
To run tests, you need to build `lli`.


Step 2: Compiling LLVM-CBE
==========================

Now you can download and compile llvm-cbe.

If you built LLVM yourself, put it in the same folder you built LLVM in:
```sh
    ~$ cd llvm-project/llvm/projects
    projects$ git clone https://github.com/JuliaComputing/llvm-cbe
    projects$ cd ../build
    build$ cmake -S ..
    build$ make llvm-cbe
```
If you used your distribution's package, put it wherever you feel like:
```sh
    ~$ git clone https://github.com/JuliaComputing/llvm-cbe
    ~$ cd llvm-cbe && mkdir build && cd build
    build$ cmake -S ..
    build$ make llvm-cbe
```
Step 3: Usage Examples
======================

If llvm-cbe compiles, you should be able to run it with the following commands.
```sh
    llvm-cbe$ cd test/selectionsort
    selectionsort$ ls
    main.c
    selectionsort$ clang -S -emit-llvm -g main.c
    selectionsort$ ls
    main.c main.ll
    selectionsort$ ../../build/tools/llvm-cbe/llvm-cbe main.ll
```

You can find options to configure the C backend's output with `llvm-cbe --help`.
Look for options beginning with `--cbe-`.

Compile Generated C Code and Run
================================

```sh
    selectionsort$ gcc -o main.cbe main.cbe.c
    selectionsort$ ls
    main.c  main.cbe  main.cbe.c  main.ll
    selectionsort$ ./main.cbe
```

Running tests
==================

Unit tests:

```sh
    llvm-project$ cd llvm/build
    build$ make CBEUnitTests && projects/llvm-cbe/unittests/CWriterTest
```

Note that you need to have passed `-DLLVM_INCLUDE_TESTS=1` to cmake if you used
your distribution's LLVM package. You also will need to install gtest (on Debian
derivatives: `apt install libgtest-dev`).

Other tests:

First, compile llvm-cbe, and install pytest (e.g. `pip install pytest`). Then:

```sh
    llvm-cbe$ pytest
```

You might have to adjust the llvm-cbe and lli paths in that configuration.

If you want the tests to run faster, installing `pytest-xdist` will allow you to run the test suite in parallel, e.g. `pytest -n 4` if you want to use 4 cores.
