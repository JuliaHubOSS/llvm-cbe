llvm-cbe
========

LLVM 3.9 "C Backend", with improvements


INSTALLATION INSTRUCTIONS
=========================

This version of the LLVM-CBE library works with LLVM 3.9. You will have to
compile this version of LLVM before you try to use LLVM-CBE. This
guide will walk you through the compilation and installation of both
tools and show usage statements to verify that the LLVM-CBE library is
compiled correctly.

The library is known to compile on various Linux versions (Redhat,
Mageia, Ubuntu, Debian), Mac OS X, and Windows (Mingw-w64).


Step 1: Installing GCC 4.8
==========================

### Ubuntu 14.04

```bash
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get install gcc-4.8
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 50
```

Step 2: Installing CMAKE 3.4.3 or higher
========================================

### Ubuntu 14.04

LLVM/Clang 3.9 needs cmake 3.4.3 or higher, since Ubuntu does not have this natively, just install it from a binary package: (installed here in $HOME/progs)

```bash
wget https://cmake.org/files/v3.5/cmake-3.5.2-Linux-x86_64.tar.gz
tar -xf cmake-3.5.2-Linux-x86_64.tar.gz -C ~/progs
export PATH=~/progs/cmake-3.5.2-Linux-x86_64/bin:$PATH

sudo apt-get install clang-3.9 cmake
export CC=clang-3.9
export CXX=clang++-3.9
```

Step 3: Installing LLVM
=======================

The next step is to compile LLVM on your machine
(this assumes an in-tree build, but out-of-tree will also work):

```bash
cd $HOME
git clone https://github.com/llvm-mirror/llvm
cd llvm
git checkout release_39
```

Step 4: Compiling LLVM-CBE
==========================

Next, download and compile llvm-cbe from the same folder:

```bash
cd $HOME/llvm/projects
git clone https://github.com/gapkalov/llvm-cbe.git llvm-cbe
cd $HOME/llvm/
mkdir build
cd build
cmake -G "Unix Makefiles" ..
make
```

Step 5: Run Tests
=================

If llvm-cbe compiles, you should be able to run it with the following commands.

```bash
cd $HOME/llvm/projects/test
make clean
make
```

Step 6: Usage Examples
======================

```bash
$ cd llvm/lib/test/selectionsort
$ ls
main.c
$ clang -S -emit-llvm main.c
$ ls
main.c main.ll
$ $(HOME)/llvm/Release/bin/llvm-cbe main.ll
```

Compile Generated C-Code and Run
================================
```bash
$ gcc -o main.cbe main.cbe.c
$ ls
main.c  main.cbe  main.cbe.c  main.ll
$ ./main.cbe
```


Known Issues
============
1. Type uint64_t not genrated, now replaced to int64_t
2. local array is not generated correctly
3. APInt-c.cpp not compiled
4. Warning as error now disabled
5. Test 86 - disabled
6. Test 87 - disabled
7. Too much FIX ME in the code ;)
