INSTALLATION INSTRUCTIONS
=========================

This version of the LLVM-CBE library works with LLVM r202033. You will have to
compile this version of LLVM before you try to use LLVM-CBE. This
guide will walk you through the compilation and installation of both
tools and show usage statements to verify that the LLVM-CBE library is
compiled correctly.

The library is known to compile on various linux versions (Redhat,
Mageia, Ubuntu, Debian).

Step 1: Installing LLVM
=======================

LLVM-CBE relies on specific LLVM internals, and so it is best to use
it with a specific revision of the LLVM development tree. Currently,
llvm-cbe works with LLVM 3.5.

To convert c to llvm ir you will need the clang compiler. To aid 
you, we have forked the repository at the appropriate revisions:

* https://github.com/llvm-mirror/clang
* https://github.com/llvm-mirror/llvm

As llvm-cbe is a prototype library, we only use it with debugging
enabled. sample of commands to do that is as follows:

     cd $HOME
     git clone https://github.com/llvm-mirror/llvm
     cd llvm
     git checkout release_35
     cd tools
     git clone https://github.com/llvm-mirror/clang
     cd clang
     git checkout release_35
     cd $HOME/llvm
     ./configure --enable-debug-symbols --prefix=/usr/local --build=<your info>
     make -j16
     sudo make install

Note: The `--build` option is important, it should match your `gcc -v`
output:

    $ gcc -v
    Using built-in specs.
    COLLECT_GCC=gcc
    COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-mageia-linux-gnu/4.8.2/lto-wrapper
    Target: x86_64-mageia-linux-gnu
    Configured with: ../configure --prefix=/usr --libexecdir=/usr/lib --with-slibdir=/lib

In this example, the `--build` variable is:

    ./configure --enable-debug-symbols --prefix=/usr/local --build=x86_64-mageia-linux-gnu
    
Note: Building LLVM with the `make -j16` command uses ~10GB of memory. On less robust machines it is recommended to just use `make` instead. 


Step 2: Compiling LLVM-CBE 
==========================

Sample commands to download and compile llvm-cbe are:

    git clone https://github.com/draperlaboratory/llvm-cbe.git llvm-cbe 
    cd llvm-cbe 
    ./autoconf/AutoRegen.sh
    ./configure --with-llvmsrc=$HOME/llvm --with-llvmobj=$HOME/llvm
    make -j16

The directory `$HOME/llvm` is the directory you compiled LLVM in step 1. We assume you were in your home directory (`/home/yourusername/` in Linux) when you compiled.

Step 3: Usage Examples
======================

If llvm-cbe compiles, you should be able to run it with the following commands.
```
$ cd llvm-cbe/test/selectionsort
$ ls
main.c
$ clang -S -emit-llvm main.c
$ ls
main.c main.ll
$ llvm-cbe main.ll


$ cat main.cbe.c

/* Provide Declarations */
#include <stdarg.h>
#include <setjmp.h>
#include <limits.h>
/* get a declaration for alloca */
#if defined(__CYGWIN__) || defined(__MINGW32__)
#define  alloca(x) __builtin_alloca((x))
#define _alloca(x) __builtin_alloca((x))
#elif defined(__APPLE__)
extern void *__builtin_alloca(unsigned long);
#define alloca(x) __builtin_alloca(x)
#define longjmp _longjmp
#define setjmp _setjmp
#elif defined(__sun__)
#if defined(__sparcv9)
extern void *__builtin_alloca(unsigned long);
#else
extern void *__builtin_alloca(unsigned int);
#endif
#define alloca(x) __builtin_alloca(x)
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__arm__)
#define alloca(x) __builtin_alloca(x)
#elif defined(_MSC_VER)
#define inline _inline
#define alloca(x) _alloca(x)
#else
#include <alloca.h>
#endif

#ifndef __GNUC__  /* Can only support "linkonce" vars with GCC */
#define __attribute__(X)
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __EXTERNAL_WEAK__ __attribute__((weak_import))
#elif defined(__GNUC__)
#define __EXTERNAL_WEAK__ __attribute__((weak))
#else
#define __EXTERNAL_WEAK__
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __ATTRIBUTE_WEAK__
#elif defined(__GNUC__)
#define __ATTRIBUTE_WEAK__ __attribute__((weak))
#else
#define __ATTRIBUTE_WEAK__
#endif

#if defined(__GNUC__)
#define __HIDDEN__ __attribute__((visibility("hidden")))
#endif

#ifdef __GNUC__
#define LLVM_NAN(NanStr)   __builtin_nan(NanStr)   /* Double */
#define LLVM_NANF(NanStr)  __builtin_nanf(NanStr)  /* Float */
#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */
#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */
#define LLVM_INF           __builtin_inf()         /* Double */
#define LLVM_INFF          __builtin_inff()        /* Float */
#define LLVM_PREFETCH(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define __ATTRIBUTE_CTOR__ __attribute__((constructor))
#define __ATTRIBUTE_DTOR__ __attribute__((destructor))
#define LLVM_ASM           __asm__
#else
#define LLVM_NAN(NanStr)   ((double)0.0)           /* Double */
#define LLVM_NANF(NanStr)  0.0F                    /* Float */
#define LLVM_NANS(NanStr)  ((double)0.0)           /* Double */
#define LLVM_NANSF(NanStr) 0.0F                    /* Float */
#define LLVM_INF           ((double)0.0)           /* Double */
#define LLVM_INFF          0.0F                    /* Float */
#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */
#define __ATTRIBUTE_CTOR__
#define __ATTRIBUTE_DTOR__
#define LLVM_ASM(X)
#endif

#if __GNUC__ < 4 /* Old GCC's, or compilers not GCC */ 
#define __builtin_stack_save() 0   /* not implemented */
#define __builtin_stack_restore(X) /* noop */
#endif

#if __GNUC__ && __LP64__ /* 128-bit integer types */
typedef int __attribute__((mode(TI))) llvmInt128;
typedef unsigned __attribute__((mode(TI))) llvmUInt128;
#endif


#ifndef __cplusplus
typedef unsigned char bool;
#endif


/* Support for floating point constants */
typedef unsigned long long ConstantDoubleTy;
typedef unsigned int        ConstantFloatTy;
typedef struct { unsigned long long f1; unsigned short f2; unsigned short pad[3]; } ConstantFP80Ty;
typedef struct { unsigned long long f1; unsigned long long f2; } ConstantFP128Ty;


/* Global Declarations */

/* External Global Variable Declarations */
typedef struct { unsigned char array[26]; } _OC_L_OC_str_t ;
typedef struct { unsigned char array[3]; } _OC_L_OC_str1_t ;
typedef struct { unsigned char array[19]; } _OC_L_OC_str2_t ;
typedef struct { unsigned char array[33]; } _OC_L_OC_str3_t ;
typedef struct { unsigned char array[4]; } _OC_L_OC_str4_t ;

/* Function Declarations */
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
extern int main(void);
extern int printf(unsigned char *,...);
extern int __isoc99_scanf(unsigned char *,...);


/* Global Variable Declarations */


/* Global Variable Definitions and Initialization */
static _OC_L_OC_str_t _OC_L_OC_str = { "Enter number of elements\n" };
static _OC_L_OC_str1_t _OC_L_OC_str1 = { "%d" };
static _OC_L_OC_str2_t _OC_L_OC_str2 = { "Enter %d integers\n" };
static _OC_L_OC_str3_t _OC_L_OC_str3 = { "Sorted list in ascending order:\n" };
static _OC_L_OC_str4_t _OC_L_OC_str4 = { "%d\n" };


/* Function Bodies */
static inline int llvm_fcmp_ord(double X, double Y) { return X == X && Y == Y; }
static inline int llvm_fcmp_uno(double X, double Y) { return X != X || Y != Y; }
static inline int llvm_fcmp_ueq(double X, double Y) { return X == Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_une(double X, double Y) { return X != Y; }
static inline int llvm_fcmp_ult(double X, double Y) { return X <  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ugt(double X, double Y) { return X >  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ule(double X, double Y) { return X <= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_uge(double X, double Y) { return X >= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_oeq(double X, double Y) { return X == Y ; }
static inline int llvm_fcmp_one(double X, double Y) { return X != Y && llvm_fcmp_ord(X, Y); }
static inline int llvm_fcmp_olt(double X, double Y) { return X <  Y ; }
static inline int llvm_fcmp_ogt(double X, double Y) { return X >  Y ; }
static inline int llvm_fcmp_ole(double X, double Y) { return X <= Y ; }
static inline int llvm_fcmp_oge(double X, double Y) { return X >= Y ; }

int main(void) {
  unsigned int retval;    /* Address-exposed local */
  struct { unsigned int array[100]; } array ;    /* Address-exposed local */
  unsigned int n;    /* Address-exposed local */
  unsigned int c;    /* Address-exposed local */
  unsigned int d;    /* Address-exposed local */
  unsigned int position;    /* Address-exposed local */
  unsigned int swap;    /* Address-exposed local */
  unsigned int reg2mem_20_alloca_20_point;
  unsigned int call;
  unsigned int call1;
  unsigned int n_1;
  unsigned int call2;
  unsigned int c_2;
  unsigned int n_3;
  unsigned int c_4;
  unsigned int call3;
  unsigned int c_5;
  unsigned int c_6;
  unsigned int n_7;
  unsigned int c_8;
  unsigned int c_9;
  unsigned int d_10;
  unsigned int n_11;
  unsigned int position_12;
  unsigned int arrayidx11_13;
  unsigned int d_14;
  unsigned int arrayidx13_15;
  unsigned int d_16;
  unsigned int d_17;
  unsigned int position_18;
  unsigned int c_19;
  unsigned int c_20;
  unsigned int arrayidx21_21;
  unsigned int position_22;
  unsigned int arrayidx23_23;
  unsigned int c_24;
  unsigned int swap_25;
  unsigned int position_26;
  unsigned int c_27;
  unsigned int call32;
  unsigned int c_28;
  unsigned int n_29;
  unsigned int c_30;
  unsigned int arrayidx37_31;
  unsigned int call38;
  unsigned int c_32;

  reg2mem_20_alloca_20_point = ((int )0u);
  retval = 0u;
  call = printf(((&_OC_L_OC_str.array[0u])));
  call1 = __isoc99_scanf(((&_OC_L_OC_str1.array[0u])), (&n));
  n_1 = n;
  call2 = printf(((&_OC_L_OC_str2.array[0u])), n_1);
  c = 0u;
  /* BasicBlock Size: 4*/ 
  for(c_2 = c, n_3 = n; (((int )c_2) < ((int )n_3)); c_2 = c ) { 
    n_3 = n;
    c_4 = c;
    call3 = __isoc99_scanf(((&_OC_L_OC_str1.array[0u])), ((&array.array[(((long long )(int )c_4))])));
    c_5 = c;
    c = (c_5 + 1u);
  }  /* end of printLoop */ 
  c = 0u;
  /* BasicBlock Size: 5*/ 
  for(c_6 = c, n_7 = n; (((int )c_6) < ((int )(n_7 - 1u))); c_6 = c ) { 
    n_7 = n;
    c_8 = c;
    position = c_8;
    c_9 = c;
    d = (c_9 + 1u);
    /* BasicBlock Size: 4*/ 
    for(d_10 = d, n_11 = n; (((int )d_10) < ((int )n_11)); d_10 = d ) { 
      n_11 = n;
      position_12 = position;
      arrayidx11_13 = *((&array.array[(((long long )(int )position_12))]));
      d_14 = d;
      arrayidx13_15 = *((&array.array[(((long long )(int )d_14))]));
      if ( (((int )arrayidx11_13) > ((int )arrayidx13_15)) ) { 
        d_16 = d;
        position = d_16;
      }
      else { 
      }
      d_17 = d;
      d = (d_17 + 1u);
    }  /* end of printLoop */ 
    position_18 = position;
    c_19 = c;
    if ( (position_18 != c_19) ) { 
      c_20 = c;
      arrayidx21_21 = *((&array.array[(((long long )(int )c_20))]));
      swap = arrayidx21_21;
      position_22 = position;
      arrayidx23_23 = *((&array.array[(((long long )(int )position_22))]));
      c_24 = c;
      *((&array.array[(((long long )(int )c_24))])) = arrayidx23_23;
      swap_25 = swap;
      position_26 = position;
      *((&array.array[(((long long )(int )position_26))])) = swap_25;
    }
    else { 
    }
    c_27 = c;
    c = (c_27 + 1u);
    position_12 = position;
    arrayidx11_13 = *((&array.array[(((long long )(int )position_12))]));
    d_14 = d;
    arrayidx13_15 = *((&array.array[(((long long )(int )d_14))]));
    if ( (((int )arrayidx11_13) > ((int )arrayidx13_15)) ) { 
    }
    else { 
    }
  }  /* end of printLoop */ 
  call32 = printf(((&_OC_L_OC_str3.array[0u])));
  c = 0u;
  /* BasicBlock Size: 4*/ 
  for(c_28 = c, n_29 = n; (((int )c_28) < ((int )n_29)); c_28 = c ) { 
    n_29 = n;
    c_30 = c;
    arrayidx37_31 = *((&array.array[(((long long )(int )c_30))]));
    call38 = printf(((&_OC_L_OC_str4.array[0u])), arrayidx37_31);
    c_32 = c;
    c = (c_32 + 1u);
  }  /* end of printLoop */ 
  return 0u;
}
```
Compile Generated C-Code and Run
================================
```
$ gcc -o main.cbe main.cbe.c
$ ls
main.c  main.cbe  main.cbe.c  main.ll
$ ./main.cbe
```
