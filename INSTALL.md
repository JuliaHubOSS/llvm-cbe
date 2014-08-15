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
it with a specific revision of the LLVM development tree. Currently
that revision is:

     202033

To convert c to llvm ir you will need the clang compiler. To aid 
you, we have forked the repository at the appropriate revisions:

* https://github.com/draperlaboratory/clang
* https://github.com/draperlaboratory/llvm

As Fracture is a prototype library, we only use it with debugging
enabled. sample of commands to do that is as follows:

    git clone https://github.com/draperlaboratory/llvm
    cd llvm/tools
    git clone https://github.com/draperlaboratory/clang
    cd ..
    ./configure --enable-debug-symbols --prefix=/usr/local --build=<your info>
    make -j16
    sudo make install

Note: The --build option is important, it should match your `gcc -v`
output:

    $ gcc -v
    Using built-in specs.
    COLLECT_GCC=gcc
    COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-mageia-linux-gnu/4.8.2/lto-wrapper
    Target: x86_64-mageia-linux-gnu
    Configured with: ../configure --prefix=/usr --libexecdir=/usr/lib --with-slibdir=/lib

In this example, the `--build` variable is:

    ./configure --enable-debug-symbols --prefix=/usr/local --build=x86_64-mageia-linux-gnu


Step 2: Compiling LLVM-CBE 
==========================

Sample commands to download and compile fracture are:

    git clone https://github.com/draperlaboratory/llvm-cbe.git llvm-cbe 
    cd llvm-cbe 
    ./autoconf/AutoRegen.sh
    ./configure --with-llvmsrc=/opt/llvm --with-llvmobj=/opt/llvm
    make -j16

The directory `/opt/llvm` is the directory you compiled llvm in step
1. We assume you were in `/opt` when you compiled.


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


$ less main.cbe.c


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
/* Helper union for bitcasts */
typedef union {
  unsigned int Int32;
  unsigned long long Int64;
  float Float;
  double Double;
} llvmBitCastUnion;

/* External Global Variable Declarations */
typedef struct { unsigned char array[3]; } _OC_L_OC_str_t ;

/* Function Declarations */
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
int main(void);
int printf(unsigned char *,...);
void abort(void);


/* Global Variable Declarations */


/* Global Variable Definitions and Initialization */
static _OC_L_OC_str_t _OC_L_OC_str = { "%f" };


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
static const ConstantFloatTy FPConstant0 = 0x4089999AU;    /* 4.300000e+00 */

int main(void) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe_x;    /* Address-exposed local */
  float llvm_cbe_y;    /* Address-exposed local */
  float llvm_cbe_z;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp__1;
  float llvm_cbe_tmp__2;
  float llvm_cbe_tmp__3;
  unsigned int llvm_cbe_call;

  *(&llvm_cbe_retval) = 0u;
  *(&llvm_cbe_x) = 5u;
  llvm_cbe_tmp__1 = *(&llvm_cbe_x);
  *(&llvm_cbe_y) = (((float )(int )llvm_cbe_tmp__1));
  llvm_cbe_tmp__2 = *(&llvm_cbe_y);
  *(&llvm_cbe_z) = (((float )(llvm_cbe_tmp__2 + (*(float*)&FPConstant0))));
  llvm_cbe_tmp__3 = *(&llvm_cbe_z);
  llvm_cbe_call = printf(((&_OC_L_OC_str.array[((int )0u)])), (((double )llvm_cbe_tmp__3)));
  return 0u;
}

