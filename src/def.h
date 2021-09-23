
#ifndef R_PRINTLEVEL

#ifdef Long
#undef Long
#endif

#ifdef Ulong
#undef Ulong
#endif

#include<inttypes.h> // uintptr_t

#define R_PRINTLEVEL 1
#define C_PRINTLEVEL 1

#define LENMSG 1000
#define LENERRMSG 1000
#define MAXERRORSTRING 1000
#define nErrorLoc 1000
typedef char errorstring_type[MAXERRORSTRING];
typedef char errorloc_type[nErrorLoc];

typedef int Rint;
typedef unsigned int Uint;
typedef uint64_t Ulong;
typedef int64_t Long;


#ifdef SCHLATHERS_MACHINE
#define INITCORES 4
#define DO_TESTS true
#else
#define INITCORES 1
#define DO_TESTS false
#endif

#endif
