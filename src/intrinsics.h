/*
 Authors
 Martin Schlather, schlather@math.uni-mannheim.de

 Copyright (C) 2018 -- 2019 Martin Schlather,

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/



#ifndef miraculix_initrinsics_H
#define miraculix_initrinsics_H 1


// PKG_CXXFLAGS =  $(SHLIB_OPENMP_CXXFLAGS) -mavx -msse -msse2 -msse3 -msse4 -msse4a -march=core-avx2


#ifdef __MMX__
#define MMX __MMX__
#endif
#ifdef __SSE__
#define SSE __SSE__
#endif
#ifdef  __SSE2__
#define SSE2 __SSE2__
#endif
#ifdef  __SSE3__
#define SSE3 __SSE3__
#endif
#ifdef  __SSSE3__
#define SSSE3 __SSSE3__
#endif
#ifdef  __SSE4A__
#define SSE4A __SSE4A__
#endif
#if defined __SSE41__ || defined __SS42__
#define SSE412 1
#endif
//
#ifdef __AVX__
#define AVX __AVX__
#endif




#if defined (AVX512)
#define SSEBITS 512
#define SSEMODE 30
#elif defined (SSE)
#define SSEBITS 256
#define SSEMODE 20
#elif defined (SSE)
#define SSEBITS 128
#define SSEMODE 10
#else
#define SSEBITS 64
#define SSEMODE 0
#endif


#define ALLIGNED __declspec(align(SSEBITS/8))


#ifdef MMX
//#include <mmintrin.h>
#endif

#ifdef SSE
#include <xmmintrin.h>
#endif

#ifdef SSE2
//#include <emmintrin.h>
#endif

#ifdef SSE3
//#include <pmmintrin.h>
#endif

#ifdef SSSE3
//#include <tmmintrin.h>
#endif

#ifdef SSE4A
//#include <ammintrin.h>
#endif

#ifdef SSE412
//#include <smmintrin.h>
#endif

#ifdef AVX
#include <x86intrin.h>
#endif

#if defined AVX || defined AVX2
#include <x86intrin.h>
#endif

#ifdef AVX512
//#include <immintrin.h>
#endif


#endif


