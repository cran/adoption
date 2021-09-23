/*
 Authors 
 Martin Schlather, schlather@math.uni-mannheim.de

 Copyright (C) 2018 -- 2019 Martin Schlather

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
 
#ifdef DO_PARALLEL
#include <omp.h>
#endif
#include <Basic_utils.h>  
#include <R_ext/Lapack.h>
#include <General_utils.h>

#include "adoption.h"
#include "kleinkram.h"
#include "intrinsics.h"
#include "error.h"

/*

install.packages("quadprog")

#if defined AVX
#define DoublesPerBlock 4
#define Double __m256d
#define ZERO _mm256_setzero_si256()
#define Int __m128i
#define AND _mm_and_si128
#define LESS_THAN(A, B) _mm256_cmp_pd(A, B, _CMP_LT_OS)
#define GREATER_ZERO(A) _mm256_cmp_pd(A, zero, _CMP_GT_OS)
#define EQUAL_ZERO(A) _mm256_cmp_pd(A, zero, _CMP_EQ_OS)
#define BLEND _mm256_blendv_pd

#define MULTDOUBLE _mm256_mul_pd 
#define ADDDOUBLE  _mm256_add_pd 
#define SUBDOUBLE  _mm256_sub_pd
#define ZERODOUBLE _mm256_setzero_pd()
#define LOAD _mm256_load_si256
#define LOADuDOUBLE _mm256_loadu_pd

__m256d LOAD1_DOUBLE(double *A) {
  union { __m256d m256; __m128d s[2]; } Y;
  Y.s[0] = Y.s[1] = _mm_load1_pd(A);
  return Y.m256;
}

#define LOADuINT _mm128_loadu_epi32
#define STORE_DOUBLE _mm256_storeu_pd
#define SHUFFLE8 _mm256_shuffle_epi8 //https://software.intel.com/en-us/node/524017
#define ADD8 _mm256_add_epi8 //https://software.intel.com/en-us/node/523879
#define SAD8 _mm256_sad_epu8
#define HalfBlockType __m128i
#define MOVEMASK _mm256_movemask_ps
#endif
*/

#define NOWEIGHT 0
#define MATRIX 1
#define SPARSE_MATRIX 2
#define SPARSE 3


SEXP adoption(SEXP SUtotal, // r/w
	      SEXP SIc, // r
	      SEXP SWeight,  // r
	      SEXP SIr,     // w
	      SEXP Sdt,     // r
	      SEXP SDeltaUp, SEXP Sit, // r
	      SEXP Sthreshold,  // r
	      SEXP Snevertried, // r/w
	      SEXP Salpha, SEXP Sbeta, SEXP Sgamma, // r	      
	      SEXP Sdummy,   // w
	      SEXP Scheck,
	      SEXP SdNt, SEXP SnT,
	      SEXP SNt,
	      SEXP Sshow_n_indiv, SEXP SM_show,
	      SEXP SU_show, SEXP SIc_show, SEXP SIr_show, SEXP SDeltaUp_show,
	      SEXP SUp_show, SEXP SNever_Tried, SEXP CORES
	      ) {
  #define r_per_step 2
  int
#ifdef DO_PARALLEL
    cores = INTEGER(CORES)[0],
#endif    
    *nevertried = INTEGER(Snevertried),
    rep_m = length(SUtotal),
    m = nrows(Snevertried),
    rep = ncols(Snevertried),
    it = INTEGER(Sit)[0],
    nT = INTEGER(SnT)[0],
    *dNt = INTEGER(SdNt) + it,
    TypeOfWeight = length(SWeight) == 0 ? NOWEIGHT :
                   isMatrix(SWeight) ? MATRIX :
                   isMatrix(VECTOR_ELT(SWeight, 0)) ? SPARSE_MATRIX : SPARSE
    ;
  //  printf("type of weight = %d NO=%d SM=%d rep=%d\n", TypeOfWeight, NOWEIGHT, SPARSE_MATRIX, rep);
  double
    *Utotal = REAL(SUtotal),
    *pIc = REAL(SIc),
    *pIr = REAL(SIr),
    *DeltaUp = REAL(SDeltaUp) + (it - 1L) * rep_m,
    threshold = REAL(Sthreshold)[0],
    *dummy = REAL(Sdummy)
    ;
  bool
    check = LOGICAL(Scheck)[0];
  if (pIc[0] == -1) ERR("Ic[1] different from -1");
  if (check) {
    double *Ic = REAL(SIc);
    for (int i=0; i<rep_m; i++)
      if (ISNA(Utotal[i]) || ISNA(Ic[i])) ERR("U or Ic is NA.");
  }
    
  int nshowIndiv = length(Sshow_n_indiv),
    *showIndiv = INTEGER(Sshow_n_indiv);
  double * Mshow  = REAL(SM_show);
  for (int i=0; i<nshowIndiv; i++) {
    int from = showIndiv[i],
      to = it + i * nT;
    Mshow[to] = Utotal[from];
  }

#ifdef DO_PARALLEL
#pragma omp parallel for num_threads(cores) // version 0.2.11 ohne parallel
#endif  
  for (int r=0; r<rep; r++) {
    int
      mr = m * r,
      *never = nevertried + mr,
      deltaN = 0;
    double *utot = Utotal + mr,
      *Ir = pIr + mr, // zwingend so gross wegen o m p !
      *Up = DeltaUp + mr,
      *Ic = pIc + mr,
      *Isocial = dummy + mr;

    switch (TypeOfWeight) {
    case NOWEIGHT : for (int i=0; i<m; Ir[i++] = 0.0); break;
    case MATRIX :
      if (rep > 1) xA_noomp(utot, REAL(SWeight), m, m, Ir);
      else xA(utot, REAL(SWeight), m, m, Ir);
      break;
              // sparse matrix alg?  - braucht 3/4 der Zeit
    case SPARSE_MATRIX : {
       SEXP Sidx = VECTOR_ELT(SWeight, 0);
      int k = nrows(Sidx),
	*idx = INTEGER(Sidx);
      double *A = REAL(VECTOR_ELT(SWeight, 1));
      for (int i=0; i<m; i++) {
	double sc = 0.0;
	for (int j=0; j<k; j++, idx++, A++) sc += utot[*idx] * *A;
	Ir[i] = sc;
      }
      break;
    }
    case SPARSE : {
      assert(SWeight  != NULL);
      SEXP Sidxlist = VECTOR_ELT(SWeight, 0),
	SA = VECTOR_ELT(SWeight, 1);
      assert(Sidxlist != NULL);
      assert(SA != NULL);
      double *A = REAL(SA);
      
      for (int i=0; i<m; i++) {
	//printf("i=%d ", i);
	SEXP Sidx = VECTOR_ELT(Sidxlist, i);
	int 
	  k = length(Sidx),
	  *idx = INTEGER(Sidx);
	double sc = 0.0;
	for (int j=0; j<k; j++, A++) {
	  //	  printf("j=%d %d %f\n", j, idx[j]);
	  //	  printf("%f %f\n", utot[idx[j]], *A);
	  sc += utot[idx[j]] * *A;
	}
	Ir[i] = sc;
      }
      break;
    }
     default: ERR("unknown weight matrix type");
    }
    /*
#ifdef AVX // Achtung! hier ist ein Fehler drin && es ist nicht wirklich schneller bei  den Standardeinstellungen, da rest viel zu langsam!!
    int endfor = (m / DoublesPerBlock) * DoublesPerBlock;
    // printf("endfor=%d\n", endfor);
    {
    Double
      alpha1 = LOAD1_DOUBLE(REAL(Salpha)),
      alpha2 = LOAD1_DOUBLE(REAL(Salpha) + 1),
      beta1 = LOAD1_DOUBLE(REAL(Sbeta)),
      beta2 = LOAD1_DOUBLE(REAL(Sbeta) + 1),
      gamma1 = LOAD1_DOUBLE(REAL(Sgamma)),
      gamma2 = LOAD1_DOUBLE(REAL(Sgamma) + 1),
      dt = LOAD1_DOUBLE(REAL(Sdt)),
      //  threshold = LOAD1_DOUBLE(REAL(Sthreshold)),
      zero = ZERODOUBLE;
    for (int i=0; i<endfor; i+=DoublesPerBlock) {
    //   printf("i=%d bbar=%f %f Ic=%f %f R=%f %f ", i, b, Ir[i], Ic, dt, RSc[i], utot[i]);
      union { __m256d m; double d[4]; } divers;
#define Ir256 divers.m      
      Ir256 = LOADuDOUBLE(Ir + i);
      Double 
	diff = SUBDOUBLE(LOADuDOUBLE(Ic + i), Ir256),
	factor; 
#define v Ir256
#define dummy diff
      factor = BLEND(alpha1, alpha2, GREATER_ZERO(diff));
      dummy = BLEND(zero, diff, EQUAL_ZERO(factor));
      v = MULTDOUBLE(ADDDOUBLE(v, MULTDOUBLE(factor, dummy)), dt);
      STORE_DOUBLE(Isocial + i, v);
      
#define Isocial256 v
      Double M = LOADuDOUBLE(utot + i);
      diff = SUBDOUBLE(Isocial256, M);
      factor = BLEND(beta1, beta2, GREATER_ZERO(diff));
      dummy = BLEND(zero, diff, EQUAL_ZERO(factor));
      M = ADDDOUBLE(M, MULTDOUBLE(factor, dummy));
      
#define Up256 Ir256
      Up256 = LOADuDOUBLE(Up + i);
      diff = SUBDOUBLE(M, Up256);
#define utot256 Up256
      factor = BLEND(gamma1, gamma2, GREATER_ZERO(diff));
      dummy = BLEND(zero, diff, EQUAL_ZERO(factor));
      utot256 = ADDDOUBLE(utot256, MULTDOUBLE(factor, dummy));
      STORE_DOUBLE(utot + i, utot256);

#define UTOT divers.d
      int nevernew[DoublesPerBlock];
      nevernew[0] = never[i+0] && UTOT[0] < threshold;
      nevernew[1] = never[i+1] && UTOT[1] < threshold;
      nevernew[2] = never[i+2] && UTOT[2] < threshold;
      nevernew[3] = never[i+3] && UTOT[3] < threshold;
      deltaN += (nevernew[0] xor never[i+0]) +
	        (nevernew[1] xor never[i+1]) +
		(nevernew[2] xor never[i+2]) +
		(nevernew[3] xor never[i+3]);
      never[i+0] = nevernew[0];
      never[i+1] = nevernew[1];
      never[i+2] = nevernew[2];
      never[i+3] = nevernew[3];
    }
    dNt[r * nT] = deltaN;
  }
#else
#define endfor 0  
#endif
    */
#define endfor 0  
    
    double
      dt = REAL(Sdt)[0],
      alpha1 = REAL(Salpha)[0],
      alpha2 = REAL(Salpha)[1],
      beta1 = REAL(Sbeta)[0],
      beta2 = REAL(Sbeta)[1],
      gamma1 = REAL(Sgamma)[0],
      gamma2 = REAL(Sgamma)[1];
    for (int i=endfor; i<m; i++) {
      double  diff, factor,
	M = utot[i];
      diff = Ic[i] - Ir[i];
      double u = Ir[i];
      factor = diff > 0 ? alpha1 : alpha2;
      u += factor * (factor != 0 ? diff : 0.0);  // to avoid 0 * Inf = NA
      Isocial[i] = u * dt;
      //       printf("Isc = %f ", u);
      
      diff = Isocial[i] - M;
      factor = diff > 0 ? beta1 : beta2;
      M += factor * (factor != 0 ? diff : 0.0); // to avoid 0 * Inf = NA
      //    printf("M=%f ", M);
      
      diff = M - Up[i];
      utot[i] = Up[i];
      factor = diff > 0 ? gamma1 : gamma2;
      utot[i] += factor * (factor != 0 ? diff : 0.0);
      //    printf("utot=%f ", utot[i]);
      
      int nevernew = never[i] && utot[i] < threshold;
      deltaN += nevernew xor never[i];
      never[i] = nevernew;
    }
    dNt[r * nT] = deltaN;
    INTEGER(SNt)[r] += deltaN;
    //  printf("%d %d\n",  INTEGER(SNt)[r], deltaN);
  }


  if (check) {
    double *Ic = REAL(SIc);
    for (int i=0; i<rep_m; i++)
      if (ISNA(Utotal[i]) || ISNA(Ic[i])) ERR("U or Ic has become NA.");
  }


  double    
    *Ushow = REAL(SU_show),
    *Icshow = REAL(SIc_show),
    *Irshow = REAL(SIr_show),
    *DeltaUpshow = REAL(SDeltaUp_show),
    *Upshow = REAL(SUp_show);
  for (int i=0; i<nshowIndiv; i++) {
    int from = showIndiv[i],
      to = it + i * nT;
    Ushow[to] = Utotal[from];
    Icshow[to] = pIc[from];
    //   if (!R_finite(Icshow[to])) BUG;
    Irshow[to] = pIr[from];
    DeltaUpshow[to] = DeltaUp[from];
    Upshow[to] = Upshow[to - 1] + DeltaUp[from];
  }

  int j = it,
    *NeverTried = INTEGER(SNever_Tried);  
  for (int i=0; i<m; i++, j += nT) {
    NeverTried[j] = nevertried[i];
  }
  return(R_NilValue);
}


SEXP isSparse(SEXP TW, SEXP Threshold) {
  int m = nrows(TW),
    zaehler = 0,
    bytes = m * sizeof(int),
    *z = (int*) MALLOC(bytes),
    threshold = (int) REAL(Threshold)[0];
  double *tW = REAL(TW);
  SEXP Ans;
  for (int i=0; i<m; i++) {
    double *w = tW + i * m;
    int z0 = 0;
    for (int j=0; j<m; z0 += w[j++] != 0);
    z[i] = z0;
    zaehler += z0;
    if (zaehler >= threshold) break;
  }
  //  printf("zaehler=%d %d\n", zaehler, threshold);
  if (zaehler == 0 || zaehler >= threshold) {
    PROTECT(Ans = allocVector(LGLSXP, 1));
    LOGICAL(Ans)[0] = zaehler == 0;
  } else {  
    PROTECT(Ans = allocVector(INTSXP, m));
    MEMCOPY(INTEGER(Ans), z, bytes);
  }  
  UNPROTECT(1);
  FREE(z);
  return Ans;
}


SEXP filled(SEXP TW, SEXP Z) {
  int zaehler = 0,
    m = nrows(TW),
    *z = INTEGER(Z);  
  double *tW = REAL(TW);
  for (int i=0; i<m; zaehler += z[i++]);

  SEXP Where, Value, Ans;
  PROTECT(Value = allocVector(REALSXP, zaehler));
  PROTECT(Where = allocVector(VECSXP, m));
  double *value = REAL(Value);
  
  int L = 0;
  for (int i=0; i<m; i++) {    
    double *w = tW + i * m;
    int k = 0,
      z0 = z[i];
    SEXP Idx;
    PROTECT(Idx = allocVector(INTSXP, z0));
    int *idx = INTEGER(Idx);
    for (int j=0; j<m; j++) {
      if (w[j]) {
	idx[k++] = j;
	// printf("i=%d %d %d %d w=%f\n", i, j, L, z0, w[j]);
	value[L++] = w[j];
      }
    } 
    SET_VECTOR_ELT(Where, i, Idx);
    UNPROTECT(1);
  }
  
  PROTECT(Ans = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(Ans, 0, Where);
  SET_VECTOR_ELT(Ans, 1, Value);
  UNPROTECT(3);
  return Ans;
}




SEXP GoldenbergDistance(SEXP Param, SEXP Dist, SEXP TW, SEXP Goldenberg_C) {
  int m = nrows(Dist);
  if (m != ncols(Dist) || m != ncols(TW) || m != nrows(TW))
    ERR("Illegal use of 'GoldenbergDistance'. See the manual.")
  double *param = REAL(Param),
    p1 = param[0],
    p2 = length(Param) > 1 ? param[1] : 0.0,
    *dist = REAL(Dist),
    *w = REAL(TW),
    c = REAL(Goldenberg_C)[0],
    invC = 1.0 / c;
  //printf("%e %e %e\n", p1, p2, invC);
  if (p2 <= 0.0) {
    int mP1 = m + 1,
      mSq = m * m;
    for (int i=0; i<mSq; i++) w[i] = dist[i] > p1 ? 0.0 : invC;
    for (int i=0; i<mSq; i+=mP1) w[i] = 0.0;
  } else {
    GetRNGstate();
    for (int i=0; i<m; i++, w+=m, dist+=m) {
      int j = 0;
      for (; j<i; j++)
	if ( dist[j] > p1) w[j] = 0.0;
	else w[j] = UNIFORM_RANDOM < p2 ? -invC : invC;
      w[j++] = 0.0;
      for (; j<i; j++)
	if (dist[j] > p1) w[j] = 0.0;
	else w[j]= UNIFORM_RANDOM < p2 ? -invC : invC;
    }
    PutRNGstate();
  }
  return R_NilValue;
} 


SEXP VarDistance(SEXP Param, SEXP Dist, SEXP TW) {
  int m = nrows(Dist),
    mP1 = m + 1,
    mSq = m * m;
  if (m != ncols(Dist) || m != ncols(TW) || m != nrows(TW))
    ERR("Illegal use of 'VarDistance'. See the manual.")
  double *param = REAL(Param),
    p1 = param[0],
    *dist = REAL(Dist),
    *w = REAL(TW);

  for (int i=0; i<mSq; i++) w[i] = EXP(-dist[i] * p1);
  for (int i=0; i<mSq; i+=mP1) w[i] = 0.0;
  
  return R_NilValue;
}
