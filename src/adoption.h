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

#ifndef scan_public_H
#define scan_public_H 1

//
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

//extern "C" {
#ifdef __cplusplus
extern "C" {
#endif
  
  SEXP RFoptions(SEXP options);
  void RelaxUnknownRFoption(int *relax);

  void attachadoption();
  void detachadoption();

  
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
		);

  SEXP isSparse(SEXP TW, SEXP Threshold);
  SEXP filled(SEXP TW, SEXP Z);
  SEXP GoldenbergDistance(SEXP Param, SEXP Dist, SEXP TW, SEXP Goldenberg_C);
  SEXP VarDistance(SEXP Param, SEXP Dist, SEXP TW);
 
    
#ifdef __cplusplus
}
#endif

#endif /* RF_simu_PUBLIC_H*/
