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
 

#include <Basic_utils.h>
#include <R_ext/Rdynload.h> 
#include <zzz_RandomFieldsUtils.h>
#include "adoption.h"
#include "xport_import.h"

#define none 0

#define CDEF(name, n, type) {#name, (DL_FUNC) &name, n, type}
//static R_NativePrimitiveArgType
//char2int7[] = { CHARSXP, CHARSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
//	      INTSXP, INTSXP}; 
static const R_CMethodDef cMethods[]  = {
  /*  CDEF(initiate_matrix, 0, none),
  CDEF(file_coding, 9, char2int7),
  */
  CDEF(attachadoption, 0, none),
  CDEF(detachadoption, 0, none),
  {NULL, NULL, 0, none}
};

  
#define CALLDEF_DO(name, n) {#name, (DL_FUNC) &name, n}
static R_CallMethodDef callMethods[]  = {
  // in die respectiven C-Dateien muss adoption.h eingebunden sein
  CALLDEF_DO(adoption, 26),
  CALLDEF_DO(GoldenbergDistance, 4),
  CALLDEF_DO(VarDistance, 3),
  CALLDEF_DO(filled, 2),
  CALLDEF_DO(isSparse, 2),
  //  CALLDEF_DO(),
  {NULL, NULL, 0}
};




#define CALLABLE(FCTN)  R_RegisterCCallable("adoption", #FCTN, (DL_FUNC)  FCTN)
void R_init_adoption(DllInfo  *dll) {
  R_registerRoutines(dll, cMethods, // .C
		     callMethods,
		     NULL, // .Fortran
		     NULL); // extz
  R_useDynamicSymbols(dll, FALSE); //
}



void R_unload_adoption(DllInfo *info) {
  //#ifdef SCHLATHERS_MACHINE
  if (info == NULL) info = NULL; // to get rid of the conpiler warning
  //#endif  
  /* Release resources. */
}
