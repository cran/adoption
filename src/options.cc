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

#include "adoption.h"
#include "options.h"
#include "xport_import.h"
#include "error.h"
 
CALL1(void, getErrorString, errorstring_type, errorstring)
CALL1(void, setErrorLoc, errorloc_type, errorloc)

const char * prefixlist[prefixN] = 
{"adoption"}; 
 

// IMPORTANT: all names of general must be at least 3 letters long !!!
const char *adoption_names[adoptionN] = 
  {"ymax", "buttons2right", "join_models", "wait",
   "fontsize", "Tstart", "showNindiv", "screenshot", "filename",
   "startwith", "numberSteps", "extension", "sliderColumn", "cumfit",
   "fit_repetitions", "fit_operators", "ratio", "tracefit", "pgtol",
   "factr", "fit_m", "windows", "max_increasing", "simuOnTheFly",
   "gui"
  };
  

int PL=C_PRINTLEVEL,
  CORES = INITCORES; // no err ok -- do not delete comment

globalparam GLOBAL = {
  adoption_START
};
utilsparam *GLOBAL_UTILS;


const char **all[prefixN] = {adoption_names};
int allN[prefixN] = {adoptionN};
 

void setparameter(int i, int j, SEXP el, char name[200], 
		  bool VARIABLE_IS_NOT_USED isList,
		  int VARIABLE_IS_NOT_USED local) {
  globalparam *options = &GLOBAL;
  switch(i) { 
  case 0: {// adoption 
    adoption_param *gp;
    gp = &(options->adoption);
    switch(j) {
    case 0: Real(el, name, gp->ymax, 5); break;
    case 1: gp->buttons2right = USRLOG; break; 
    case 2: gp->join_models = USRLOG; break; 
    case 3: gp->wait = INT;break;
    case 4: gp->fontsize = Integer(el, name, 0, true);  break;     
    case 5: gp->Tstart = INT;break; 
    case 6: gp->showNindiv = Integer(el, name, 0, true);  break;     
    case 7: STR(gp->screenshot, 255);break; 
    case 8: STR(gp->filename, 255);break;
    case 9: gp->startwith = INT; break;
    case 10: gp->numberSteps = INT;
      if (gp->numberSteps % 2 == 1) {
	warn("'numberSteps' must be an even number. Given number is increased by 1");
	gp->numberSteps++;
      }
      break;
    case 11: STR(gp->extension, 255);break;
    case 12: gp->sliderColumn = Integer(el, name, 0, true);
	if (gp->sliderColumn != NA_INTEGER && gp->sliderColumn <= 0) {
	  gp->sliderColumn = NA_INTEGER;
	  PRINTF("Non-positive value of '%s' turned NA.", name);
 	}
	break;	
    case 13: gp->cumfit = LOGI;
      if (!gp->cumfit) {
	gp->cumfit = true;
	ERR("currently only cumfit=true possible");
      }
      break;
    case 14: gp->fit_repet = POSINT;break;
    case 15: gp->fit_operators = LOGI; break;
    case 16: gp->ratio = USRLOG; break;
    case 17: gp->trace = POS0INT; break;
    case 18: gp->pgtol = POS0NUM; break;
    case 19: gp->factr = POSNUM; break;      
    case 20: gp->fit_m = LOGI; break;
    case 21: gp->fit_window = USRLOG; break; 
    case 22: gp->trace = POS0INT; break;
    case 23: gp->simu_on_the_fly = POS0INT; break;
    case 24: gp->gui = LOGI; break;
    default: BUG; 
    }}
    break;
  default: BUG;
  }
}

void finalparameter(int VARIABLE_IS_NOT_USED local) {
  PL = GLOBAL_UTILS->basic.Cprintlevel;
  CORES = GLOBAL_UTILS->basic.cores;
}

void getparameter(SEXP sublist, int i, int VARIABLE_IS_NOT_USED local) {
  int k = 0;
  globalparam *options = &GLOBAL;
  switch (i) {
  case 0 : {
    adoption_param *p = &(options->adoption);
    //    ADD(ScalarReal(p->digits));
    SET_VECTOR_ELT(sublist, k++, Num(p->ymax, 5, 5)); 
    ADD(ExtendedBooleanUsr(p->buttons2right));    
    ADD(ExtendedBooleanUsr(p->join_models));
    ADD(ScalarInteger(p->wait));   
    ADD(ScalarInteger(p->fontsize));   
    ADD(ScalarInteger(p->Tstart));   
    ADD(ScalarInteger(p->showNindiv));   
    ADD(ScalarString(mkChar(p->screenshot)));
    ADD(ScalarString(mkChar(p->filename)));
    ADD(ScalarInteger(p->startwith));   
    ADD(ScalarInteger(p->numberSteps));   
    ADD(ScalarString(mkChar(p->extension)));
    ADD(ScalarInteger(p->sliderColumn));   
    ADD(ScalarLogical(p->cumfit));
    ADD(ScalarInteger(p->fit_repet));   
    ADD(ScalarLogical(p->fit_operators));
    ADD(ExtendedBooleanUsr(p->ratio));
    ADD(ScalarInteger(p->trace));
    ADD(ScalarReal(p->pgtol));
    ADD(ScalarReal(p->factr));
    ADD(ScalarLogical(p->fit_m));
    ADD(ExtendedBooleanUsr(p->fit_window));
    ADD(ScalarInteger(p->max_increasing));
    ADD(ScalarInteger(p->simu_on_the_fly));
    ADD(ScalarLogical(p->gui));
 }
    break;
  default : BUG;
  }
}


void attachadoption() {
  includeXport();
  Ext_getUtilsParam(&GLOBAL_UTILS);
  //  GLOBAL_UTILS->solve.max_chol = 8192;
  //  GLOBAL_UTILS->solve.max_svd = 6555;  
/*
  spam.min.n = as.integer(400) # 400
  spam.tol = 1e-8
  spam.min.p = 0.8 # 0.8
  spam.n = as.integer(1:500)
  spam.factor = as.integer(4294967)# prime, so that n * factor is still integer
  silent = T R U E
*/

  finalparameter(isGLOBAL);
  Ext_attachRFoptions(prefixlist, prefixN, all, allN,
		      setparameter, finalparameter, getparameter, 
		      NULL, -10, false);
  finalparameter(isGLOBAL);
}

void detachadoption() {
  Ext_detachRFoptions(prefixlist, prefixN);
}

void RelaxUnknownRFoption(int *RELAX) { 
  Ext_relaxUnknownRFoption((bool) *RELAX); 
}
