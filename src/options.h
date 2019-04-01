/*
 Authors 
 Martin Schlather, schlather@math.uni-mannheim.de


 Copyright (C)  2018 --  2019 Martin Schlather

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


#ifndef adoption_options_H
#define adoption_options_H 1

#include <Basic_utils.h>

extern int CORES;

#define adoptionN 25
#define nchar 255
typedef struct adoption_param {
  bool cumfit, fit_operators, fit_m, gui;
  usr_bool buttons2right, join_models, ratio, fit_window;
  int wait, fontsize, Tstart, showNindiv, startwith, 
    numberSteps, sliderColumn, fit_repet, trace, max_increasing,
    simu_on_the_fly;
  double ymax[5], pgtol, factr;
  char screenshot[nchar], filename[nchar], extension[nchar];
} adoption_param;

#define adoption_START {true, false, false, true, 	\
      Nan, Nan,	Nan, Nan,				\
      5000, NA_INTEGER, 1, 10, 1,			\
      20, NA_INTEGER, 100, 0, 3, 2,			\
      {1.3, 3, 6.7, 0.02, 6}, 1e-1, 1e14,		\
      "xfce4-screenshooter -w -s .", "adoption", "rda"}


typedef struct globalparam{
  adoption_param adoption;
} globalparam;
extern globalparam GLOBAL;

#define prefixN 1
extern const char * prefixlist[prefixN], **all[prefixN];
extern int allN[prefixN];
void setparameter(int i, int j, SEXP el, char name[200], bool isList);
void getRFoptions(SEXP *sublist);
void finalparameter();


#endif
