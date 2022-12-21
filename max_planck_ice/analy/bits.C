#include <stdio.h>
#include <stdlib.h>

#include "grid_math.h"

#define NX 345/2
#define NY 355/2

int main(int argc, char *argv[]) {
  grid2<float> sref(NX, NY), salt(NX, NY), round(NX, NY);
  FILE *fin;
  int nbit, nbitdel;
  int prec;

  fin = fopen(argv[1], "r");
  prec = atoi(argv[2]);

  sref.binin(fin);

  while (!feof(fin)) {
     salt.binin(fin);
     salt.grib_scale(prec, nbit, round) ;
     salt -= sref;
     salt.grib_scale(prec, nbitdel, round) ;
     printf("Full %d delta %d\n",nbit, nbitdel);
  }

  return 0;
} 
