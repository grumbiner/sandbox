#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"

// Program to conduct the horizontal interpolation between two grids,
//   initially done for the Regional Otis Analysis and Dmitry Chalikov's 
//   Regional Ocean model
// Robert Grumbine 11 June 1999

int main(int argc, char *argv[]) {

// DC-regional related data:
  DESTGRID<float> regout;

// Regional Otis model data:
  SOURCEGRID<float> tmp1, tmask;

// Utility variables:
  ijpt x;
  fijpt y;
  FILE *fin, *fout;
  char fname[600];
  int i, k;
  float nonval = -99.9, landval = 0.0;

// First, read in the restart file and the mask file:
  tmask.set(5.);

  fin = fopen(argv[1], "r");
  //printf("Trying to open file %s\n",argv[1]); fflush(stdout);
  i = tmp1.read(fin);
  if (i == -1) {
    tmp1.set(nonval);
  }
  fclose(fin);

  k = atoi(argv[3]);
  sprintf(fname, "regout.%s.%04d", argv[2], k);
  fout = fopen(fname, "w");
  if (fout == NULL) { printf("Failed to open %s\n", fname); return -1; }
 
  regout.fromall( tmp1, tmask, landval, nonval);
  regout.binout(fout);
  fclose(fout);

  return 0;
  
}
