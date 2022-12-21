#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"
#include "cofs.h"

#define NZ 31

// Program to read in the 31 horizontal slices from OTIS in T, S and
//  write out a 3d cfs field.

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid<float> cfsout;
  cfsgrid3<float> t3out(19), t3(NZ);
  cfsgrid<float> tmp;
  mvector<float> sound(NZ), vert(19);
  metricvector<float> cfsotis(NZ), cfsfin(19);
  float flag = -99.9;

// Aux:
  grid2<float> transects(t3out.xpoints(), t3out.nz);
  grid2<float> transecte(t3out.ypoints(), t3out.nz);
  mvector<float> otisz(NZ);

// Utility variables:
  latpt alat;
  ijpt x;
  fijpt y;
  FILE *fin, *fout, *fout2, *fin2;
  char fname[600];
  int i, j, k;
  float depth;

// First, read in the file with level tags and depths:
  fin = fopen(argv[1], "r");
  if (fin == NULL) { printf("Failed to open input file!\n"); return -1;}

  i = 5;
  for (j = 0; j < NZ; j++) {
    fscanf(fin, "%4d %6.1f", &i, &depth);
    //fscanf(fin, "%s\n", fname);
    //printf("fname = %s\n", fname);
    printf("tag, depth = %d %f\n", i, depth);

//    otisz.vec[j] = depth;

//    sprintf(fname,"cfsout.tmp.%0d",i);
//    fin2 = fopen(fname, "r");
//    tmp.read(fin2);
//    t3.put_layer(j, tmp);
    
  }

  return 0;
   
  cfsotis.set_metric(otisz);

}
