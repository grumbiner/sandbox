#include <stdio.h>
#include "grid3.h"
#include "ncepgrids.h"
#include "cofs.h"

#define NZ 19

// Difference two 3d cofs grids (native)

int main(int argc, char *argv[]) {

  cfslevel<float> cfsmetric;

// CFS-related data:
  cfsgrid3<float> t3a(NZ), t3b(NZ);
  cfsgrid3<float> s3a(NZ), s3b(NZ);
  cfsgrid<float> tmp;
  float flag = -99.9;

// Utility variables:
  FILE *fin, *fout, *fout2, *fin2;
  int i, j, k;

  fin = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");
  if (fin == NULL || fin2 == NULL || fout == NULL) {
    printf("Could not open a required file\n");
    if (fin ==  NULL) printf("-- first input\n");
    if (fin2 == NULL) printf("-- second input\n");
    if (fout == NULL) printf("-- output file\n");
  }

// Now open the files and put them in to the right level of the 3d grids.
  for (j = 0; j < NZ; j++) {
    tmp.binin(fin);
    t3a.put_layer(j, tmp);
    tmp.binin(fin2);
    t3b.put_layer(j, tmp);
  }
  for (j = 0; j < NZ; j++) {
    tmp.binin(fin);
    s3a.put_layer(j, tmp);
    tmp.binin(fin2);
    s3b.put_layer(j, tmp);
  }

  t3a -= t3b;
  s3a -= s3b;

  for (j = 0; j < NZ; j++) {
    float tavg, savg;
    t3a.get_layer(j, tmp);
    tavg = tmp.average();
    s3a.get_layer(j, tmp);
    savg = tmp.average();
    printf("Layer %2d average difference T- %f S- %f\n", j, tavg, savg);
  }
  t3a.printer(fout);
  s3a.printer(fout);
  fclose(fout);

  return 0;
}
