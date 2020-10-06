#include <stdio.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<float> ingrid;
  GRIDTYPE<float> outgrid;
  GRIDTYPE<unsigned char> land;
  ijpt loc;
  int i, j;
  FILE *fin;
  float mul;

  fin = fopen(argv[1],"r");
  ingrid.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  land.binin(fin);
  fclose(fin);

  if (ingrid.average() < 3.) {
    mul = 100.;
  }
  else {
    mul = 1.;
  }
  outgrid =  ingrid;
  outgrid *= mul;
  for (loc.j = 0; loc.j < outgrid.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < outgrid.xpoints(); loc.i++) {
    if (outgrid[loc] > 128 || land[loc] > (unsigned char) 100) {
       outgrid[loc] = 0.0;
    }
  }
  }

  fin = fopen(argv[3],"w");
  outgrid.binout(fin);
  fclose(fin);
  
  return 0;
}
