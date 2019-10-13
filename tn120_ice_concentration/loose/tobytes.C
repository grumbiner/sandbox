#include <stdio.h>

#include "ncepgrids.h"


int main(int argc, char *argv[]) {
  GRIDTYPE<float> ingrid;
  GRIDTYPE<unsigned char> outgrid;
  ijpt loc;
  int i, j;
  FILE *fin;
  float mul;

  fin = fopen(argv[1],"r");
  //ingrid.ftnin(fin);
  ingrid.binin(fin);
  fclose(fin);

  if (ingrid.average() < 3.) {
    mul = 100.;
  }
  else {
    mul = 1.;
  }

  for (loc.j = 0; loc.j < ingrid.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ingrid.xpoints(); loc.i++) {
    outgrid[loc] = (unsigned char) (0.5 + ingrid[loc] * mul);
  }
  }

  fin = fopen(argv[2],"w");
  outgrid.binout(fin);
  fclose(fin);
  
  return 0;
}
