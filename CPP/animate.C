#include <stdio.h>

#include "ncepgrids.h"
#include "color.h"

//October 23 1997

int main(int argc, char *argv[]) {
  southgrid<float> x;
  southgrid<unsigned char> nland, y;
  FILE *fin, *fout;
  palette<unsigned char> gg(19,65);
  char fname[80];
  int i, j;
  float tmax;
  ijpt ij;

  fin = fopen(argv[1], "r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the input southgrid(float)\n");
    return 1;
  }
  x.read(fin);
  fclose(fin);

  tmax = x.gridmax();

  for (ij.j = 0; ij.j < x.ypoints(); ij.j++) {
  for (ij.i = 0; ij.i < x.xpoints(); ij.i++) {
    if (tmax >= 3) {
      y[ij ] = (unsigned char) (x[ij ] + 0.5);
    }
    else {
      y[ij ] = (unsigned char) (x[ij ]*100. + 0.5);
    }
      
  }
  }
  sprintf(fname, "%s.bin",argv[1]);
  fout = fopen(fname, "w");
  y.binout(fout);
  fclose(fout);

  return 0;
}
