#include <stdio.h>

#include "ncepgrids.h"
#include "gaussian.h"

int main(void) {
  FILE *fin, *fout;
  gaussian<float> in(170), inmask(170);
  southgrid<float> out;
  float nonval = 1.e6, maskval = 1.e6;
  int i;
  char fname[900];

  fout = fopen("metout.south", "w");

  inmask.set((float) 0.0);

  for (i = 0; i < 12; i++) {
    sprintf(fname, "fort.%2d",i+50);
    fin = fopen(fname, "r");
    in.ftnin(fin);
    fclose(fin);
    out.fromall(in, inmask, maskval, nonval);
    out.binout(fout);
  }
  fclose(fout);

  return 0;
}
