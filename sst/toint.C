#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin, *fout;
  global_quarter<short int> out;
  global_quarter<float> in;
  int i, index;
  char fname[900];

  for (i = 0; i < 10957; i++) {
    sprintf(fname,"residual.%05d",i+2);
    fin = fopen(fname, "r");
    in.binin(fin);
    fclose(fin);

    for (index = 0; index < in.xpoints()*in.ypoints(); index++) {
      out[index] = (short int) nearbyintf(in[index]);
    }
    sprintf(fname,"sires.%05d",i);
    fout = fopen(fname, "w");
    out.binout(fout);
    fclose(fout);
  }

  return 0;
}
