#include "ncepgrids.h"

int main(void) {
  FILE *fin, *fout;
  global_12th<unsigned char> maskin;
  global_12th<float> maskout;
  fin = fopen("seaice_gland5min","r");
  maskin.binin(fin);
  fclose(fin);
  conv(maskin, maskout);
  fout = fopen("mask","w");
  maskout.ftnout(fout);
  fclose(fout);
  return 0;
}
