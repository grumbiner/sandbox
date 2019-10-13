#include "ncepgrids.h"
//21 May 2015
//Robert Grumbine

int main(void) {
  FILE *fin;
  global_12th<unsigned char> umask;
  global_12th<float> fmask;
  fin = fopen("seaice_gland5min","r");
  umask.binin(fin);
  fclose(fin);

  conv(umask, fmask);
  fin = fopen("mask","w");
  fmask.ftnout(fin);
  fclose(fin);

  return 0;
}
