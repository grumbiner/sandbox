#include "ncepgrids.h"
#include "lambert.h"

int main(void) {
  lambert<float> great(1500,1000,40.5,-92.5,-90,45.0,1.0e3,1.0e3,1);
  global_12th<unsigned char> mask;
  global_12th<float> fmask;
  palette<unsigned char> gg(19,65);
  FILE *fin;

  fin = fopen("global_5min","r");
  mask.binin(fin);
  fclose(fin);

  conv(mask, fmask);
  great.fromall(fmask, fmask, 157, 157);
  great.xpm("gl.xpm",14,gg);

  return 0;
}
