#include "ncepgrids.h"

int main(void) {
  global_12th<unsigned char> mask;
  ijpt loc;

  FILE *fin;

  fin = fopen("seaice_gland5min","r");
  mask.binin(fin);
  loc.i = 1;
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
    printf("%4d %3d\n",loc.j+1, (int) mask[loc]);
  }

  return 0;
}
