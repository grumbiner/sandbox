#include "ncepgrids.h"

int main(void) {
  ndfd<float> mask;
  global_12th<unsigned char> gmask;
  FILE *fin;
  ijpt loc, loc2;
  latpt ll;

  fin = fopen("mask","r");
  mask.ftnin(fin);
  fclose(fin);
  printf("mask max etc. %f %f %f %f\n",mask.gridmax(), mask.gridmin(), mask.average(), mask.rms() );

  fin = fopen("seaice_gland5min","r");
  gmask.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    ll   =  mask.locate(loc);
    loc2 = gmask.locate(ll);
    printf("%4d %4d %f %f  %f %3d\n",loc.i, loc.j, ll.lat, ll.lon, mask[loc], gmask[loc2]);
  }
  }

  return 0;
}
