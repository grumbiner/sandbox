#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  global_12th<float> sst, crit;
  global_12th<unsigned char> land;
  ijpt loc;

  fin = fopen("sst","r");
  sst.binin(fin);
  if (sst.gridmax() > 200) sst -= 273.15;
  fclose (fin);
  fin = fopen("/usr/local/data/seaice_gland5min","r");
  land.binin(fin);
  fclose(fin);

  for (float tc = -2.0; tc <= 45; tc += 0.5) {
    crit.set((float) 0.0);
    for (loc.j = 0; loc.j < land.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < land.xpoints(); loc.i++) {
    if (land[loc] == 0 && sst[loc] < tc) crit[loc] = 1.0;
    }
    }
    printf("tc %4.1f area %f million km^2\n",tc, crit.integrate() / 1.e12);
  }

  return 0;
}
