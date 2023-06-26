#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> high;
  global_ice<float> low;
  ijpt loc, tloc;
  latpt ll;

  fin = fopen(argv[1],"r");
  high.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  low.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < high.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < high.xpoints(); loc.i++) {
    ll = high.locate(loc);
    tloc = low.locate(ll);
    if (low[tloc] != high[loc] && low[tloc] < 1.28 && high[loc] < 1.28) {
      printf("%7.3f %7.3f  %4.2f %4.2f  %5.2f\n",ll.lat, ll.lon, low[tloc], high[loc], 
          high[loc] - low[tloc]);
    }
  }
  }

  return 0;
}
