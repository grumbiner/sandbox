#include "ncepgrids.h"

int main(void) {
  mrf1deg<float> p6, tide;
  FILE *fin6, *fintide;
  ijpt loc;
  latpt ll;
  int i = 0;

  fin6 = fopen("fog.p6","r");
  fintide = fopen("fog.tide","r");
  while (!feof(fin6) ) {
    p6.binin(fin6);
    tide.binin(fintide);
    for (loc.j = 0; loc.j < p6.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < p6.xpoints(); loc.i++) {
      if (p6[loc] != tide[loc] ) {
        ll = tide.locate(loc);
        printf("%3d  %3d %3d  %6.2f %7.2f  %f %f  %f\n",i, loc.i, loc.j, ll.lat, ll.lon, p6[loc], tide[loc], p6[loc] - tide[loc]);
      }
    }
    }
    i++;
  }

  return 0;
}
