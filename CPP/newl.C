#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_ice<unsigned char> g1, g4;
  global_ice<float> g2, g3;
  global_ice<float> pct, pct2;
  global_12th<unsigned char> high;
  global_ice<int> count, sum;

  FILE *fin;
  ijpt loc, tloc;
  latpt ll;

  fin = fopen(argv[1], "r");
  g1.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  g2.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  g3.binin(fin);
  fclose(fin);
  fin = fopen(argv[4], "r");
  pct.binin(fin);
  fclose(fin);
  fin = fopen(argv[5], "r");
  high.binin(fin);
  fclose(fin);

  g2 *= 100;
  g3 *= 100;

// possibly filled pts from global:
  sum.set(0);
  count.set(0);
  for (loc.j = 0; loc.j < high.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < high.xpoints(); loc.i++) {
    ll = high.locate(loc);
    tloc = g4.locate(ll);
    sum[tloc] += 1;
    if (high[loc] == 0 ) {
      count[tloc] += 1;
    }  
  }
  }

  for (loc.j = 0; loc.j < g1.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < g1.xpoints(); loc.i++) {
    if (g3[loc] == 100) g3[loc] = 157;
    if (count[loc] > 0) {
      g4[loc] = 0; }
    else {
      g4[loc] = 157; }
    if (sum[loc] > 0) {
      pct2[loc] = (float) count[loc] / (float) sum[loc];
    }
  }
  }

  for (loc.j = 0; loc.j < g1.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < g1.xpoints(); loc.i++) {
    //if (g1[loc] != g2[loc] || g1[loc] != g3[loc] || g2[loc] != g3[loc]) {
    if (g1[loc] != g4[loc] ) {
      ll = g1.locate(loc);
      printf("%3d %3d  %6.2f %6.2f  %3d %3d %3d %3d\n",loc.i, loc.j, ll.lon, ll.lat, 
               (int) g1[loc], (int) g2[loc], (int) g3[loc], (int) g4[loc]);
    }
  }
  }

  //for (loc.j = 0; loc.j < g1.ypoints(); loc.j++) {
  //for (loc.i = 0; loc.i < g1.xpoints(); loc.i++) {
  //  if (pct[loc] != pct2[loc] ) {
  //    printf("pct %3d %3d  %f %f  %f\n",loc.i, loc.j, pct[loc], pct2[loc], pct2[loc] - pct[loc]);
  //  }
  //}
  //}

  return 0;
}
