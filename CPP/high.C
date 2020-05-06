#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  int i;
  float tc;
  global_12th<float> sst;
  ijpt loc;
  latpt ll;

  tc = atof(argv[1]);
  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i],"r");
    sst.binin(fin);
    if (sst.gridmax() > 200) sst -= 273.15;
    for (loc.j = 0; loc.j < sst.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < sst.xpoints(); loc.i++) {
      if (sst[loc] > tc) {
        ll = sst.locate(loc);
        printf("%4d %4d  %f %f  %f\n",loc.i, loc.j, ll.lat, ll.lon, sst[loc]);
      }
    }
    }
    fclose(fin);
  }
    
  return 0;
}
