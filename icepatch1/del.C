#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<unsigned char> older, newer;
  global_12th<float> delta, d2;
  ijpt loc;
  latpt ll;
  FILE *fin;
  
  fin = fopen(argv[1],"r");
  older.binin(fin);
  fclose(fin);

  fin = fopen(argv[2],"r");
  newer.binin(fin);
  fclose(fin);

  conv(older, delta);
  conv(newer, d2);
  delta -= d2;
  for (loc.j = 0; loc.j < d2.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < d2.xpoints(); loc.i++) {
    if (delta[loc] != 0) {
      ll = delta.locate(loc);
      printf("%4d %4d %7.3f %7.3f  %3d %3d  %4d\n",loc.i, loc.j, ll.lon, ll.lat, older[loc], newer[loc], newer[loc] - older[loc] );
    }
  }
  }

  return 0;
} 
