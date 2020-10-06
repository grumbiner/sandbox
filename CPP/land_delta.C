#include "ncepgrids.h"

int main(void) {
  global_ice<unsigned char> older, newer;
  FILE *fin1, *fin2;
  ijpt loc;
  latpt ll;

  fin1 = fopen("out.old","r");
  older.binin(fin1);
  fclose(fin1);

  fin2 = fopen("out.new", "r");
  newer.binin(fin2);
  fclose(fin2);

  for (loc.j = 0; loc.j < older.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < older.xpoints(); loc.i++) {
    if (older[loc] != newer[loc] ) {
      ll = older.locate(loc);
      printf("%3d %3d %f %f  %3d %3d\n",loc.i, loc.j, ll.lat, ll.lon, 
            older[loc], newer[loc] );
    }
  }
  }

  return 0;
}
