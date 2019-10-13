#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  northgrid<float> x;
  palette<unsigned char> gg(19, 65);
  ijpt loc;
  latpt ll;
 
  fin = fopen(argv[1], "r");
  x.binin(fin);
  fclose(fin);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 0) {
      ll = x.locate(loc);
      printf("%3d %3d  %6.2f %6.2f  %6.1f\n",loc.i, loc.j, ll.lon, ll.lat, x[loc]);
    }
  }
  }

  x.scale();
  x.xpm("x.xpm", 7, gg);

  return 0;
}
