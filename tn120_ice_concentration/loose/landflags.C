#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_12th<unsigned char> x, y;
  FILE *fin;
  palette<unsigned char> gg(19,65);
  mvector<int> flag(256);
  ijpt loc;

  fin = fopen(argv[1], "r");
  x.binin(fin);
  fclose(fin);
  flag = 0;
  for (loc.j = 0; loc.j <  x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++ ) {
    flag[x[loc] ] += 1;
    if (x[loc] == 157) y[loc] = LAND;
    if (x[loc] ==   0) y[loc] = 4;
    if (x[loc] == 100) y[loc] = 4 + (100 - MIN_CONC)/7;
    if (x[loc] == 195) y[loc] = COAST;
    if (x[loc] == 224) y[loc] = NO_DATA;
  }
  }
  for (int i = 0; i < flag.xpoints(); i++) {
    if (flag[i] != 0) {
      printf("%3d %d\n",i, flag[i]);
    }
  }

  y.xpm("mask.xpm",7,gg);

  return 0;
}
