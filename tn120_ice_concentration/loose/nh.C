#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northhigh<unsigned char> x, y;
  northhigh<float> z;
  FILE *fin, *fout;
  palette<unsigned char> gg(19,65);
  mvector<int> flag(256);
  ijpt loc;
  float fi, fj, val;
  int i, j;

  fin = fopen(argv[1], "r");
  x.binin(fin);
  fclose(fin);
  flag = 0;
  for (loc.j = 0; loc.j <  x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++ ) {
    if (x[loc] == 157 ) y[loc] = 0;
    //if (x[loc] ==   0 || x[loc] == 195) y[loc] = 4;
    if (x[loc] ==   0 ) y[loc] = 4;
    if (x[loc] == 100 ) y[loc] = 4 + (100 - MIN_CONC)/7;
    if (x[loc] == 195) y[loc] = 1;
    if (x[loc] == 224) y[loc] = 18;
    flag[y[loc] ] += 1;
  }
  }

  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++ ) {
    flag[y[loc] ] += 1;
    z[loc] = (float) y[loc];
  }
  }

  for (int i = 0; i < flag.xpoints(); i++) {
    if (flag[i] != 0) {
      printf("%3d %d\n",i, flag[i]);
    }
  }

  y.xpm("nhmask.xpm",1,gg);
  fout = fopen(argv[2],"w");
  z.binout(fout);
  fclose(fout);

  return 0;
}
