#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<unsigned char> x;
  mvector<int> y(256);
  ijpt loc;
  int i;

  fin = fopen(argv[1],"r");
  x.binin(fin);
  fclose(fin);

  y = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    y[x[loc] ] += 1;
    if (x[loc] != 0) x[loc] = 1;
    printf("%1d",x[loc]);
  }
  printf("\n");
  }

  return 0;
  for (i = 0; i < y.xpoints(); i++) {
    if (y[i] != 0) {
      printf("i, count %3d %d\n",i,y[i]);
    }
  }

  return 0;
}
