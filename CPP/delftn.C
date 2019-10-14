#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2;
  northgrid<float> a, b, land;
  ijpt loc;

  fin1 = fopen(argv[1],"r");
  fin2 = fopen(argv[2],"r");
  a.ftnin(fin1);
  b.ftnin(fin2);
  fclose(fin1); fclose(fin2);
  fin1 = fopen(argv[3],"r");
  land.binin(fin1);
  fclose(fin1);

  for (loc.j = 0; loc.j < a.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < a.xpoints(); loc.i++) {
    if (a[loc] != b[loc]) {
      printf("%3d %3d  %6.3f %6.3f %f\n",loc.i, loc.j, a[loc], b[loc], land[loc]);
    }
  }
  }

  return 0;
}
