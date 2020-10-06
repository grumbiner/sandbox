#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  global_quarter<short int> orig;
  FILE *fin, *fout;
  ijpt loc, tloc;
  global_quarter<short int> rey_day;
  short int rey_flag = -999;
  int x[4];
 
// Do a simple reformatting from reynolds grid to mine
  fin = fopen(argv[1], "r");
  fread(&x, sizeof(int), 4, fin);
  rey_day.binin(fin);
  fclose(fin);
  // flip from reynolds convention to mine:
  for (loc.j = 0; loc.j < rey_day.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rey_day.xpoints(); loc.i++) {
    tloc.j = rey_day.ypoints() - loc.j - 1;
    tloc.i = loc.i;
    orig[tloc] = rey_day[loc];
    if (orig[tloc] == rey_flag) {
      orig[tloc] = 0;
    }
  }
  }
  printf("orig max min %d %d\n",orig.gridmax(), orig.gridmin());

  fout = fopen(argv[2], "w");
  orig.binout(fout);
  fclose(fout);

  return 0;
}
