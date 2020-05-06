#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> high;
  FILE *fout;
  ijpt loc;
  float val;

  while (!feof(stdin)) {
    fscanf(stdin,"%d %d %f\n",&loc.i, &loc.j, &val);
    high[loc] = val;
  }

  fout = fopen(argv[1],"w");
  high.binout(fout);
  fclose(fout);

  return 0;
}
