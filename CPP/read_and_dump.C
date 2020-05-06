#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<TYPE> x;
  FILE *fin;

  fin = fopen(argv[1],"r");
  x.binin(fin);
  fclose(fin);
  x.printer(stdout);
  return 0;
}
