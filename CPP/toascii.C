#include "ncepgrids.h"
//26 May 2011
//Robert Grumbine

int main(int argc, char *argv[]) {
  GRIDTYPE<DTYPE> bathy;
  FILE *fin;
  fin = fopen(argv[1],"r");
  bathy.binin(fin);
  bathy.printer(stdout);
  return 0;
}
