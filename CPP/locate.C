// Return the lat-long position of a given i,j coordinate
// Robert Grumbine
// 28 June 1999

#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> n;
  char **x = NULL;
  ijpt loci;
  latpt locl;

  loci.i = strtol(argv[1], x, 10);
  loci.j = strtol(argv[2], x, 10);
  
  locl = n.locate(loci);
  printf("%3d %3d %7.3f %7.3f\n", loci.i, loci.j, locl.lat, locl.lon);

  return 0;
}
