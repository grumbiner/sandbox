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

  locl.lat = strtod(argv[1], x);
  locl.lon = strtod(argv[2], x);
  
  loci = n.locate(locl);
  printf("%4d %4d lat %9.4f N  lon %9.4f E\n", loci.i, loci.j, locl.lat, locl.lon);

  return 0;
}
