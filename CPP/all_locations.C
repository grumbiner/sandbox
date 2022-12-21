// Return the lat-long position of a given i,j coordinate
// Robert Grumbine
// 28 June 1999
// Variant: Do so for every grid point

#include <stdio.h>
#include <stdlib.h>
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> n;
  ijpt loci;
  latpt locl;

  for (loci.j = 0; loci.j < n.ypoints(); loci.j++) {
  for (loci.i = 0; loci.i < n.xpoints(); loci.i++) {
    locl = n.locate(loci);
    printf("%3d %3d %7.3f %7.3f\n", loci.i, loci.j, locl.lat, locl.lon);
  }
  }

  return 0;
}
