#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"
#include "points.h"

// Given a lat-long, find the i,j in grid which corresponds
// Robert Grumbine 17 June 1998

int main(int argc, char *argv[]) {
  ijpt x;
  latpt lloc, loc2;
  fijpt cfsirrloc, floc2;
  GRIDTYPE<float> cfsmetric;
  FILE *fin1, *fin2;
  float regval, natval;

  lloc.lat = atof(argv[1]);
  lloc.lon = atof(argv[2]);
  cfsirrloc = cfsmetric.locate(lloc);
  x.i = (int) (cfsirrloc.i + 0.5);
  x.j = (int) (cfsirrloc.j + 0.5);
  printf("lon, lat %6.2f %6.2f  i, j %7.3f %7.3f  \n", 
           lloc.lon, lloc.lat, cfsirrloc.i, cfsirrloc.j);

  return 0;
}
