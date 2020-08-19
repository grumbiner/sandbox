#include <stdio.h>

#include "ncepgrids.h"

// Test that the given grid's location functions truly do invert.
// Take the corner points, find their latitude/longitude, and then
// find the i,j corresponding to that latitude/longitude.  The resultant
// i,j should be within roundoff error of the corner point values.

// Robert Grumbine 18 April 2000
 
int main(void) {
  GRIDTYPE<float> x;
  ijpt xijpoint;
  fijpt fij;
  latpt latout;

  printf("xpoints, ypoints %d %d\n", x.xpoints(), x.ypoints() );

  xijpoint.i = 0;
  xijpoint.j = 0;
  latout = x.locate(xijpoint);
  fij = x.locate(latout);
  printf("latout of ll %f %f inverted %f %f \n", latout.lat, latout.lon,
        fij.i, fij.j);

  xijpoint.i = -1 + x.xpoints();
  xijpoint.j = 0;
  latout = x.locate(xijpoint);
  fij = x.locate(latout);
  printf("latout of lr %f %f inverted %f %f\n", latout.lat, latout.lon, fij.i, fij.j);

  xijpoint.i = 0;
  xijpoint.j = -1 + x.ypoints();
  latout = x.locate(xijpoint);
  fij = x.locate(latout);
  printf("latout of ul %f %f inverted %f %f\n", latout.lat, latout.lon, fij.i, fij.j);

  xijpoint.i = -1 + x.xpoints();
  xijpoint.j = -1 + x.ypoints();
  latout = x.locate(xijpoint);
  fij = x.locate(latout);
  printf("latout of ur %f %f inverted %f %f\n", latout.lat, latout.lon, fij.i, fij.j);

  return 0;

}
