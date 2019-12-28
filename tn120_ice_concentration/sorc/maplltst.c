#include <stdio.h>
#include "ssmi.h"
#include "icessmi.h"
#include "icegrids.h"

int main(int argc, char *argv[])
{
/* Test behavior of the mapping routine by running along a line in j, for
   i = the pole value.  Output should be smoothly longitude = slon, then
   longitude = reflection across pole of slon, latitude varying as the
   grid spacing. */
/* Also print out the 4 corner points on each map */
/* Bob Grumbine 1 March 1995 */

  int i, j, x;
  int ni, nj, oi, oj;
  float lat, lon;
 
  for (j = 0; j < NY_NORTH; j++)
  {
    lat = 40.+0.0125*j;
    lon = -90.+0.0125*j;

    mapll(lat, lon, &oi, &oj, xorig_NORTH, yorig_NORTH, eccen2, 
        slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
    newmapll(lat, lon, &ni, &nj, xorig_NORTH, yorig_NORTH, eccen2, 
        slat_NORTH, slon_NORTH, rearth, dx, dy, sgn_NORTH);
    printf("%f %f  %d %d new %d %d\n", lat, lon, oi, oj, ni, nj);
    if (ni != oi || nj != oj ) {
      fprintf(stderr,"%f %f  %d %d new %d %d\n", lat, lon, oi, oj, ni, nj);
    }

  }
      
      
  return 0;

}
