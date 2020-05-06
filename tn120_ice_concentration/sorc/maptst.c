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
  float lat, lon;
 
  x = 0;
  i = polei_NORTH;
  for (j = 0; j < NY_NORTH; j++)
  {
/*    printf("%d %d %f %f %f %f %f %f %f  %f %f\n",i, j, xorig_NORTH, 
      yorig_NORTH, dx, dy, slat_NORTH, slon_NORTH, sgn_NORTH, rearth, 
      eccen2); */

    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf("%5d %6d %7d %3d\n", j, (int)(1000*lat), (int)(1000*lon), x);

  }
/* Print out the pole point */
  i = polei_NORTH;
  j = polej_NORTH;
    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf("%6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

/* Northern */
  printf("Northern hemisphere\n");
  i = 0;
  j = 0;
    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf(" 0  0 %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);
  i = 0; 
  j = NY_NORTH - 1;
    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf("0  ny %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

   i = NX_NORTH - 1;
   j = 0;
    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf("nx  0 %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

   i = NX_NORTH - 1;
   j = NY_NORTH - 1;
    mapxy(&lat, &lon, i, j, xorig_NORTH, yorig_NORTH, dx, dy,
           slat_NORTH, slon_NORTH, sgn_NORTH, rearth, eccen2);
    printf("nx ny %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

  printf("\nSouthern hemisphere\n");

  i = 0;
  j = 0;
    mapxy(&lat, &lon, i, j, xorig_SOUTH, yorig_SOUTH, dx, dy,
           slat_SOUTH, slon_SOUTH, sgn_SOUTH, rearth, eccen2);
    printf(" 0  0 %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);
  i = 0; 
  j = NY_SOUTH - 1;
    mapxy(&lat, &lon, i, j, xorig_SOUTH, yorig_SOUTH, dx, dy,
           slat_SOUTH, slon_SOUTH, sgn_SOUTH, rearth, eccen2);
    printf(" 0 ny %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

   i = NX_SOUTH - 1;
   j = 0;
    mapxy(&lat, &lon, i, j, xorig_SOUTH, yorig_SOUTH, dx, dy,
           slat_SOUTH, slon_SOUTH, sgn_SOUTH, rearth, eccen2);
    printf("nx  0 %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);

   i = NX_SOUTH - 1;
   j = NY_SOUTH - 1;
    mapxy(&lat, &lon, i, j, xorig_SOUTH, yorig_SOUTH, dx, dy,
           slat_SOUTH, slon_SOUTH, sgn_SOUTH, rearth, eccen2);
    printf("nx ny %6d %7d %3d\n", (int)(1000*lat), (int)(1000*lon), x);
      
      
  return 0;

}
