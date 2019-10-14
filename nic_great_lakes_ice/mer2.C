#include <stdio.h>
#include "math.h"
#include "metric.h"

int main(void) {
  ijpt x;
  latpt y;
  fijpt fx;
  mercator<float> z;


  x.j = 0;
  x.i = 0;
  printf("About to try to locate(ijpt)\n"); fflush(stdout);
  y = z.locate(x);
  printf("done locate(ijpt)\n"); fflush(stdout);
  printf("0,0            = %f %f\n", y.lat, y.lon);

  x.i = 515;
  x.j = 509;
  y = z.locate(x);
  printf("upper right    = %f %f\n", y.lat, y.lon);

  y.lat =  38.83549833;
  y.lon = -92.3823017;
  fx = z.locate(y);
  printf("lower left = %f %f \n", fx.i, fx.j);

  y.lat =  50.53890037;
  y.lon = -75.70612348;
  fx = z.locate(y);
  printf("upper right = %f %f \n", fx.i, fx.j);




  x.i = 515/2;
  printf("x.i = %d\n", x.i);
  x.j = 510 - 74;
  y = z.locate(x);
  printf("Lake Superior  = %f %f\n", y.lat, y.lon);
   
  x.j = 510-407;
  y = z.locate(x);
  printf("Lake Erie      = %f %f\n", y.lat, y.lon);

  y.lat = 45.0;
  y.lon = -84.0;
  fx = z.locate(y);
  printf("Reference point in i, j %f %f\n", fx.i, fx.j);
  

  x.j = 0;
  x.i = 0;
  y = z.locate(x);
  y.lon = y.lon + 1./40.;
  fx = z.locate(y);
  printf("Reference point 2 in i, j %f %f\n", fx.i, fx.j);
  
  return 0;

}
