#include "ncepgrids.h"
#include "gaussian.h"

int main(void) {
  gaussian<float> x(254), y;
  ijpt loc;
  latpt lats;
  fijpt floc;

  printf("%d %d\n", x.xpoints(), x.ypoints() );
  printf("%d %d\n", y.xpoints(), y.ypoints() );

  loc.i = 0;
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
    lats = y.locate(loc);
//    printf("%d %d  %f %f\n",loc.i, loc.j, lats.lon, lats.lat);
  }
  loc.j = 0;
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    lats = y.locate(loc);
//    printf("%d %d  %f %f\n",loc.i, loc.j, lats.lon, lats.lat);
  }

  lats.lon = 0.0;
  for (loc.j = 0; loc.j < 360; loc.j++) {
     lats.lat = -90. + (float) loc.j/2.;
     floc = y.locate(lats);
     printf("%f  %f %f\n",lats.lat, floc.i, floc.j);
  }
  

  return 0;
} 
