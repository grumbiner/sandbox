#include <stdio.h>

#include "metric.h"

int main(void) {
  mercator<float> x;
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
