#include "ncepgrids.h"

int main(void) {
  lambert<float> eta;
  lambert<float> ndfd(6500, 2500, 1.0, -145.5, -107.0, 50.0, 2.5e3, 2.5e3, 1.0);
  ijpt loc;
  latpt ll;
  float lat, lon;

  loc.i = 0; loc.j = 0;
  ll = eta.locate(loc);
  printf("eta32 0 0 %f %f\n",ll.lat, ll.lon);
  ll = ndfd.locate(loc);
  printf("ndfd  0 0 %f %f\n",ll.lat, ll.lon);

  loc.i = ndfd.xpoints() - 1;
  ll = ndfd.locate(loc);
  printf("ndfd nx 0 %f %f\n",ll.lat, ll.lon);
  loc.j = ndfd.ypoints() - 1;
  ll = ndfd.locate(loc);
  printf("ndfd nx ny %f %f\n", ll.lat, ll.lon);

  lat = -90.0;
  lon = -180.0;
  for (loc.j = 0; loc.j < ndfd.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ndfd.xpoints(); loc.i++) {
    ll = ndfd.locate(loc);
    if (ll.lat > lat) lat = ll.lat;
    if (ll.lon < -180.0) ll.lon += 360.;
    if (ll.lon > 180.)   ll.lon -= 360.;
    if (ll.lon > lon) lon = ll.lon;
  }
  }
  printf("max lat lon = %f %f\n", lat, lon); 

  return 0;
}
