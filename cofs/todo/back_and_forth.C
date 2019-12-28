#include "ncepgrids.h"
#include "cofs.h"
#include "resops.h"

int main(int argc, char *argv[]) {
  SOURCE<float> x, xmask;
  DEST<float> y, ymask;
  ijpt loc;
  latpt ll;
  fijpt fij;
  float toler;
  float nonval = -999.0, flagval = -999.0;

  toler = atof(argv[1]);
  xmask.set(0.0);
  ymask.set(0.0);

// Set x = longitude and verify that it comes back:
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    ll = x.locate(loc);
    x[loc] = ll.lon;
  }
  }
  y.fromall(x, xmask, flagval, nonval);
  for (loc.j = 0; loc.j < y.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints() ; loc.i++) {
    ll = y.locate(loc);
    if ( (            fabs(ll.lon - y[loc])  > toler)  && 
         (fabs(360. - fabs(ll.lon - y[loc])) > toler)  &&
         (y[loc] != nonval)                               ) {
      printf("lon1 %4d %4d  %9.4f %7.2f  %9.4f %f\n",loc.i, loc.j, 
              ll.lon, ll.lat, y[loc], y[loc] - ll.lon);
    }
  }
  }
  x.fromall(y, ymask, flagval, nonval);
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    ll = x.locate(loc);
    if (fabs(ll.lon - x[loc]) > toler) {
      printf("lon2 %4d %4d  %9.4f %7.2f  %9.4f %f\n",loc.i, loc.j, ll.lon, ll.lat, x[loc], x[loc] - ll.lon);
    }
  }
  }

// Set x = latitude and verify that it comes back:
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    ll = x.locate(loc);
    x[loc] = ll.lat;
  }
  }
  y.fromall(x, xmask, flagval, nonval);
  for (loc.j = 0; loc.j < y.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints() ; loc.i++) {
    ll = y.locate(loc);
    if ( (            fabs(ll.lat - y[loc])  > toler)  &&
         (fabs(360. - fabs(ll.lat - y[loc])) > toler)  &&
         (y[loc] != nonval)                               ) {
      printf("lat1 %4d %4d  %9.4f %7.2f  %9.4f %f\n",loc.i, loc.j,
              ll.lon, ll.lat, y[loc], y[loc] - ll.lat);
    }
  }
  }
  x.fromall(y, ymask, flagval, nonval);
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    ll = x.locate(loc);
    if (fabs(ll.lat - x[loc]) > toler) {
      printf("lat2 %4d %4d  %9.4f %7.2f  %9.4f %f\n",loc.i, loc.j, ll.lon, ll.lat, x[loc], x[loc] - ll.lat);
    }
  }
  }
  return 0;
}
