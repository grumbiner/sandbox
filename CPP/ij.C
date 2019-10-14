#include "ncepgrids.h"
//20 Oct 2004  Robert Grumbine

int main(void) {
  latpt l1, l2, l3, l4;
  int mini, minj, maxi, maxj;
  float minlat = 44, maxlat = 52, minlon = -70+360, maxlon = -52+360;
  northgrid<float> x;
  northhigh<float> xhigh;
  ijpt loc;
  latpt ll;

  x.set( (float) 0.);
  xhigh.set( (float) 0.);

  mini = x.xpoints();
  minj = x.ypoints();
  maxi = 0;
  maxj = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++ ) {
    ll = x.locate(loc);
    if (ll.lon < 0.) ll.lon += 360.;
    if (ll.lat >= minlat && ll.lat <= maxlat &&
        ll.lon >= minlon && ll.lon <= maxlon ) {
      x[loc] = 1;
      mini = min(loc.i, mini);
      minj = min(loc.j, minj);
      maxi = max(loc.i, maxi);
      maxj = max(loc.j, maxj);
    }
  }
  }
  printf("reg mini, j, maxi, j %d %d  %d %d\n",mini, minj, maxi, maxj);


  mini = xhigh.xpoints();
  minj = xhigh.ypoints();
  maxi = 0;
  maxj = 0;
  for (loc.j = 0; loc.j < xhigh.ypoints(); loc.j++ ) {
  for (loc.i = 0; loc.i < xhigh.xpoints(); loc.i++ ) {
    ll = xhigh.locate(loc);
    if (ll.lon < 0.) ll.lon += 360.;
    if (ll.lat >= minlat && ll.lat <= maxlat &&
        ll.lon >= minlon && ll.lon <= maxlon ) {
      xhigh[loc] = 1; 
      mini = min(loc.i, mini);
      minj = min(loc.j, minj);
      maxi = max(loc.i, maxi);
      maxj = max(loc.j, maxj);
    }
  }
  }
  printf("hi  mini, j, maxi, j %d %d  %d %d\n",mini, minj, maxi, maxj);

  return 0;
}
