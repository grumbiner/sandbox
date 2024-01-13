#include "llgrid.h"
#include "ncepgrids.h"
#include "legacy.h"

//Robert Grumbine

int main(void) {
  GRIDTYPE<float>  x;
  ijpt ll, ur;
  latpt l1, l2;

  ll.i = 0; ll.j = 0;
  ur.i = x.xpoints() - 1;
  ur.j = x.ypoints() - 1;
  l1 = x.locate(ll);
  l2 = x.locate(ur);
  printf("corner locs of are %f %f  %f %f\n",l1.lon, l1.lat, l2.lon, l2.lat);

  return 0;
}
