#include "ncepgrids.h"

int main(void) {
  northhigh<float> x;
  ijpt loc;
  latpt ll;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x.cellarea(loc) < 0) {
      ll = x.locate(loc);
      printf("%d %d  %f %f  %f\n",loc.i, loc.j, ll.lon, ll.lat, x.cellarea(loc) );
    }
  }
  }
  return 0;
}
