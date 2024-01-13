#include "ncepgrids.h"

int main(void) {
  northgrid<float> x;
  ijpt loc;
  latpt ll;
  
  for (loc.j = 0; loc.j < x.ypoints(); loc.j += 2) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i += 2) {
    ll = x.locate(loc);
    printf("%d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  }

  return 0;
}
