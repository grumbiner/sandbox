#include "ncepgrids.h"

int main(void) {
  global_ice<float> x;
  double area = 0;
  ijpt loc;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    area += x.cellarea(loc);
  }
  }
  printf("global area %e  %e  %e\n",area, area/1e6, area/1e12);
  return 0;
}
