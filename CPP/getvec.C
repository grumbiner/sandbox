// 10 April 2000
#include "metric.h"

void getvec(ijpt &loc, mvector<float> &x, metricgrid<float> *ary) {
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    printf("getvec, i, loc %d  %3d %3d\n",i, loc.i, loc.j); fflush(stdout);
    x[i] = ary[i].operator[](loc);
  }
  return;
}
