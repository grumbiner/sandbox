#include "ncepgrids.h"

template<class T>
void showregion(char *label, latpt ll, latpt ur, metricgrid<T> &gfilt,
                  metricgrid<T> &oldland) {
  ijpt x, x1, x2;
  fijpt y;

  printf("%s",label);
  y = gfilt.locate(ll);
  x1.i = (int) (y.i + 0.5);
  x1.j = (int) (y.j + 0.5);
  y = gfilt.locate(ur);
  x2.i = (int) (y.i + 0.5);
  x2.j = (int) (y.j + 0.5);
  printf("x1, x2 %3d %3d %3d %3d\n",x1.i, x1.j, x2.i, x2.j);
  for (x.j = x1.j; x.j < x2.j; x.j++) {
  for (x.i = x1.i; x.i < x2.i; x.i++) {
    ll = gfilt.locate(x);
    if (gfilt[x] != oldland[x] ) {
      printf("%6.3f %6.3f New %3d Old %3d \n",ll.lat, ll.lon,
        gfilt[x], oldland[x] );
    }
  }
  }

  return;
}
