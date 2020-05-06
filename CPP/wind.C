#include "ncepgrids.h"

int main(void) {
  gaussian<float> u(254), v(254);
  gaussian<float> speed(254);
  FILE *fin;
  ijpt loc;
  int count = 0;
  float lim = 20.0;
  palette<unsigned char> gg(26);
  int dx, i, delta;

  dx = 255 / gg.ncol;
  delta = 255 - gg.ncol*dx;
  for (i = 0; i < gg.ncol;  i++) {
    gg.set_color(i, dx*i+delta, dx*i+delta, 0);
  }
  gg.invert();

  fin = fopen("u","r");
  u.binin(fin);
  v.binin(fin);
  fclose(fin);

  for (loc.j = 0; loc.j < u.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < u.xpoints(); loc.i++) {
    speed[loc] = sqrt(u[loc]*u[loc] + v[loc]*v[loc]);
  }
  }

  for (lim = 0.0; lim < 30.0; lim += 1.0) {
    count = 0.0;
    for (loc.j = 0; loc.j < u.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < u.xpoints(); loc.i++) {
      if (speed[loc] > lim) count += 1;
    }
    }
    printf("%d of %d points over %f\n",count, u.xpoints()*u.ypoints(), lim);
  }

  speed.xpm("speed.xpm",1,gg);

  return 0;
}
