//#include "ncepgrids.h"
#include "grid_math.h"

#include "genes.h"

void make_ref(grid2<float> &x);
void get_exact(grid2<float> &x, grid2<float> &y);
#define GRIDSIZE 32

grid2<float> working;

int main(void) {
  grid2<float> initial(GRIDSIZE, GRIDSIZE), exact(GRIDSIZE, GRIDSIZE);
  grid2<float> tmp(GRIDSIZE, GRIDSIZE);

  working.resize(GRIDSIZE, GRIDSIZE);
  make_ref(initial);
  get_exact(initial, exact);

  return 0;
}
void make_ref(grid2<float> &x) {
  ijpt loc;
  float t2 = 2.0;
  float circle = 4.0;
  float midx = x.xpoints()/2., midy = x.ypoints()/2.;
// Build circular pattern first:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
     x[loc] = circle*( (loc.i/midx - 1.)*(loc.i/midx - 1.) + (loc.j/midy - 1.)*(loc.j/midy - 1.) );
  }
  }
// Put in boundary conditions:
  loc.j = 0;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    x[loc] = loc.i/x.xpoints() * t2;
  }
  loc.j = x.ypoints() - 1;
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    x[loc] = t2 + loc.i/x.xpoints() * t2;
  }

  loc.i = 0;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = loc.j/x.ypoints() * t2;
  }
  loc.i = x.xpoints() - 1;
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    x[loc] = t2 + loc.j/x.ypoints() * t2;
  }

  return ;
}

void get_exact(grid2<float> &x, grid2<float> &y) {
  ijpt loc, locip, locjp, locim, locjm;
  int iter = 0, itmax = 4000;
  float lim = 0.001, del;
  grid2<float> lap;

  lap.resize(x.xpoints(), x.ypoints());
  working.set( (float) 0.0);
  lap.set( (float) 0.0);
  y = x;

  do {
    del = 0.0;
    for (loc.j = 1; loc.j < x.ypoints() - 1; loc.j++) {
      locjp.j = loc.j + 1;
      locjm.j = loc.j - 1;
      locip.j = loc.j;
      locim.j = loc.j;
    for (loc.i = 1; loc.i < x.xpoints() - 1; loc.i++) {
      locjp.i = loc.i;
      locjm.i = loc.i;
      locip.i = loc.i + 1;
      locim.i = loc.i - 1;
      working[loc] = (y[locip] + y[locim] + y[locjp] + y[locjm] - 4.*y[loc]) *1.0 / 4.;
      del = max(del, fabs(working[loc]) );
    }
    }
    y += working;
    y.laplace(lap);
    printf("iter del = %d %f rms delta %f rms.lap %f\n",iter, del, working.rms(), lap.rms() ); fflush(stdout);
    iter += 1;
  }
  while (iter < itmax && del > lim);
 
  return ;
}
