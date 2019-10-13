#include "gaussian.h"
#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  gaussian<float> x, mask;
  stlawrence<float> y;
  latpt ll;
  ijpt loc;
  float nonval = -999.0, flagval = -999.0;
  int pad = y.xpoints()*y.ypoints();
  FILE *fin, *fout;

  mask.set((float) 0.0);

  loc.j = 0;  loc.i = 0;
  ll = x.locate(loc);
  printf("latitude of 0 0 = %f\n",ll.lat);

  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
     ll = x.locate(loc);
     x[loc] = ll.lat; 
  }
  }
  
  y.fromall(x, mask, flagval, nonval);
  for (loc.j = 0; loc.j < y.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints() ; loc.i++) {
    ll = y.locate(loc);
    if (fabs(y[loc] - ll.lat) > 5.e-6) {
      printf("%d %d  %f %f  %f\n",loc.i, loc.j, y[loc], ll.lat, 
                                 (y[loc] - ll.lat) );
    }
  }
  }

  return 0;
}
