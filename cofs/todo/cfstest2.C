#include "ncepgrids.h"
#include "cofs.h"

int main(void) {
  cfsnative<float> x, mask;
  cfslevel<float> y;
  cfsreg<float> destination, regmask;
  ijpt l, lx, ly;
  latpt llocx, llocy;
  float nonval = -99., flagval = -99.;

// Test that the grid forward/backward mapping comes out the same as before
  for (l.j = 0; l.j < x.ypoints() ; l.j++) {
  for (l.i = 0; l.i < x.xpoints() ; l.i++) {
    llocx = x.locate(l);
    llocy = y.locate(l);
    lx = x.locate(llocx);
    ly = y.locate(llocy);
    if (lx.i != ly.i || lx.j != ly.j) {
      printf("%3d %3d and %3d %3d vs. %3d %3d\n", lx.i, lx.j, 
         ly.i, ly.j, l.i, l.j);
    }
    //if (x.land(l) ) printf("land");;
    x[l] = l.i;
  }
  }
// Now try a forward/backward interpolation:
  // Native to regular
  destination.fromall(x, mask, nonval, flagval); 
  // regular to native
  regmask.set(0.);
  for (l.j = 0; l.j < regmask.ypoints() ; l.j++) {
  for (l.i = 0; l.i < regmask.xpoints() ; l.i++) {
     if (destination[l] == nonval) regmask[l] = nonval;
  }
  }  
  x.fromall(destination, regmask, nonval, flagval);
  for (l.j = 0; l.j < x.ypoints() ; l.j++) {
  for (l.i = 0; l.i < x.xpoints() ; l.i++) {
     if (fabs(x[l] - l.i) > 0.1 && x[l] != nonval ) {
       printf("large delta %f %f  %3d %3d\n",x[l], x[l] - (float) l.i, l.i, l.j);
     }
  }
  }

  return 0;
}
