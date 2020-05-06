#include <stdio.h>
#include <math.h>

#define LINUX

#include "ncepgrids.h"

int main(void) {
  cfslevel<float> native;
  cfsreg<float> regular, regmask, regdepth;
  cfsgrid<float> mask, natgrid;
  float nonval = -99.0, flagval = -99.0;

  ijpt loc;

// Establish the land mask from the depth file -- read in native, compute
//   regular
  for (loc.j = 0; loc.j < mask.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints() ; loc.i++) {
    if (native.depth[loc] < 10.) {
      mask[loc] = flagval;
    }
  }
  }
  native = native.depth;
  native.from(native, mask, flagval, nonval, regdepth);
  for (loc.j = 0; loc.j < regmask.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < regmask.xpoints() ; loc.i++) {
    if (regdepth[loc] < 10.) {
      regmask[loc] = flagval;
    }
  }
  }

//Construct dummy file to interpolate - from regular to native
  for (loc.j = 0; loc.j < regmask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < regmask.xpoints(); loc.i++) {
     regular[loc] = loc.i; 
  }
  }

//  //Interpolate from regular to native:
  native.from(regular, regmask, flagval, nonval, natgrid); 
                    //natgrid is a cfsnative 
  //Interpolate from native back to regular
  native = natgrid;
  native.from(native, mask, flagval, nonval, regular); // regular is cfsreg 
  // Test result for nearness to original
  for (loc.j = 0; loc.j < regmask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < regmask.xpoints(); loc.i++) {
    if (fabs(regular[loc] - loc.i) > 0.2 && regdepth[loc] != nonval ) {
      printf("big interpolation error, %f %f at %3d %3d %f\n",
         regular[loc], regular[loc] - loc.i, loc.i, loc.j, regdepth[loc]);
    }
  }
  }

  return 0;
}
