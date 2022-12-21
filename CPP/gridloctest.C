#include "ncepgrids.h"

// Test the forward and backward mapping of the grid location functions
// Let the tolerance be a run-time parameter, and the grid be a 
//   compile-time parameter
// Robert Grumbine September 26, 2003

int main(int argc, char *argv[]) {
  GRIDTYPE<float> x;
  ijpt loc;
  latpt ll;
  fijpt fij;
  float toler;

  toler = atof(argv[1]);
  for (loc.j = 0; loc.j < x.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints() ; loc.i++) {
    ll = x.locate(loc);
    fij = x.locate(ll);
    if (fabs(fij.j - loc.j) > toler || fabs(fij.i - loc.i) > toler) {
      printf("%f %f vs %d %d del = %f %f  %f\n",
		fij.i, fij.j, loc.i, loc.j, fij.i - loc.i, fij.j - loc.j,
	sqrt((fij.i - loc.i)*(fij.i - loc.i) + 
             (fij.j - loc.j)*( fij.j - loc.j) )  );
    }
  }
  }

  return 0;
}
