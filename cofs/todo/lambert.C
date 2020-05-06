// Template program for how to declare a new projection type, for
//   later inclusion in metric.h, the OMB class library of metric 
//   projections.
// This template shows how the lambert conformal grid was tested prior
//   to acceptance
#include <stdlib.h>

#include "cofs.h"
// Include the file defining the new class
#include "lambert.h"



// This is the standard test program which must run successfully with your
//   grid declarations (the functions and data segments above) for the 
//   grid to be accepted into the OMB class library
int main(int argc, char *argv[]) {
  lambert<float> x, xmask;
  cfslevel<float> ref, refmask;
  ijpt loc;
  fijpt floc;
  latpt ll;
  float maskval = 900., nonval = 999., toler;
  int errcount = 0, count = 0;

  if (argc < 2) { toler = 0.05; }
  else {
    toler = atof(argv[1]);
  }
  printf("Tolerance level is %f\n",toler);

// Print location of corner point:
  ll.lat = 1.0;
  ll.lon = -145.5;
  floc = x.locate(ll);
  printf("%f %f  %f %f\n",ll.lon, ll.lat, floc.i, floc.j);

// Loop over grid in i,j space and print out lat, long of each point:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    //printf("new grid %d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  }
  
// Initialize the reference grid with values equal to the longitude of
//   the grid point
  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
     ll = ref.locate(loc);
     ref[loc] = ll.lon;     
  }
  }
  refmask.set(0.0);
  xmask.set(0.0);

// Translate from the reference grid to the new grid:
  count = 0;
  errcount = 0;
  x.fromall(ref, refmask, maskval, nonval);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc); 
    if (ll.lon < 0) ll.lon += 360.;
    if (x[loc] < 0) x[loc] += 360.;
    if (ref.lin(ll) ) {
      count += 1;
      if ( fabs(x[loc] - ll.lon) > toler) {
        errcount += 1;
        printf("Failed ref->new tolerance test %4d %4d  %8.3f %8.3f %f  %8.3f\n",
                 loc.i, loc.j, ll.lon, x[loc] , ll.lat, ll.lon - x[loc]);
      }
    }
  }
  }
  printf("Finished with ref->new, total vs. err %d vs %d\n", count, errcount);


// Now the same idea in reverse, from new grid to the reference grid:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    // Note that the following is needed to keep away from pathologies
    //   in the interpolation.
    if (ll.lon < 0.) ll.lon += 360.;
    x[loc] = ll.lon;
  }
  }
  ref.fromall(x, xmask, maskval, nonval);
  count = 0;
  errcount = 0;
  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
    ll = ref.locate(loc);
    if (ll.lon < 0) ll.lon += 360.;
    if (ref[loc] < 0) ref[loc] += 360.;
    // If the difference is large and we're still on the source grid
    if (x.lin(ll) ) {
      floc = x.locate(ll);
      count += 1;
      if ( fabs(ref[loc] - ll.lon) > toler ) {
        errcount += 1;
        printf("Failed new->ref %4d %4d  %6.3f %8.3f %8.3f %8.3f  %7.3f %7.3f\n",
                   loc.i, loc.j, ll.lat, ll.lon, ref[loc], 
                   ll.lon - ref[loc], floc.i, floc.j );
      }
    }
  }
  }

  printf("total grid points in new grid vs. number of errors %d vs. %d\n",
          count, errcount);


// Finally, test whether you wind up with the same thing as you started
//  if you go forward and then backwards.  I.e., newgrid -> llgrid -> newgrid
// In the last step, we computed new -> ll, so try reversion:
  count = 0;
  errcount = 0;
  x.fromall(ref, refmask, maskval, nonval);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    if (ref.llin(ll) ) {
      count += 1;
      if (ll.lon < 0) ll.lon += 360.;
      if (x[loc] < 0) x[loc] += 360.;
      if ( fabs(x[loc] - ll.lon) > toler  ) {
        errcount += 1;
        printf("Failed for.back test %4d %4d  %7.3f  %8.3f %8.3f  %8.3f\n",
                loc.i, loc.j, ll.lat, ll.lon, x[loc] , ll.lon - x[loc]);
      }
    }
  }
  }
  printf("for.back total points %d vs. %d errs\n",count, errcount);
 
  return 0;
}
