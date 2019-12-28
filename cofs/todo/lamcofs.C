#include <stdlib.h>

// Include the file defining the new class
#include "gaussian.h"
#include "ncepgrids.h"
#include "cofs.h"
#include "lambert.h"



// This is the standard test program which must run successfully with your
//   grid declarations (the functions and data segments above) for the 
//   grid to be accepted into the OMB class library
//ref is the 'old' grid, we'll start here
//x is the 'new' grid.
int main(int argc, char *argv[]) {
  lambert<float> x, x0, xmask;
  cfsnative<float> ref, ref0, refmask;
  ijpt loc;
  fijpt floc;
  latpt ll;
  float maskval = 500., nonval = 555., toler;
  int errcount = 0, count = 0;
  palette<unsigned char> gg(19, 65);

  if (argc < 2) { toler = 0.05; }
  else {
    toler = atof(argv[1]);
  }
  printf("Tolerance level is %f\n",toler);
  printf("Reference grid x cyclicity: %d\n", (int) ref.iscyclicx() );

// Print location of corner point:
  ll.lat = 1.0;
  ll.lon = -145.5;
  floc = x.locate(ll);
  printf("%f %f  %f %f\n",ll.lon, ll.lat, floc.i, floc.j);
  fflush(stdout);

// Loop over grid in i,j space and print out lat, long of each point:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    //printf("new grid %d %d  %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  }
  }
  
  refmask.set(0.0);
  xmask.set(0.0);
// Initialize the reference grid with values equal to the longitude of
//   the grid point
  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
     ll = ref.locate(loc);
     while (ll.lon < 0.) ll.lon += 360.;
     ref0[loc] = ll.lon;     
  }
  }
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    // Note that the following is needed to keep away from pathologies
    //   in the interpolation.
    while (ll.lon < 0.) ll.lon += 360.;
    x0[loc] = ll.lon;
  }
  }


// Translate from the reference grid to the new grid:
  count = 0;
  errcount = 0;
  ref = ref0;
  x.fromall(ref, refmask, maskval, nonval);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc); 
    while (ll.lon < 0.) { ll.lon += 360.; }
    while (x[loc] < 0.) { x[loc] += 360.; }
    if (ref.lin(ll) ) {
      count += 1;
      if (ll.lon < 0. ) {printf("No way we should be here - 2!\n"); }
      if ( fabs(x[loc] - ll.lon) > toler) {
        errcount += 1;
        floc = ref.locate(ll);      
        printf("Fail ref->new %4d %4d  %8.3f %8.3f %7.3f  %8.3f %8.3f %8.3f\n",
                 loc.i, loc.j, ll.lon, x[loc] , ll.lat, ll.lon - x[loc],
                 floc.i, floc.j);
      }
    }
  }
  }
  printf("Finished with ref->new, total vs. err %d vs %d\n", count, errcount);
  x.xpm("x.xpm", 30, gg);
  x -= x0;
  x.scale();
  x.xpm("delx.xpm", 7, gg);
  ref.xpm("ref.xpm",30, gg);


// Now the same idea in reverse, from new grid to the reference grid:
  x = x0;
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
  x.xpm("x2.xpm", 30, gg);
  ref.xpm("ref2.xpm", 30, gg);
  ref -= ref0;
  printf("bounds on delref: %f %f\n",ref.gridmax(), ref.gridmin() );
  ref.scale();
  ref.xpm("delref.xpm", 7, gg);
  printf("total grid points in new grid vs. number of errors %d vs. %d\n",
          count, errcount);


// Finally, test whether you wind up with the same thing as you started
//  if you go forward and then backwards.  I.e., newgrid -> llgrid -> newgrid
  count = 0;
  errcount = 0;
  ref.fromall(x, xmask, maskval, nonval);
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
  x.xpm("x3.xpm", 30, gg);
  x -= x0;
  x.scale();
  x.xpm("delforback.xpm", 7, gg);
  ref.xpm("ref3.xpm", 30, gg);
 
  return 0;
}
