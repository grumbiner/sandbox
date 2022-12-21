// Template program for how to declare a new projection type, for
//   later inclusion in metric.h, the OMB class library of metric 
//   projections.
// This template shows how the lambert conformal grid was tested prior
//   to acceptance

#include <math.h>
#include "ncepgrids.h"


// This is the standard test program which must run successfully with your
//   grid declarations (the functions and data segments above) for the 
//   grid to be accepted into the OMB class library
int main(void) {
  lambert<float> x, xmask;
  global_ice<float> ref, refmask;
  ijpt loc;
  fijpt floc;
  latpt ll;
  float maskval = 900., nonval = 999., toler = 0.0001;

// Print location of corner point:
  ll.lat = 1.0;
  ll.lon = -145.5;
  floc = x.locate(ll);
  printf("%f %f  %f %f\n",ll.lon, ll.lat, floc.i, floc.j);

// Loop over grid in i,j space and print out lat, long of each point:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    printf("new grid %d %d  %f %f\n",loc.i, loc.j, ll.lon, ll.lat);
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
  x.fromall(ref, refmask, maskval, nonval);
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc); 
    if (ll.lon < 0) ll.lon += 360.;
    if (x[loc] < 0) x[loc] += 360.;
    if ( fabs(x[loc] - ll.lon) > toler) {
      printf("Failed tolerance test %d %d  %f %f  %f\n",loc.i, loc.j, 
                                                    ll.lon, x[loc] , ll.lat);
    }
  }
  }
  return 0;

// Now the same idea in reverse, from new grid to the reference grid:
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    ll = x.locate(loc);
    x[loc] = ll.lon;
  }
  }
  ref.fromall(x, xmask, maskval, nonval);
  for (loc.j = 0; loc.j < ref.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < ref.xpoints(); loc.i++) {
    ll = ref.locate(loc);
    if (ll.lon < 0) ll.lon += 360.;
    if (ref[loc] < 0) ref[loc] += 360.;
    if ( fabs(ref[loc] - ll.lon) > toler) {
      printf("Failed reverse test %d %d  %f %f  %f\n",loc.i, loc.j,
                                                    ll.lon, ref[loc], ll.lat );
    }
  }
  }
 
  return 0;
} 
