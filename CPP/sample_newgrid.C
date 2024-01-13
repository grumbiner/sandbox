// Template program for how to declare a new projection type, for
//   later inclusion in metric.h, the OMB class library of metric 
//   projections.

#include <math.h>
#include "ncepgrids.h"

template <class T>  // The template permits you to have a grid of 
                    //integers, floats, characters, etc. while using 
                    //the same projection and codes
class newgrid : public metricgrid<T> { // mandatory, newgrid = your projection type/name

  private : // data required for specifying your grid type should be private
            // to make it impossible for users to accidentally modify.

  public : 
    // The following functions are mandatory.  You must write this set of
    //   5 functions for the class to be permissable in the OMB library.  
    newgrid(void);   // default parameters for this projection
    newgrid(newgrid<T> &); // how to copy over one of these grids to another
    inline fijpt locate(latpt &); // translate lat-lon to grid ij
    inline latpt locate(ijpt &);  // translate integer ij to lat-lon
    inline latpt locate(fijpt &); // translate float ij to lat-lon
  
};

template <class T>
newgrid<T>::newgrid(void) {
}
template <class T>
newgrid<T>::newgrid(newgrid<T> &x) {
}
template <class T>
fijpt newgrid<T>::locate(latpt &x) {
}
template <class T>
latpt newgrid<T>::locate(ijpt &x) {
}
template <class T>
latpt newgrid<T>::locate(fijpt &x) {
}


// This is the standard test program which must run successfully with your
//   grid declarations (the functions and data segments above) for the 
//   grid to be accepted into the OMB class library
int main(void) {
  newgrid<float> x, xmask;
  global_ice<float> ref, refmask;
  ijpt loc;
  latpt ll;
  float maskval = 900., nonval = 999., toler = 0.0001;

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
                                                    ll.lon, x[loc], ll.lat );
    }
  }
  }

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
    if ( fabs(ref[loc] - ll.lon) > toler) {
      printf("Failed reverse test %d %d  %f %f\n",loc.i, loc.j,
                                                    ll.lon, ref[loc] );
    }
  }
  }
 
  return 0;
} 
