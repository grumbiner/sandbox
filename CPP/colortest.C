//Robert Grumbine

//Include no others
#include "params.h"
#include "grib.h"
#include "points.h"
#include "mvector.h"

//Include 1 other
#include "color.h"

//Include 2 other
#include "buoy.h"


int main(void) {
// For point testing
  point3<float> x0, x1(1), x2(1,2), x3(1,2,3);
  point3< point3<float> > ypoint(x1, x2, x3);
  fijpt f, g;
  ijpt ijf, ijg;
// For parameter testing
  parameters pars;
// for grib testing
  grib_pds xpds;
  grib_gds xgds;
// mvector tests:
//Testing construction of mvectors -- with and without arguments
  mvector<float> a(VEC_LENGTH), b(VEC_LENGTH), z;

// test color class
  palette<unsigned char> g1, gg(19,65);

   return 0;
}
