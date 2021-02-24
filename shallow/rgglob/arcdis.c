#include <math.h>

float arcdis (float long1, float lat1, float long2, float lat2) {

  double ab, ac, bc, arg;

  double pi, rdpdg, rearth;
  pi = 3.141592654;
  rdpdg = pi / 180.;
  rearth = 6370.949;
  
  if ( fabs(lat1) > 90.0 || fabs(lat2) > 90.0) {
    return -1;
  }
  
  //     Special case included because trig round off can give identical
  //      points a nonzero separating distance. BG 6/3/93.
  if ( (long1 == long2) && (lat1 == lat2) ) {
    return 0;
  }

  ab = (90.-lat1)*rdpdg;
  ac = (90.-lat2)*rdpdg;
  bc = fabs(long1-long2)*rdpdg;

  arg = cos(ab)*cos(ac)+sin(ab)*sin(ac)*cos(bc);
  if (arg >  1.0) arg = 1.0;
  if (arg < -1.0) arg = -1.0;

  return acos(arg)*rearth;

}
