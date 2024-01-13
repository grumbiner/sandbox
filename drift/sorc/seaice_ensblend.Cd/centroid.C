#include "points.h"
#include "mvector.h"

// find the centroid of a number of lat-lon points
//   -- point that minimizes total distance from centroid and all other points
// n.b., try also min squared distance

void centroid(mvector<latpt> &x, latpt &center) ;
void topseek(mvector<latpt> &x, latpt &center, double &y) ;
void seek(mvector<latpt>&x, latpt &center, double &tdist, double &tdist2) ;

extern "C" float arcdis_(float &x1, float&x2, float&x3, float&x4);


void centroid(mvector<latpt> &x, latpt &center) {
  double sum1 = 0, sum2 = 0;
  double step = 0.001;
  int i;

// Find a starting point:
  for (i = 0; i < x.xpoints(); i++) {
    sum1 += x[i].lat;
    if (x[i].lon < 0) {
      sum2 += x[i].lon + 360.;
    }
    else {
      sum2 += x[i].lon ;
    }
  }
  sum1 /= x.xpoints();
  sum2 /= x.xpoints();
  center.lat = sum1;
  center.lon = sum2;

  seek(x, center, sum1, sum2);
  //printf("start at %6.3f %7.3f\n",center.lat, center.lon);
  //printf("start center %6.3f %7.3f distances %f %f\n",center.lat, center.lon, sum1, sum2);

  while (step > 1.e-5) {
    seek(x, center, sum1, sum2);
    //printf("step = %f  center %f %f distances %f %f\n",step, center.lat, center.lon, sum1, sum2);
    topseek(x, center, step);
    //step -= 1.e-4;
    step *= 0.75;
  }

  return;
}

void topseek(mvector<latpt> &x, latpt &center, double &step) {
  double tdist1 = 0., tdist2 = 0.;
  double dx1, dx2, dy1, dy2;
  latpt trial;

// get distances from first guess:
  seek(x, center, tdist1, tdist2);

// Now try dx, dy nudged points:
  trial      = center;
  trial.lon += step;
  seek(x, trial, dx1, dx2);

  trial      = center;
  trial.lat += step;
  seek(x, trial, dy1, dy2);
  
  if ( (dy1 < tdist1) || (dx1 < tdist1) ) {
    if (dy1 < dx1) { center.lat += step; }
    else           { center.lon += step; }
    topseek(x, center, step); // note recursion here -- will kick out when no longer a 'step' that gives improvement
  }
  if (center.lat >  90.0) center.lat =  90.0;
  if (center.lat < -90.0) center.lat = -90.0;
  if (center.lon >= 360.0) center.lon -= 360.0;
  if (center.lon < 0.0)    center.lon += 360.0;

  return;
}
// find distance, distance^2
void seek(mvector<latpt>&x, latpt &center, double &tdist1, double &tdist2) {
  double delta;

  tdist1 = 0.0; tdist2 = 0.0;

  for (int i = 0; i < x.xpoints(); i++) {
    delta   = ARCDIS(x[i].lon, x[i].lat, center.lon, center.lat);
    tdist1 += delta;
    tdist2 += delta*delta;
  }

  return ;
}
