#include "points.h"
#include "mvector.h"
#include "metric.h"

// find the centroid of a number of lat-lon points
//   -- point that minimizes total distance from centroid and all other points
// n.b., try also min squared distance

void centroid(mvector<latpt> &x, latpt &center) ;
void topseek(mvector<latpt> &x, latpt &center, double &y) ;
void seek(mvector<latpt>&x, latpt &center, double &tdist, double &tdist2) ;

extern "C" float arcdis_(float &x1, float&x2, float&x3, float&x4);

int main(void) {
  mvector<latpt> x(3);
  latpt center;

  x[0].lat = 85.0;
  x[0].lon = 0;
  x[1].lat = 85.0;
  x[1].lon = 120;
  x[2].lat = 85.0;
  x[2].lon = 240;
////  x[3].lat = 46.0;
////  x[3].lon = 46.0;

//x[0].lat = 83.902; x[0].lon =  -68.3763;
//x[1].lat = 83.289; x[1].lon =  -47.979;
//x[2].lat = 80.106; x[2].lon = -119.780;
//x[3].lat = 78.9096; x[3].lon =   -0.2702;
//x[4].lat = 77.5903; x[4].lon =  175.8234;
//x[5].lat = 75.404; x[5].lon = -126.468;
//x[6].lat = 73.541; x[6].lon =   -6.234;
//x[7].lat = 71.157; x[7].lon = -176.325;
//x[8].lat = 70.655; x[8].lon = -175.762;
//x[9].lat = 70.4363; x[9].lon = -174.6299;

  centroid(x, center);
  printf("center at %f %f\n",center.lat, center.lon);

}
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
  printf("start at %f %f\n",center.lat, center.lon);
  printf("start center %f %f distances %f %f\n",center.lat, center.lon, sum1, sum2);

  while (step > 1.e-5) {
    seek(x, center, sum1, sum2);
    printf("step = %f  center %f %f distances %f %f\n",step, center.lat, center.lon, sum1, sum2);
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
