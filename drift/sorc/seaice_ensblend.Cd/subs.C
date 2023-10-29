#include "mvector.h"
#include "points.h"
#include "buoy.h"
#include <cstring>
using namespace std;

void unbearing(latpt &initial, float dist, float dir, latpt &final);
void bearing(latpt &x, latpt &y, float &dist, float &dir);
void convert_bw(const float &bdir, float &wdir) ;
void convert_wb(latpt &x0, const float &wdir, float &bdir) ;
extern "C" void wdir_(float &x, float &y, float &d);


// given a distance and direction from a starting point, find lat lon you 
//   wind up at.  movable-type algorithm
void unbearing(latpt &initial, float dist, float dir, latpt &final) {
  float rpdg = parameters::radians_per_degree;
  float theta = dir*rpdg; 
  float R = 6371.2; // km
  final.lat = asin(sin(initial.lat*rpdg)*cos(dist/R) + 
                   cos(initial.lat*rpdg)*sin(dist/R)*cos(dir*rpdg)) / rpdg;
  final.lon = initial.lon + atan2(
     sin(theta)*sin(dist/R)*cos(initial.lat*rpdg),
     cos(dist/R)-sin(initial.lat*rpdg)*sin(final.lat*rpdg)  ) / rpdg;
  return;
}

// given two lat-lon points, find the distance and bearing between them:
void bearing(latpt &x, latpt &y, float &dist, float &dir) {
  float rpdg = parameters::radians_per_degree;
  dist = ARCDIS(x.lon, x.lat, y.lon, y.lat);
  dir  = atan2(sin((x.lon-y.lon)*rpdg)*cos(y.lat*rpdg) , 
               cos(x.lat*rpdg)*sin(y.lat*rpdg) - 
               sin(x.lat*rpdg)*cos(y.lat*rpdg)*cos((x.lon-y.lon)*rpdg) )  
         / rpdg;
  if (dir < 0) dir += 360.0;         
  return;
}

// convert from movable type bearings to weather directions
void convert_bw(const float &bdir, float &wdir) {
  float rpdg = parameters::radians_per_degree;
  float x = cos((90+bdir)*rpdg), y = sin((90+bdir)*rpdg);
  wdir_(x, y, wdir);
  if (wdir >= 360.) wdir -= 360.;
}

// convert from weather directions to movable type bearings
void convert_wb(latpt &x0, const float &wdir, float &bdir) {
  float rpdg = parameters::radians_per_degree;
  float kmdeg=111.2;
  float x = cos((-wdir-90)*rpdg), y = sin((-wdir-90)*rpdg);
  float dlat = y/kmdeg, dlon = x/kmdeg/cos(x0.lat*rpdg);
  latpt x2;
  float dist, dir;
  x2.lat = x0.lat + dlat;
  x2.lon = x0.lon + dlon;
  if (dlon == 0.) {
    if (dlat > 0) { dir = 0.; }
    else {
      dir = 180.;
    }
  }
  else {
    bearing(x0, x2, dist, dir);
  }
   
  if (dir >= 360.) dir -= 360.;

  bdir = dir; 

}
