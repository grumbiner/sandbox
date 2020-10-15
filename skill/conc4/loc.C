#include "ncepgrids.h"

int main(void) {
  global_12th<float> x;
  ijpt loc;
  fijpt floc;
  latpt ll;
  float a;

  for (int i = 0; i < 30; i++) {

    for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      ll = x.locate(loc);
    }
    }
  
    for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
      a = x.cellarea(loc);
    }
    }
  
    for (floc.j = 0; floc.j < x.ypoints(); floc.j++) {
    for (floc.i = 0; floc.i < x.xpoints(); floc.i++) {
      ll = x.locate(floc);
    }
    }
  
  }
//  for (ll.lat = -90.0; ll.lat < 90.0; ll.lat += 0.05) {
//  for (ll.lon = 0.025; ll.lon < 360.0; ll.lon += 0.05) {
//    floc = x.locate(ll);
//  }
//  }
 
  return 0;
}
