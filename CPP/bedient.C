#include "ncepgrids.h"

//Bedient grid parameters --
#define NTRY (64*30)
#define DELTA (381.e3 / 30.)

int main(void) {
  psgrid<unsigned char> *nh;
  fijpt loc;
  latpt ll;

  nh = new psgrid<unsigned char>(NTRY, NTRY, NTRY/2, NTRY/2, 
                                 60.0, -10.0, 1.0, DELTA, DELTA);
  for (ll.lat = 90.0; ll.lat >= 0.0; ll.lat -= 0.125) {
  for (ll.lon = 0.0; ll.lon <= 360.0; ll.lon += 0.125) {
     loc = nh->locate(ll);
     if (loc.i >= -0.5 && loc.i <= nh->xpoints() - 0.5 &&
         loc.j >= -0.5 && loc.j <= nh->ypoints() - 0.5 ) {
       //do nothing
     }
     else {
        printf("Ran off grid at %f %f, ij = %f %f\n",
                 ll.lat, ll.lon, loc.i, loc.j);
     }
  }
  }

  return 0;
}


