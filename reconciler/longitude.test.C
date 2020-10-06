#include "resops.h"

int main(void) {
  hycom<unsigned char> x;
  latpt ll;
  fijpt fij;

  ll.lat = 85.00;

  for (int i = 0; i < 69120 ; i++) {
    ll.lon = -540. +  (float)i * (1./64.);
    printf("working on %f %f ",ll.lon, ll.lat); fflush(stdout);

    fij = x.locate(ll);
    printf(" to %f %f\n",fij.i, fij.j); fflush(stdout);
  }
  
  return 0;
}
