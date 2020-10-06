#include <unistd.h>
#include "resops.h"

int main(void) {
  hycom<float> x[1000];
  ijpt loc;
  fijpt fij;
  latpt ll;
  int i;

  //for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  //for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
  //  ll = x.locate(loc);
  //  printf("%3d %3d %f %f\n",loc.i, loc.j, ll.lat, ll.lon);
  //}
  //} 
  printf("%d %d\n",x[0].xpoints(), x[0].ypoints() );
  for (i = 0; i < 1000; i++) {
    printf("i = %d ",i); fflush(stdout);
    loc.i = x[0].xpoints()/(i/5+1);
    loc.j = x[0].ypoints()/(i/5+1);
    printf(" %3d %3d ",loc.i, loc.j); fflush(stdout);
    ll = x[0].locate(loc);
    printf(" %f %f ",ll.lat, ll.lon); fflush(stdout);
    fij = x[0].locate(ll);
    printf("%f %f\n", fij.i, fij.j);
    fflush(stdout);
  }
  return 0;
}
