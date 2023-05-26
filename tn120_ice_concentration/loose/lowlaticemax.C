#include "ncepgrids.h"

int main(void) {
  FILE *fin;
  global_12th<float> ice;
  latpt ll;
  ijpt ij;
  float land;

  //fin = fopen("halfice", "r");
  fin = fopen("ice5", "r");
  ice.binin(fin);
  fclose(fin);
  land = ice.gridmax();

  printf("max value %f\n",ice.gridmax());
  for (ij.j = 0; ij.j < ice.ypoints(); ij.j++) {
  for (ij.i = 0; ij.i < ice.xpoints(); ij.i++) {
    ll = ice.locate(ij);
    if (fabs(ll.lat) > 45.) continue;
    if (ice[ij] > 0.00 && ice[ij] != land) {
      printf("%d %d  %f %f  %f\n",ij.i, ij.j, ll.lat, ll.lon, ice[ij]);
    }
  }
  }

  return 0;
}
