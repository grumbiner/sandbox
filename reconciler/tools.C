#include "ncepgrids.h"

// -------------------------------- utilities -------------------------
void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) ;
void neighborhood(fijpt &floc, metricgrid<float> &elevs, int range) ;
void getaa(llgrid<float> &dest);

// -------------------------------- utilities -- source -------------------------
void neighborhood(fijpt &floc, metricgrid<unsigned short int> &elevs, int range) {
  ijpt center, loc;
  latpt ll;
  center = floc;
  for (loc.j = center.j - range; loc.j <= center.j + range; loc.j++) {
  for (loc.i = center.i - range; loc.i <= center.i + range; loc.i++) {
    ll = elevs.locate(loc);
    printf("  %5d %5d  %f %f  %5d\n",loc.i, loc.j, ll.lat, ll.lon, elevs[loc]);
  }
  }
  return;
} 
void neighborhood(fijpt &floc, metricgrid<float> &elevs, int range) {
  ijpt center, loc;
  latpt ll;
  center = floc;
  for (loc.j = center.j - range; loc.j <= center.j + range; loc.j++) {
  for (loc.i = center.i - range; loc.i <= center.i + range; loc.i++) {
    ll = elevs.locate(loc);
    printf("  %5d %5d  %f %f  %5d\n",loc.i, loc.j, ll.lat, ll.lon, (int)elevs[loc]);
  }
  }
  return;
} 

void getaa(llgrid<float> &dest) {
  ramp_low<float> aatopo;
  mvector<unsigned short int> tmp(aatopo.xpoints() );
  FILE *fin;
  ijpt loc, l2;
  float flagval = -1, nonval = -99;
  
///// Set up the 2 minute Antarctic grid
  fin = fopen("ramp.raw", "r");
  for (loc.j = 0; loc.j < aatopo.ypoints(); loc.j++) {
    tmp.binin(fin);
    l2.j = aatopo.ypoints() - 1 - loc.j;
  for (loc.i = 0; loc.i < aatopo.xpoints(); loc.i++) {
    l2.i = loc.i;
    aatopo[l2] = (float) tmp[loc.i];
  }
  }
  fclose(fin);
  dest.fromall(aatopo, flagval, nonval);
///////////////
  fin = fopen("reformed.aa","w");
  dest.binout(fin);
  fclose(fin);

  return;
}
