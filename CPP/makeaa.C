#include "ncepgrids.h"

void getaa(llgrid<float> &dest);

int main(void) {
//  llgrid<short int> etopo2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
//  llgrid<float> diffs(10800, 5400, -1./30., 1./30., 90.0, -180.0);
//  llgrid<short int> dbdb2(10800, 5400, -1./30., 1./30., 90.0, -180.0);
  #define MINUTES 2
  llgrid<float> dest((360*60)/MINUTES+1, ((-60- -90)*60)/MINUTES+1,
                       MINUTES/60., MINUTES/60., -90, 0.0);

  FILE *fin;
  ijpt loc, oloc, l2;
  int count = 0;
  latpt ll;
  float flagval = -1, nonval = -99;

///// Set up the 2 minute Antarctic grid
  printf("about to call getaa\n"); fflush(stdout);
  getaa(dest);
///////////////
 
  return 0;
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
