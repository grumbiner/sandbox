#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> climo;
  gaussian<float> obsd, land, ice;
  int i;
  ijpt loc;
  latpt ll;
  fijpt floc;
  
  fin = fopen(argv[1],"r");
  climo.binin(fin);
  fclose(fin);
  if (climo.gridmax() > 200) climo -= 273.15;

  fin = fopen(argv[2],"r");
  obsd.binin(fin);
  land.binin(fin);
  ice.binin(fin);
  fclose(fin);
  if (obsd.gridmax() > 200) obsd -= 273.15;


// Scan for points that are exceptional:
  float toler = atof(argv[3]);
  //float tmax = 35.0, tmin = -1.9; // tmin = tfreeze(34.6)

  printf("lon  lat  i  j  climo(C)  analy(C)  analy-climo\n");
  for (loc.j = 0; loc.j < obsd.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < obsd.xpoints(); loc.i++) {
    ll = obsd.locate(loc);
    floc = climo.locate(ll);

    if (land[loc] == 0. && ice[loc] == 0.) {
      if ( fabs(climo[floc] - obsd[loc] ) > toler) {
        printf("%8.4f %8.4f  %4d %4d  %5.2f %5.2f %6.2f\n",ll.lon, ll.lat, loc.i, loc.j, 
              climo[floc], obsd[loc], -climo[floc] + obsd[loc]);
      }
    } 
     
  }
  }

  return 0;
}
