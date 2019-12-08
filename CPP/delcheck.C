#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> climo, obsd;
  global_12th<unsigned char> mask;
  int i;

  fin = fopen(argv[1],"r");
  climo.binin(fin);
  fclose(fin);
  if (climo.gridmax() > 200) climo -= 273.15;

  fin = fopen(argv[2],"r");
  obsd.binin(fin);
  fclose(fin);
  if (obsd.gridmax() > 200) obsd -= 273.15;

  fin = fopen(argv[3],"r");
  mask.binin(fin);
  fclose(fin);

// Scan for points that are exceptional:
  ijpt loc;
  latpt ll;
  float toler = atof(argv[4]);
  //float tmax = 35.0, tmin = -1.9; // tmin = tfreeze(34.6)

  printf("lon  lat  i  j  climo(C)  analy(C)  analy-climo\n");
  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (mask[loc] == 0) {
      if ( fabs(climo[loc] - obsd[loc] ) > toler) {
// && fabs(climo[loc]) < 1.e-5 ) {
        ll = obsd.locate(loc);
        printf("%8.4f %8.4f  %4d %4d  %5.2f %5.2f %6.2f\n",ll.lon, ll.lat, loc.i, loc.j, 
              climo[loc], obsd[loc], -climo[loc] + obsd[loc]);
      }
    } 
     
  }
  }

  return 0;
}
