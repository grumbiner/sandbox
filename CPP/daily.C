#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  global_12th<float> rtg, estim, delta;
  global_12th<unsigned char> mask;
  ijpt loc;
  latpt ll;
  float t1, t2;
  char lname[900];
  char line[900];

  fin = fopen(argv[1], "r");
  rtg.binin(fin);
  fclose(fin);
  if (rtg.gridmax() > 200) rtg -= 273.15;
  
  fin = fopen(argv[2], "r");
  estim.binin(fin);
  fclose(fin);
  if (estim.gridmax() > 200) estim -= 273.15;

  delta = estim;
  delta -= rtg;

  fin = fopen(argv[3], "r");
  mask.binin(fin);
  fclose(fin);

  fin = fopen(argv[4], "r");

  float dist, area, f1, f2, f3, f4, f5;
  while ( !feof(fin) ) {
    fscanf(fin,"%f%f%f%f%f%f%f%f%f",&t1, &t2, &dist, &area, &f1, &f2, &f3, &f4, &f5);
    fgets(lname, 800, fin);
    ll.lat = t1;
    ll.lon = t2;
    loc = rtg.locate(ll);
    if (!feof(fin)) {
      printf(" %7.3f\t%8.3f\t%f\t%f\t%5.2f\t%5.2f\t%6.2f\t%s",ll.lat, ll.lon, dist, area, 
                 rtg[loc], estim[loc], delta[loc], lname);
      fflush(stdout);
    }
  }

// Add some general checks
  sprintf(lname,"ocean\n");
  dist = 0.0;
  area = 0.0;
  for (loc.j = 0; loc.j < rtg.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < rtg.xpoints(); loc.i++) {
    if ( (fabs(delta[loc]) > 5 || rtg[loc] > 40) && (mask[loc] != 157) ) {
      ll = rtg.locate(loc);
      printf(" %7.3f\t%8.3f\t%f\t%f\t%5.2f\t%5.2f\t%6.2f\t%s",ll.lat, ll.lon, dist, area, 
                 rtg[loc], estim[loc], delta[loc], lname);
    }
  }
  }


  return 0;
}
