#include "ncepgrids.h"

int main(void) {
  FILE *fin, *ferr;
  global_sst<float> oi1;
  global_12th<float> rtg;
  gaussian<float> nsst, icec, land;

  fin = fopen("oi.1deg", "r");
  oi1.binin(fin);
  fclose(fin);
  fin = fopen("rtghr", "r");
  rtg.binin(fin);
  fclose(fin);
  fin = fopen("nsstish","r");
  nsst.binin(fin);
  fclose(fin);
  fin = fopen("icec", "r");
  icec.binin(fin);
  fclose(fin);
  fin = fopen("land", "r");
  land.binin(fin);
  fclose(fin);

  ijpt loc;
  latpt ll;
  fijpt floc, floc1, floc2;
  //printf("gaussian nx ny %d %d\n",icec.xpoints(), icec.ypoints() ); fflush(stdout);

  ferr = fopen("error_outputs","w");
  float d1, d2;
  for (loc.j = 0; loc.j < icec.ypoints(); loc.j++) { 
  for (loc.i = 0; loc.i < icec.xpoints(); loc.i++) {
    if (icec[loc] > 0.50 || land[loc] == 1.0) continue;

    ll = icec.locate(loc);
    if (fabs(ll.lat) < 45.0 ) continue;
    floc1 = oi1.locate(ll);
    floc2 = rtg.locate(ll);
    if (nsst[loc] > 280. && oi1[floc1] > 280. && rtg[floc2] > 280.) continue;

    printf("%4d %4d  %7.3f %7.3f %6.2f %4.2f %3.1f  ",loc.i, loc.j, ll.lon, ll.lat, nsst[loc], icec[loc], land[loc]);
//    if (icec[loc] > 0. && nsst[loc] < 273.15 - 1.9) {
//      fprintf(ferr,"hotice %4d %4d  %.3f %.3f %.2f %.2f\n",loc.i, loc.j, ll.lon, ll.lat, nsst[loc], icec[loc]);
//    }

    d1 = nsst[loc] - oi1[floc1];
    printf(" %6.2f %.2f ",oi1[floc], d1);
    
    d2 = nsst[loc] - rtg[floc2];
    printf(" %6.2f %.2f ",rtg[floc], d2);
  
    printf("   %.2f ",rtg[floc2] - oi1[floc1]);

    printf("\n");

    if (fabs(d1) > 1. || fabs(d2) > 1.) {
      fprintf(ferr, "%4d %4d  %7.3f %7.3f %6.2f %4.2f %3.1f  ",loc.i, loc.j, ll.lon, ll.lat, nsst[loc], icec[loc], land[loc]);
      fprintf(ferr, " %6.2f %.2f ",oi1[floc1], d1);
      fprintf(ferr, " %6.2f %.2f ",rtg[floc2], d2);
      fprintf(ferr, "   %.2f ",rtg[floc2] - oi1[floc1]);
      fprintf(ferr, "\n");
    }
   

  }
  }

  return 0;
}
