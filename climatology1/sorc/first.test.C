#include <ctype.h>
#include <stdlib.h>
#include "ncepgrids.h"

// Basic test program to verify that Walsh grids are being read in correctly
// and the mapping functions are working.  If the mapping is correct,
// there will be no messages listing out locations.
// Robert Grumbine
// 16 July 1999

extern void walshtof(metricgrid<char> &c, metricgrid<float> &f) ;

int main(void) {
  FILE *fin;
  northwalsh<char> conc;
  northwalsh<float> arclat, arclon, fconc;
  float land = 1.57, dlat, dlon;
  ijpt loc;
  latpt lats;

// Read in the latlon file to verify the mapping algorithm
  fin = fopen("arctic.latlon","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic latlon file\n");
    return 2;
  }
  printf("Preparing to read the latlon file\n"); fflush(stdout);
  arclat.walshread_f(fin);
  arclon.walshread_f(fin);
  fclose(fin);

// Read in a data file:
  fin = fopen("arctic-concentration-01-90.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the arctic concentration file\n");
    return 1;
  }
  conc.walshread_c(fin);
  fclose(fin);

// Now check the lat-long, and first concentration:
  for (loc.j = 0; loc.j < conc.ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < conc.xpoints() ; loc.i++) {
      lats = conc.locate(loc);
      if (lats.lon < 0.) lats.lon += 360.;
      dlat = arclat[loc] - lats.lat;
      dlon = arclon[loc] - lats.lon;
      if (fabs(dlat) > 0.001 || fabs(dlon) > 0.001) {
        printf("%2d %2d %1c %7.3f %8.3f  %7.3f %8.3f  %7.3f %7.3f\n",
               loc.i, loc.j, conc[loc], lats.lat, lats.lon, 
               arclat[loc], arclon[loc], 
               arclat[loc] - lats.lat, arclon[loc] - lats.lon);
      }
    }
  }

  printf("Now try to get concentrations, 0-1\n"); fflush(stdout);
  walshtof(conc, fconc);
  printf("Average, max, min %f %f %f\n", fconc.average(land), fconc.gridmax(), fconc.gridmin() );

  return 0;
} 
