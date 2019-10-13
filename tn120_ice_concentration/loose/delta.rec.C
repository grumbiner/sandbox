#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

#define LAND 157
#define COAST 195
#define WEATHER 177
#define NO_DATA 224

int main(int argc, char *argv[]) {
  global_ice<unsigned char> newice, land;
  global_ice<float> oldice, flnew;
  FILE *newin, *oldin, *fout;
  int diff_count, tag;
  char fname[90];
  ijpt x;
  latpt y;
  palette<unsigned char> gg(19, 65);

  newin = fopen(argv[1], "r");
  oldin = fopen(argv[2], "r");
  newice.binin(newin);
  oldice.binin(oldin);
  fclose(newin);
  fclose(oldin);

  newin  = fopen(argv[3], "r");
  land.binin(newin);
  fclose(newin);

  fout = fopen(argv[4], "w");
  tag = atoi(argv[5]);

  printf("new ice max, min, average %d %d %d\n",newice.gridmax(), newice.gridmin(), newice.average() );

  printf("old ice max, min, average %f %f %f\n",oldice.gridmax(), oldice.gridmin(), oldice.average() );

// Ensure that the scaling is the same (0-1.)
  if (oldice.average() > 3.0 ) oldice /= 100.;
  
// Now loop over the globe and print out the differences if they're large
  diff_count = 0;
  for (x.j = 0; x.j < land.ypoints() ; x.j++) {
    for (x.i = 0; x.i < land.xpoints() ; x.i++) {
      flnew[x] = (float)newice[x];
      flnew[x] /= 100.;
      if (fabs(flnew[x] - oldice[x]) > 0.02 && (int) land[x] != LAND ) {
        diff_count += 1;
        y = flnew.locate(x);
        fprintf(fout, "Mismatch %7.3f %7.3f  %5.3f %5.3f %3d\n",y.lat, y.lon, 
                       flnew[x], oldice[x], (int) land[x]);
      }
      if ((int) land[x] == LAND || (int) land[x] == COAST ) {
        flnew[x] = 0.;
        oldice[x] = 0.;
        newice[x] = (unsigned char) 0;
      }
    }
  }

//Print out general area diagnostic
  printf("%d different points, Ice area in old, new: %f %f\n",
             diff_count, oldice.integrate()/1e6, flnew.integrate()/1e6 );

// Print out xpm files for graphic comparisons
  sprintf(fname, "new.%d.xpm", tag);
  for (x.j = 0; x.j < land.ypoints() ; x.j++) {
    for (x.i = 0; x.i < land.xpoints() ; x.i++) {
       if (land[x] == LAND) {
         newice[x] = 0;
       }
       else if (land[x] == COAST) {
         newice[x] = 0;
       }
       else if (newice[x] == NO_DATA) {
         newice[x] = 2;
       }
       else if (newice[x] == WEATHER) {
         newice[x] = 3;
       }
       else {
         newice[x] = 4 + min((unsigned char) 100,newice[x])/7;
       }
    }
  }
  newice.xpm(&fname[0], 1, gg);

  for (x.j = 0; x.j < land.ypoints() ; x.j++) {
    for (x.i = 0; x.i < land.xpoints() ; x.i++) {
       newice[x] = (unsigned char) (0.5 + 100.*oldice[x]) ;
       if (land[x] == LAND) {
         newice[x] = 0;
       }
       else if (land[x] == COAST) {
         newice[x] = 0;
       }
       else if (newice[x] == NO_DATA) {
         newice[x] = 2;
       }
       else if (newice[x] == WEATHER) {
         newice[x] = 3;
       }
       else {
         newice[x] = 4 + min((unsigned char) 100,newice[x])/7;
       }
    }
  }
  sprintf(fname, "old.%d.xpm", tag);
  newice.xpm(&fname[0], 1, gg);

  fclose(fout);
  
  return 0;
}
