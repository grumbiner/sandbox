#include <stdio.h>

#include "mvector.h"
#include "grid_math.h"
#include "ncepgrids.h"

#define DAT_LENGTH 363
#define PARMS 15
#define SCALE 10

// Run through data list and record which points ever have an 
//  ice cover.
// Robert Grumbine 19 July 2000

int main(void) {
  global_ice<float> icegrid;
  global_ice<bool> everice;
  global_ice<int> count;
  ijpt loc;
  int tot=0, j;
  FILE *fin;

  for (loc.j = 0; loc.j < icegrid.ypoints(); loc.j += 1) {
  for (loc.i = 0; loc.i < icegrid.xpoints(); loc.i += 1) {
    everice[loc] = false;
  }
  }
  count.set(0);

// Read in data:
  fin = fopen("iceall","r");
  for (j = 0; j < DAT_LENGTH; j++) {
    icegrid.binin(fin);
    for (loc.j = 0; loc.j < icegrid.ypoints(); loc.j += 1) {
    for (loc.i = 0; loc.i < icegrid.xpoints(); loc.i += 1) {
      if (icegrid[loc] != 0) {
        everice[loc] = true;
        count[loc] += 1;
      }
    }
    }
  }

  for (loc.j = 0; loc.j < icegrid.ypoints(); loc.j += 1) {
  for (loc.i = 0; loc.i < icegrid.xpoints(); loc.i += 1) {
     if (everice[loc]) tot += 1;
     printf("%3d %3d  %d %d\n",loc.i, loc.j, (int) everice[loc], count[loc]);
  }
  }
  printf("Total number of points which ever had ice: %d\n",tot);

  return 0;
}

