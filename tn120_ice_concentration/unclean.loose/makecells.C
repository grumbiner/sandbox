#include <stdio.h>

#include "ncepgrids.h"

void cellprint(northgrid<float> &x, ijpt &loc, FILE *fout) ;

int main(void) {
  northgrid<float> conc[21];
  FILE *fin, *fout;
  char fname[80];

  ijpt loc;
  int i, day, goodpts=0;
  float badval=-5;

// Read in the data fields and set all bad pts to a single value
  for (i = 0; i < 21; i++) {
    sprintf(fname,"north.9805%02d", i+1);
    fin = fopen(fname, "r");
    conc[i].binin(fin);
    fclose(fin);
    for (loc.j = 0; loc.j < conc[i].ypoints() ; loc.j++) {
    for (loc.i = 0; loc.i < conc[i].xpoints() ; loc.i++) {
      if (conc[i][loc] > 1.28) conc[i][loc] = badval;
    }
    }
  }

  fout = fopen("cellout", "w+");
// Now loop over all days from the 4th to 21st, checking if there are
//   four consecutive days of good data.
  for (day = 3; day < 21; day++) {
    // Loop over all interior points
    for (loc.j = 1; loc.j < conc[day].ypoints()-1 ; loc.j++) {
    for (loc.i = 1; loc.i < conc[day].xpoints()-1 ; loc.i++) {
       if (!conc[day].anyof(badval, 1, loc) && 
           !conc[day-1].anyof(badval, 1, loc) && 
           !conc[day-2].anyof(badval, 1, loc) &&
           !conc[day-3].anyof(badval, 1, loc)    ) {
       // Then none of the points have bad data in the 3x3 cell centered at loc
         cellprint(conc[day-3], loc, fout);
         cellprint(conc[day-2], loc, fout);
         cellprint(conc[day-1], loc, fout);
         cellprint(conc[day  ], loc, fout);
         goodpts += 1;
       }
    }
    } // end looping over points
  } // end looping over all days
    
  printf("Found %d good points to work with\n", goodpts);
  fclose(fout);
  return 0;
}

void cellprint(northgrid<float> &x, ijpt &loc, FILE *fout) {
    ijpt tloca, tlocx, delloc, base;
    grid2<unsigned char> a(3,3);
    int i, j;

    base.j = 1;
    base.i = 1;
    for (delloc.j = -1; delloc.j <= +1; delloc.j++) {
    for (delloc.i = -1; delloc.i <= +1; delloc.i++) {
       tlocx = loc + delloc;
       tloca = base + delloc;
       a[tloca] = (unsigned char) (0.5 + 100. * x[tlocx]);
    }
    }
    a.binout(fout);

    return;
} 
