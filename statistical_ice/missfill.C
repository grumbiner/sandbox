#include <stdio.h>
#include <stdlib.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  northgrid<float> first, last, fill;
  FILE *fin1, *fin2, *fin3, *fout; 
  int count = 0, count2 = 0;
  ijpt loc;

//////// Process inputs
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fin3 = fopen(argv[3], "r");

  if (fin1 == (FILE *) NULL ) {
    printf("Failed to open the first data file\n");
    return 1;
  }
  if (fin2 == (FILE *) NULL ) {
    printf("Failed to open the second data file\n");
    return 1;
  }
  if (fin3 == (FILE *) NULL ) {
    printf("Failed to open the third data file\n");
    return 1;
  }

///// Start up:
  first.binin(fin1);
  fill.binin(fin2);
  last.binin(fin3);

// Interpolate to fill in missing/invalid data points
  for (loc.j = 0; loc.j < first.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < first.xpoints() ; loc.i++) {
    if (fill[loc] > 1.0) {
      if (first[loc] > 1.0 || last[loc] > 1.0) {
        //can't do anything, have missing data at hand
        count2 += 1;
      }
      else {
        fill[loc] = (first[loc] + last[loc]) / 2.0;
        count += 1;
      }
    }
   } //loc.i
   } //loc.j
       

  printf("filled %d points on %s %d remain\n",count, argv[2], count2);
  fout = fopen(argv[4],"w");
  fill.binout(fout);
  fclose(fout);

  return 0;

}
