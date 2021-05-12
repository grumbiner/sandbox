#include "ncepgrids.h"


// Construct low res 'skip' grid from high res
//  derived from concentration resolution reducer
// Take information on a high resolution grid and transfer as much as
//   possible to a low resolution grid.  Assumes that the grids are
//   integer multiples of each other.
// Robert Grumbine 30 April 2004

int main(int argc, char *argv[]) {
  GRIDLOW<unsigned char> mask;
  GRIDHIGH<unsigned char> himask;

  ijpt loc, lowloc;
  FILE *fin, *fout;

  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");

  if (fin == (FILE *) NULL) {
    printf ("failed to open the hires skip file %s\n", argv[1]);
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf ("failed to open the output file %s\n", argv[2]);
    return 2;
  }
  
  himask.binin(fin);
  fclose(fin);
  printf("himask max, min, avg = %d %d %d\n",himask.gridmax(), himask.gridmin(), himask.average() );
  fflush(stdout);

// If any point on the high resolution grid is not land or coast, use
//   its concentration for the low resolution grid.

  int ratio = himask.xpoints() / mask.xpoints();
  mask.set((unsigned char) 1);

  for (loc.j = 0; loc.j < himask.ypoints(); loc.j++) {
  lowloc.j = loc.j / ratio;
  for (loc.i = 0; loc.i < himask.xpoints(); loc.i++) {
    lowloc.i = loc.i / ratio;
    if (himask[loc] == 0) {
      mask[lowloc] = 0;
    } 
  }
  }
  mask.binout(fout);
  fclose(fout);

  return 0;
}
