#include <stdio.h>

#include "ncepgrids.h"

#define COAST 195
#define LAND 157

// Take a 12.7 km grid and put out the corresponding lake/coast tolerant 25.4
//   km grid.
int main(int argc, char *argv[]) {
  GRIDLOW<unsigned char> lores, loref;
  GRIDHIGH<unsigned char> hires;
  psgrid<unsigned char> piece;

  int i, j;
  ijpt loc, lowloc;
  ijpt lowerl, ur;
  latpt ll;
  FILE *fin, *fout, *finlo;
  palette<unsigned char> gg(19, 65);

  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  finlo = fopen(argv[3], "r");
  if (fin == (FILE *) NULL) {
    printf ("failed to open the input file %s\n", argv[1]);
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf ("failed to open the output file %s\n", argv[2]);
    return 2;
  }
  if (finlo == (FILE *) NULL) {
    printf("failed to open the reference land mask file\n", argv[3]);
    return 3;
  }
  
  hires.binin(fin);
  fclose(fin);
  loref.binin(finlo);
  fclose(finlo);
  

// If any point on the high resolution grid is not land or coast, flag
//   the corresponding low res grid point as water.  
// Future: compute percentage coverage.
  lores = loref;
  for (loc.j = 0; loc.j < hires.ypoints(); loc.j++) {
  lowloc.j = loc.j / 2;
  for (loc.i = 0; loc.i < hires.xpoints(); loc.i++) {
    lowloc.i = loc.i / 2;
    if (hires[loc] < 100) lores[lowloc] = 0;
  }
  }

  for (loc.j = 0; loc.j < lores.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lores.xpoints(); loc.i++) {
    if (lores[loc] != loref[loc]) {
      ll = lores.locate(loc);
      printf("new %3d %3d  %7.2f %5.2f  %3d %3d\n",loc.i, loc.j, 
                     ll.lon, ll.lat, 
                     (int) lores[loc], (int) loref[loc] );
    }
    if (lores[loc] == COAST) {
      ll = lores.locate(loc);
      printf("cst %3d %3d  %7.2f %5.2f  %3d %3d\n",loc.i, loc.j, 
                     ll.lon, ll.lat, 
                     (int) lores[loc], (int) loref[loc] );
    }
  }
  }

  lores.binout(fout);
  fclose(fout);

  lores.xpm("new.xpm", 14, gg);
  loref.xpm("ref.xpm", 14, gg);
  
/*
  lowerl.i = 80; lowerl.j = 0;
  ur.i = 280; ur.j = 250;
  piece.subset(lores, lowerl, ur);
  piece.xpm("new.na.xpm", 14, gg);
  piece.subset(loref, lowerl, ur);
  piece.xpm("old.na.xpm", 14, gg);

  loref -= lores;
  loref.xpm("delta.xpm", 14, gg);
  piece.subset(loref, lowerl, ur);
  piece.xpm("delta.na.xpm", 14, gg);
*/


  return 0;
}
