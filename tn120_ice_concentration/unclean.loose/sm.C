#include <stdio.h>

#include "ncepgrids.h"

#define COAST 195
#define LAND 157
#define MAX_CONC 128

// Take a 12.7 km grid of ice concentrations and put out the corresponding 
//    lake/coast tolerant 25.4 km grid.

int main(int argc, char *argv[]) {
  GRIDLOW<unsigned char> lotmp, loref, count, mask;
  GRIDLOW<int> lores;
  GRIDHIGH<unsigned char> hires;

  int i, j;
  ijpt loc, lowloc;
  ijpt lowerl, ur;
  latpt ll;
  FILE *fin, *fout, *finlo, *fmask;
  palette<unsigned char> gg(19, 65);
  float sum = 0., sumsq = 0.;
  int equal = 0, tally=0;

  fin = fopen(argv[1], "r");
  fout = fopen(argv[2], "w");
  finlo = fopen(argv[3], "r");
  fmask = fopen(argv[4], "r");

  if (fin == (FILE *) NULL) {
    printf ("failed to open the hires ice file %s\n", argv[1]);
    return 1;
  }
  if (fout == (FILE *) NULL) {
    printf ("failed to open the output file %s\n", argv[2]);
    return 2;
  }
  if (finlo == (FILE *) NULL) {
    printf("failed to open the old lores ice file\n", argv[3]);
    return 3;
  }
  if (fmask == (FILE *) NULL) {
    printf("failed to open the lores land mask file\n", argv[4]);
    return 4;
  }
  /* printf("finished opening files\n"); fflush(stdout); */
  
  hires.binin(fin);
  fclose(fin);
  loref.binin(finlo);
  fclose(finlo);
  mask.binin(fmask);
  fclose(fmask);
  
  /* printf("finished reading files\n"); fflush(stdout); */

// If any point on the high resolution grid is not land or coast, flag
//   the corresponding low res grid point as water.  
// Future: compute percentage coverage.

  count.set((unsigned char) 0);

  for (loc.j = 0; loc.j < hires.ypoints(); loc.j++) {
  lowloc.j = loc.j / 2;
  for (loc.i = 0; loc.i < hires.xpoints(); loc.i++) {
    lowloc.i = loc.i / 2;
    if (hires[loc] < MAX_CONC && mask[lowloc] != LAND ) { 
      if (hires[loc] > 100) {
        lores[lowloc] += 100;
      }
      else {
        lores[lowloc] += hires[loc];
      }
      count[lowloc] += 1;
    }
  }
  }

  for (loc.j = 0; loc.j < lores.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lores.xpoints(); loc.i++) {
    if (count[loc] != 0 ) {
      lores[loc] /= count[loc];
      if (lores[loc] < MIN_CONC) lores[loc] = 0;
    }
    else {
      lores[loc] = NO_DATA;
    }
    if (mask[loc] == LAND || mask[loc] == COAST) {
      loref[loc] = mask[loc];
      lores[loc] = mask[loc];
    }
    if ((int) loref[loc] > (int) 100 && 
        (int) loref[loc] < (int) MAX_CONC) loref[loc] = 100;

    /* if ((int) lores[loc] != (int) loref[loc] && */ 
     if( (int) lores[loc] < (int) MAX_CONC    && 
         (int) loref[loc] < (int) MAX_CONC)       {
      ll = lores.locate(loc); 
      printf("%3d %3d  %7.2f %5.2f  %3d %3d  %4d\n",loc.i, loc.j, 
                     ll.lon, ll.lat, 
                     (int) lores[loc], (int) loref[loc], (int) lores[loc]-(int) loref[loc] ); 
    }
    else { 
      if ( (int) lores[loc] < MAX_CONC && (int) loref[loc] < MAX_CONC)       {
        equal += 1;
      }
    }
    lotmp[loc] = (unsigned char) lores[loc];
  }
  }

  lotmp.binout(fout);
  fclose(fout);

  lores.xpm("new.xpm", 14, gg);
  loref.xpm("ref.xpm", 14, gg);
  
  for (loc.j = 0; loc.j < lores.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < lores.xpoints(); loc.i++) {
    ll = loref.locate(loc);
    if ((int) lores[loc] <  MAX_CONC && (int) loref[loc] <  MAX_CONC &&
         fabs(ll.lat) > 50.0 ) {
      tally += 1;
      lores[loc] -= loref[loc];
      sum   += lores[loc];
      sumsq += lores[loc]*lores[loc];
      loref[loc] = (unsigned char) (128 + lores[loc]);
    }
  }
  } 
  fout = fopen("delta.out","w");
  loref.binout(fout);
  fclose(fout);
  printf("%d points of %d were equal\n",equal, tally);
  printf("bias, rms, sigma = %f %f  %f\n",sum/tally, sqrt(sumsq/tally), 
                sqrt(sumsq - sum*sum/tally)/sqrt(tally) );

  loref.xpm("delta.xpm", 14, gg);

  return 0;
}
