#include <stdio.h>

#include "ncepgrids.h"
#define MAX_CONC 128
#define MIN_CONC  15


int main(int argc, char *argv[]) {
  nsidcnorth<unsigned char> conc, last, mask;
  nsidcnorth<int> count, flop, fcount, allcount;
  nsidcnorth<float> delta, deltasq, sum, sumsq;

  FILE *fin;
  ijpt loc;
  latpt ll;
  int i;
  palette<unsigned char> gg(19,65);

//  fin = fopen("statmask.bin","r");
//  mask.binin(fin);
//  fclose(fin);
  mask.set(0);

  count.set(0);
  flop.set(0);
  allcount.set(0);
  delta.set(0.0);
  deltasq.set(0.0);
  sum.set(0.0);
  sumsq.set(0.0);

  fin = fopen(argv[1],"r");
  last.binin(fin);
  fclose(fin);

  for (i = 2; i < argc; i++) {
    fin = fopen(argv[i], "r");
    conc.binin(fin);
    fclose(fin);

    for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
      if (mask[loc] != 1) {
// Look for flopping:
        if (conc[loc] >= MIN_CONC && conc[loc] <= MAX_CONC 
            && (last[loc] < MIN_CONC || last[loc] > MAX_CONC) ) {
          flop[loc] += 1;
        } // gained cover
        if (conc[loc] < MIN_CONC || conc[loc] > MAX_CONC 
            && (last[loc] >= MIN_CONC && last[loc] <= MAX_CONC))  {
          flop[loc] += 1;
        } // lost cover

// Examine retention
        if (conc[loc] >= MIN_CONC && conc[loc] <= MAX_CONC
            && (last[loc] >= MIN_CONC && last[loc] <= MAX_CONC) ) {
          // retained cover
          delta[loc] += (conc[loc] - last[loc]);
          deltasq[loc] += (conc[loc] - last[loc])*(conc[loc] - last[loc]);
          sum[loc] += conc[loc];
          sumsq[loc] += conc[loc]*conc[loc];
          count[loc] += 1;
        }

// Bulk stats
        if (conc[loc] >= MIN_CONC && conc[loc] <= MAX_CONC) {
          allcount[loc] += 1;
        }
      }
    }
    } // finished looping over all points

    last = conc;
  } // go back for next file


  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (!mask[loc] && allcount > 1) {
      ll = mask.locate(loc);
      printf("%3d %3d  %5.1f %6.1f  %3d %3d %3d  %f\n",
        loc.i, loc.j, ll.lat, ll.lon,
        flop[loc], count[loc], allcount[loc], 
        (float)flop[loc]/(float) allcount[loc]
      ); 
    }
  }
  }

  flop.scale();
  flop.xpm("flop.xpm",7, gg);

  return 0;
}
