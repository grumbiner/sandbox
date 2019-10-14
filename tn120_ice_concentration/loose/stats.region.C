#include <stdio.h>
#include <math.h>

#include "ncepgrids.h"

int main(int argc, char *argv[]) {
  FILE *fin;
  nsidcnorth<unsigned char> conc, statmask;
  nsidcnorth<int> count;
  nsidcnorth<float> sum;
  nsidcnorth<float> sumsq;
  palette<unsigned char> gg(19,65);
  
  ijpt loc;
  latpt ll;
  int i, j;

  count.set(0);
  sum.set(0.0);
  sumsq.set(0.0);
  //printf("trying to work with %d args, first being %s\n", argc, argv[0]);
  for (i = 1; i < argc; i++) {
    fin = fopen(argv[i],"r");
    //printf("%s\n",argv[i]);
    if ( fin == (FILE*) NULL) {
      printf("failed to open %s\n",argv[i]);
      continue;
    }
    j = conc.binin(fin);
    fclose(fin);
    if (j = conc.xpoints()*conc.ypoints() ) {
      for (loc.j = 0; loc.j < conc.ypoints(); loc.j++) {
      for (loc.i = 0; loc.i < conc.xpoints(); loc.i++) {
         if ((int) conc[loc] < 128 && (int) conc[loc] >= 15) {
           if ((int) conc[loc] > 100) conc[loc] = (unsigned char) 100;
           count[loc] += 1;
           sum[loc]   += (float) conc[loc];
           sumsq[loc] += conc[loc]*conc[loc];
         }
      }
      }
    }
  }

  statmask.set(0);
  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    if (count[loc] > 1) {
      #ifdef VERBOSE
      ll = sum.locate(loc);
      printf("%3d %3d %6.1f %6.1f  %3d %6.2f %6.2f %5.2f\n",
             loc.i, loc.j, ll.lat, ll.lon, 
             count[loc], sum[loc]/count[loc], sqrt(sumsq[loc]/count[loc]), 
             sqrt(((sumsq[loc] - sum[loc]*sum[loc]/count[loc]))/
                   (count[loc] -1)) );
      #endif
      sumsq[loc] = (sumsq[loc] - sum[loc]*sum[loc]/count[loc]) /
                   (count[loc] -1) ;
      sumsq[loc] = sqrt(sumsq[loc]);
      sum[loc] /= count[loc];
      if (sumsq[loc] > 40.0 || count[loc] <= 4 || sum[loc]+3.*sumsq[loc] < 40.) {
        statmask[loc] = 1;
      }
      else {
        statmask[loc] = 0;
      }

    }
    else {
      sum[loc] = 0.;
      sumsq[loc] = 0.;
      statmask[loc] = 1;
    }
    
  }
  }
  

  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    if (statmask[loc]) {
      sumsq[loc] = 0;
      sum[loc] = 0;
    }
    if (!statmask[loc]) {
      ll = sum.locate(loc);
#ifdef GEOGRAPHIC
      // East N. Atl:
      if (ll.lat < 58.0 && ll.lon > -48.0 && ll.lon < 7.) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }
      // West N. Atl:
      if (ll.lat < 45.0 && ll.lon > -75.0 && ll.lon < -40.) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }
      // Chesapeake
      if (ll.lat < 41. && ll.lon > -80.0 && ll.lon < 0.) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }

      // N. Central Pacific
      if (ll.lat < 52.0 && (ll.lon > 162.0 || ll.lon < -120.) ) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }
      // Gulf of Alaska
      if (ll.lat >= 51.0 && ll.lat < 60.5 && 
           ( (ll.lon > -155.0 && ll.lon < -120.) || 
             (ll.lon >  205.0 && ll.lon <  240.)   )   ) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }
      // Aleutians
      if (ll.lat < 55.0 && (ll.lon > 162.0 || ll.lon < -165.) ) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }

      // West Pac
      if (ll.lat < 43.0 && ll.lon > 130.) {
        statmask[loc] = 1;
        sumsq[loc] = 0;
        sum[loc]   = 0;
      }
#endif
// End of applying a geographic mask
    }
    if (!statmask[loc] ) {
      ll = sum.locate(loc);
      printf("%3d %3d %6.1f %6.1f  %3d %6.2f %6.2f\n",
             loc.i, loc.j, ll.lat, ll.lon,
             count[loc], sum[loc], sumsq[loc]);
    }
     
  }
  }

  fin = fopen("statmask.bin","w");
  statmask.binout(fin);
  fclose(fin);

  sum.scale();
  sum.xpm("avg.xpm", 7, gg);
  sumsq.scale();
  sumsq.xpm("var.xpm", 7, gg);
  statmask.scale();
  statmask.xpm("statmask.xpm",7, gg);
  count.scale();
  count.xpm("count.xpm",7,gg);
  sumsq.xpm("varmask.xpm",7,gg);
  sum.xpm("avgmask.xpm",7,gg);

  return 0;
}
