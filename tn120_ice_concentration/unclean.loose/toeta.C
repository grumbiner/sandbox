//collect/combine all versions of 'toeta' from narr
//
#include <cstdio>
#include <cstdlib>

#include "eta.h"
#include "ncepgrids.h"
template <class T>
void from_nomura(nomura<T> &x, global_sst<T> &y) ;

int main(int argc, char *argv[]) {
  eta32<float> x, etaland;

// Grids for sst usage, by resolution:
  global_sst<float> sst1;
  global_ice<float> ssthalf;
  global_quarter<float> sstq;
  global_12th<float> sst5min;

// land masks for sst, ice:
  global_sst<unsigned char> land1;
  global_ice<unsigned char> landhalf;
  global_quarter<unsigned char> landq;
  global_12th<unsigned char> land5min;

// Ice grids:
  nomura<float> ice_nomura; // in legacy.h
  global_sst<float> ice1;
  global_ice<float> icehalf;
  global_quarter<float> iceq;
  global_12th<float> ice5min;

  llgrid<float> *y; // point this at proper ice grid

//////////////////////////////////////////////////////////
  fin = fopen("etaland", "r");
  etaland.binin(fin);
  fclose(fin);

  fin = fopen(argv[7], "r");
  yland.binin(fin);
  fclose(fin);

  yy = atoi(argv[3]);
  mm = atoi(argv[4]);
  dd = atoi(argv[5]);

  // Read in concentration field, ensure consistently flagged, and rescale
  flag = 157.;
  fin = fopen(argv[1], "r");
  y.binin(fin);
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
     if (y[loc] > 200.) y[loc] = flag;
  }
  }
  fflush(stdout);
  y *= 100;
  fclose(fin);

  // transfer from uchar land to float land:

  // interpolate off the source grid to the target grid:
  x.fromall(y, fland, flag, 224.);

  // loop through eta grid and do some qc checks
  for (loc.j = 0; loc.j < x.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < x.xpoints(); loc.i++) {
    if (x[loc] > 128) {
      if (x[loc] != 224) printf("reset2 %3d %3d  %f\n",loc.i, loc.j,  x[loc]);
      x[loc] = 0;
    }
    if (etaland[loc] == 1 && x[loc] != 0) {
      x[loc] = 0;
    }
  }
  }
  x /= 100.; // return to %

  // write out:
  fout = fopen(argv[2], "w");
  #ifdef IBM
    x.ftnout(fout);
  #else
    x.binout(fout);
  #endif
  fclose(fout);

  return 0;
}
// from nomura grid to mine:
template <class T>
void from_nomura(nomura<T> &x, global_sst<T> &y) {
  for (loc.j = 0; loc.j < y.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < y.xpoints(); loc.i++) {
    nloc.i = loc.i - 180;
    if (nloc.i < 0) nloc.i += fland.xpoints();
    nloc.j = yland.ypoints() - loc.j - 1;
    fland[nloc] = (float) yland[loc];
    if (fland[nloc] == 195.) fland[nloc] = flag;

    if (y[loc] >= 128.) {
      #ifdef VERBOSE
        printf("Reset point at %d %d from %f\n",loc.i, loc.j, y[loc]);
      #endif
      y[loc] = flag;
    }
  }
  }
  return;
}
