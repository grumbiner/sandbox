#include <cstdio>
#include <cstdlib>
#include <cmath>

// Program for looking for qc limits on brightness temperatures
// Robert Grumbine 2016 Jan 27

#include "amsr2.h"
#include "mvector.h"
#include "ncepgrids.h"
#define land 0
//*************************************************************************************
int main(int argc, char *argv[]) {
  amsr2head  x;
  amsr2_spot s[12];

  global_12th<float> icecon, rtg;

  FILE *fin, *fout, *ice, *sst;
  int i, nobs, nread = 0, j;
  float sum;
  ijpt loc;
  fijpt floc;
  latpt ll;
  mvector<int> lrhist[12], hrhist[2];
  mvector<int> ice_lrhist[12], ice_hrhist[2];

  for (i = 0; i <  2; i++) { hrhist[i].resize(450); hrhist[i] = 0; }
  for (i = 0; i < 12; i++) { lrhist[i].resize(450); lrhist[i] = 0; }
  for (i = 0; i <  2; i++) { ice_hrhist[i].resize(450); ice_hrhist[i] = 0; }
  for (i = 0; i < 12; i++) { ice_lrhist[i].resize(450); ice_lrhist[i] = 0; }
  
  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open input satellite data file %s\n",argv[1]);
    return 1;
  }
  fout = fopen(argv[2], "w");
  if (fout == (FILE*) NULL) {
    printf("failed to open output satellite data file %s\n",argv[2]);
    return 1;
  }
  ice = fopen(argv[3], "r");
  if (ice == (FILE*) NULL) {
    printf("failed to open iceconc data file %s\n",argv[3]);
    return 1;
  }
  sst = fopen(argv[4], "r");
  if (sst == (FILE*) NULL) {
    printf("failed to open sst data file %s\n",argv[4]);
    return 1;
  }
  rtg.binin(sst); fclose(sst);
  icecon.binin(ice); fclose(ice);
  if (icecon.gridmax() < 3.0) icecon *= 100.;
  if (rtg.gridmax() > 200)    rtg     -= 273.15;

/////////////////////////////////////////////////////////////////////

  while (!feof(fin)) {
    fread(&x, sizeof(x), 1, fin); nobs = x.nspots;
    fread(&s[0], sizeof(amsr2_spot), nobs, fin);
    if (feof(fin)) continue;
    nread += 1;

// land summing:
    sum = 0;
    for (i = 0; i < nobs; i++) { sum += s[i].alfr; }
    if (x.clat < 25 && x.clat > -40.0) continue;
    if (sum == land) continue;

    ll.lat = (float) x.clat; ll.lon = (float) x.clon;
    floc = rtg.locate(ll);
    loc.i = rint(floc.i); loc.j = rint(floc.j);
    printf("%7.3f %8.3f %3.0f %6.2f ",ll.lat, ll.lon, icecon[loc], rtg[loc]);

    if (nobs == 2) {
      printf(" hr ");
      for (i = 0; i < nobs; i++) { 
        if (fabs(s[i].tmbr) < 400) {
          printf(" %6.2f ",s[i].tmbr);
          hrhist[i][(int) rint(s[i].tmbr)] += 1; 
          if (icecon[loc] != 0) ice_hrhist[i][(int) rint(s[i].tmbr)] += 1;
        }
      }
    }
    else {
      printf(" lr ");
      for (i = 0; i < nobs; i++) { 
        if (fabs(s[i].tmbr) < 400) {
          printf(" %6.2f ",s[i].tmbr);
          lrhist[i][(int) rint(s[i].tmbr)] += 1; 
          if (icecon[loc] != 0) ice_lrhist[i][(int) rint(s[i].tmbr)] += 1;
        }
      }
    }
    printf("\n");

    fflush(stdout);
  }
  return 0;

  for (i = 0; i < 2; i += 2) {
    for (j = 0; j < hrhist[i].xpoints(); j++) {
      if (hrhist[i][j] != 0) { printf("hr %1d %3d %7d %7d %7d %7d\n",
           i, j, hrhist[i][j], ice_hrhist[i][j], hrhist[i+1][j], ice_hrhist[i+1][j] ); }
    }
  }
  for (i = 0; i < 12; i += 2) {
    for (j = 0; j < lrhist[i].xpoints(); j++) {
      if (lrhist[i][j] != 0) { printf("lr %1d %3d %7d %7d %7d %7d\n",
           i, j, lrhist[i][j], ice_lrhist[i][j], hrhist[i+1][j], ice_hrhist[i+1][j] ); }
    }
  }

  return 0;
}
