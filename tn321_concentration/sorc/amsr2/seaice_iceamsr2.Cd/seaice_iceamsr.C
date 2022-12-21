#include <cstdio>
#include <cstdlib>
#include <cmath>

#ifndef AMSR2_INCLUDE
  #include "amsr2.h"
#endif
#ifndef NCEPGRIDS
  #include "ncepgrids.h"
#endif
#include "amsr2_team2.C"

#define land 0
void scanner(FILE *fin, FILE *fout, metricgrid<unsigned char> &mgrid, 
                   grid2<amsr2_hrpt> &hrgrid, grid2<amsr2_lrpt> &lrgrid) ;
bool qc(int nobs, amsr2_spot *s, amsr2head &x, int mask) ;


void regress_l(float *v19, float *h19, float *v24, float *v37, float *h37) ;
void regress_h( float *v89, float *h89);

float weather(float v19, float h19, float v24, float h24, float v37, float h37, float v89, float h89, int satno);

//float nasa_team2(float v19, float h19, float v24, float v37, float h37,
//                 float v89, float h89, amsr_team2_tables &tab) ;

//////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[]) {
  FILE *fin, *fout[4], *fland;
  char fname[90];

// declare target grids -- double them as land mask grids
  northhigh<unsigned char> nhh;
  southhigh<unsigned char> shh;
  //walcc<unsigned char>     alaska(20.); // 20th of a degree
  walcc<unsigned char>     alaska(2.); // 1/2 of a degree
  great_lakes_wave<unsigned char> glwave;

// vectors of grids for working on/with:
  mvector<metricgrid<unsigned char>* > mgrids(4);
  grid2<amsr2_hrpt>  hrgrids;
  grid2<amsr2_lrpt>  lrgrids;

  mgrids[0] = &nhh;
  mgrids[1] = &shh;
  mgrids[2] = &alaska;
  mgrids[3] = &glwave;

// Open data files:
  fin = fopen(argv[1], "r");
  if (fin == (FILE*) NULL) {
    printf("failed to open input satellite data file %s\n",argv[1]);
    return 1;
  }
  for (int i = 0; i < 4; i++) {
    sprintf(fname, "%s.%d",argv[4],i);
    fout[i] = fopen(fname, "w");
    if (fout[i] == (FILE*) NULL) {
      printf("failed to open output satellite data file %s %d\n",argv[4], i);
      return 1;
    }
  }

// Read in land mask files:
  fland = fopen(argv[2], "r");
  nhh.binin(fland);
  fclose(fland);
  fland = fopen(argv[3], "r");
  shh.binin(fland);
  fclose(fland);
  // ignore the other grids for now
  alaska.set(0);
  glwave.set(0);
  

// Now read (scan) the input files for data to work with on the given
//   grids.
// Scanner does its own rewind, and writes out data used for each grid 
// -- this in place of old process_bufr because the amsr2 are written by
//   point, separately for high frequency (89 GHz) and low freq (the rest).

  for (int i = 0 ; i < 4; i++) {
    hrgrids.resize(mgrids[i]->xpoints(), mgrids[i]->ypoints() );
    lrgrids.resize(mgrids[i]->xpoints(), mgrids[i]->ypoints() );
    scanner(fin, fout[i], *mgrids[i], hrgrids, lrgrids);
    rewind(fout[i]);


    fclose(fout[i]);
  }

  fclose(fin);

  return 0;
}
//////////////////////////////////////////////////////////////////////////
void scanner(FILE *fin, FILE *fout, metricgrid<unsigned char> &mgrid, 
                 grid2<amsr2_hrpt> &hrgrid, grid2<amsr2_lrpt> &lrgrid) {
  amsr2head  x;
  amsr2_hrpt hr;
  amsr2_lrpt lr;
  amsr2_spot s[12];

  int i, nobs, gridskip = 0, nuse = 0, nread = 0, mask = land;
  fijpt loc;
  ijpt iloc;
  latpt ll;

  rewind(fin);
  while (!feof(fin)) {
    fread(&x, sizeof(x), 1, fin);
    nobs = x.nspots;

    fread(&s[0], sizeof(amsr2_spot), nobs, fin);
    if (feof(fin)) continue;

    nread += 1;
// do some qc
    if (!qc(nobs, s, x, mask)) continue;

    if (nobs == 2) {
      hr.head = x;
      for (i = 0; i < nobs; i++) { hr.obs[i] = s[i]; }
    }
    else {
      lr.head = x;
      for (i = 0; i < nobs; i++) { lr.obs[i] = s[i]; }
    }

    ll.lat = (float) x.clat;
    ll.lon = (float) x.clon;
    loc = mgrid.locate(ll);
    iloc.i = rint(loc.i);
    iloc.j = rint(loc.j);
    if (mgrid.in(loc)) {
      if (mgrid[loc] != 0 ) {
        printf("%7.3f %8.3f land mismatch %3d static\n",ll.lat, ll.lon, mgrid[loc]);
      }

      nuse += 1;
      // do something useful
      fwrite(&x, sizeof(x), 1, fout);
      fwrite(&s[0], sizeof(amsr2_spot), nobs, fout);
      if (nobs == 2) {
        hrgrid[loc] = hr;
      }
      else {
        lrgrid[loc] = lr;
      }
    }
    else {
      gridskip += 1;
    }
  }

  printf("nread = %d nuse = %d gridskip = %d\n",nread, nuse, gridskip);

  return ;
}
bool qc(int nobs, amsr2_spot *s, amsr2head &x, int mask) {
  int i;
  float sum;

  if (x.clat < 25 && x.clat > -40.0) return false;

  sum = 0.;
  for (i = 0; i < nobs; i++) { sum += s[i].alfr; }
  if (sum == mask) return false;

// qc against ranges:
  if (nobs == 2) {
    if (s[0].tmbr < 145 || s[0].tmbr > 273) return false;
    if (s[1].tmbr < 145 || s[1].tmbr > 273) return false;
  }
  else {
    if (s[0].tmbr  <  70 || s[0].tmbr  > 273) return false;
    if (s[1].tmbr  < 150 || s[1].tmbr  > 273) return false;
    if (s[2].tmbr  <  80 || s[2].tmbr  > 273) return false;
    if (s[3].tmbr  < 165 || s[3].tmbr  > 273) return false;
    if (s[4].tmbr  <  80 || s[4].tmbr  > 273) return false;
    if (s[5].tmbr  < 170 || s[5].tmbr  > 273) return false;
    if (s[6].tmbr  <  90 || s[6].tmbr  > 273) return false;
    if (s[7].tmbr  < 180 || s[7].tmbr  > 273) return false;
    if (s[8].tmbr  < 115 || s[8].tmbr  > 273) return false;
    if (s[9].tmbr  < 195 || s[9].tmbr  > 273) return false;
    if (s[10].tmbr < 125 || s[10].tmbr > 273) return false;
    if (s[11].tmbr < 160 || s[11].tmbr > 273) return false;
  }

// check that H < V
  for (i = 0; i < nobs; i += 2) {
    if (s[i].tmbr > s[i+1].tmbr) return false;
  }

  return true;
}   
// Regress AMSR2 Tb back to AMSR-E using Walt Meier corrections, 2014.
void regress_l(float *v19, float *h19, float *v24, float *v37, float *h37) {
// new value = a + b * old
  float a[5] = {};
  float b[5] = {};



  return;
}
void regress_h(float *v89, float *h89) {
// new value = a + b * old
  float a[5] = {};
  float b[5] = {};
  

  return;
}
// weather/ocean/ice filtering:
float weather(float v19, float h19, float v24, float h24, float v37, float h37, 
                 float v89, float h89, int satno) {


  return WEATHER;
}
