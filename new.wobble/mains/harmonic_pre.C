#include "ncepgrids.h"

#define PERIOD 365.25
#define NPER    6

int main (int argc, char *argv[]) {
  FILE *fin;
  global_quarter<float> sst;
  global_quarter<int> count, ndays;
  global_quarter<float> high, low, dhigh, dlow;
  global_quarter<double> sx, sx2, sx3, sx4;

  #ifdef HARMONIC
  mvector<global_quarter<double> > sum_sin(NPER), sum_cos(NPER);
  #endif
  double fsin[NPER], fcos[NPER];

  ijpt loc;
  double tmp, offset = 273.15;
  float tcrit = offset + 2.00;
  int i, j;

// -------------------------------------------

  fin = fopen(argv[1],"r");
  sst.binin(fin);
  fclose(fin);

  if (argc == 3) {
    fin = fopen(argv[2],"r+");
    count.binin(fin);
    ndays.binin(fin);
    high.binin(fin);
    low.binin(fin);
    dhigh.binin(fin);
    dlow.binin(fin);
    sx.binin(fin);
    sx2.binin(fin);
    sx3.binin(fin);
    sx4.binin(fin);
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
      sum_sin[i].binin(fin);
      sum_cos[i].binin(fin);
    }
    #endif
  }
  else {
    fin = fopen("count","w");
    count.set(0);
    ndays.set(0);
    high.set((float) -400.0);
    low.set((float) 400.0);
    dhigh.set((float) 0.0);
    dlow.set((float) 0.0);
    sx.set( 0.0);
    sx2.set( 0.0);
    sx3.set( 0.0);
    sx4.set( 0.0);
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
      sum_sin[i].set( 0.0);
      sum_cos[i].set( 0.0);
    }
    #endif
  }

// Have now read in or initialized all grids
// SST processing:
  count += 1;
  int step = count.gridmax();
  // For fourier analysis:
  for (i = 1; i <= NPER; i++) {
    fsin[i-1] = sin(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
    fcos[i-1] = cos(2.*M_PI*i*step / PERIOD) * 2./PERIOD;
  }

  for (loc.j = 0; loc.j < count.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < count.xpoints(); loc.i++) {
    tmp = (double) sst[loc] - offset;
    sx[loc] += tmp;
    //if (ice[loc] > 0.0 && ice[loc] <= 1.28) count[loc] += 1;
    if (sst[loc] >= tcrit) ndays[loc] += 1; 
    if (sst[loc] > high[loc]) {
      high[loc] = sst[loc];
      dhigh[loc] = count[loc];
    }
    if (sst[loc] < low[loc]) {
      low[loc] = sst[loc];
      dlow[loc] = count[loc];
    }
    sx2[loc] += tmp*tmp;
    sx3[loc] += tmp*tmp*tmp;
    sx4[loc] += tmp*tmp*tmp*tmp;
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
      sum_sin[loc][i] += tmp*fsin[i];
      sum_cos[loc][i] += tmp*fcos[i];
    }
    #endif

  }
  }

  rewind(fin);
    count.binout(fin);
    ndays.binout(fin);
    high.binout(fin);
    low.binout(fin);
    dhigh.binout(fin);
    dlow.binout(fin);
    sx.binout(fin);
    sx2.binout(fin);
    sx3.binout(fin);
    sx4.binout(fin);
    #ifdef HARMONIC
    for (i = 0; i < NPER; i++) {
      sum_sin[i].binout(fin);
      sum_cos[i].binout(fin);
    }
    #endif
  fclose(fin);

  return 0;
}
