#include "ncepgrids.h"

// Compute the area-weighted global rms of sst between two fields
// This one is specialized towards one being in the reynolds short int
//   format, and the other being floating point in my format
// Robert Grumbine 1 November 2011

// Variant for trend-based climo files:
//    do the rescaling directly on input


int main(int argc, char *argv[]) {
  FILE *fin_climo, *fin_day;
  // -999 is reynolds flag value for no info
  int x[4]; // extra is for the ftn unformatted header
  global_quarter<short int> day;
// climo = a + b*jday
  global_quarter<float> climo, climo_a, climo_b, delta, mask;
  double sum = 0.0, sumsq = 0.0, sum3 = 0.0, sum4 = 0.0, totarea;
  float jday;

  ijpt loc, tloc;
 
  fin_climo  = fopen(argv[1], "r");
  climo.binin(fin_climo);
  climo_b.binin(fin_climo); 
  climo_a.binin(fin_climo);
  fclose(fin_climo);
  mask.set((float) 1.0);

  fin_day  = fopen(argv[2], "r");
  fread(&x, sizeof(int), 4, fin_day);
  day.binin(fin_day);
  fclose(fin_day);

// climatology with trend:
  jday = atof(argv[3]);
  delta = climo_b;
  delta *= jday;
  delta += climo_a;

  // flip in to my convention and subtract:
  for (loc.j = 0; loc.j < day.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < day.xpoints(); loc.i++) {
    tloc.j = day.ypoints() - loc.j - 1;
    tloc.i = loc.i;
    if (delta[tloc] != -9.99 && day[loc] != -999) {
      // -1 mult so as to be printing how much warmer than climo the day is
      delta[tloc] -= day[loc]/100; delta[tloc] *= -1; 
      sum   += delta.cellarea(tloc) * delta[tloc];
      sumsq += delta.cellarea(tloc) * delta[tloc]*delta[tloc];
      sum3  += delta.cellarea(tloc) * delta[tloc]*delta[tloc]*delta[tloc];
      sum4  += delta.cellarea(tloc) * delta[tloc]*delta[tloc]*delta[tloc]*delta[tloc];
    }
    else {
      delta[tloc] = -99999;
      mask[tloc] = 0.0;
    }
  }
  }

  totarea = mask.integrate();
  
  float rms = sqrt(sumsq/totarea);
  float mean = sum/totarea;
  float skew = sum3/totarea / pow((double) sumsq/totarea, (double) 1.5);
  float kurt = sum4/totarea / (sumsq/totarea * sumsq/totarea) - 3.0;
  printf("jday %5.0f mean %6.3f rms %7.4f skew %f kurtosis %f\n",jday, mean, rms, skew, kurt);

  return 0;
}
