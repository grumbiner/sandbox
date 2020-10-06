#include "ncepgrids.h"

float correls(GRIDTYPE<float> *history, int ndays, 
              GRIDTYPE<unsigned char> &mask, ijpt &key, GRIDTYPE<float> &r) ;

float score(GRIDTYPE<float> *history, int ndays, mvector<ijpt> &keys, 
            GRIDTYPE<float> &variance, 
            GRIDTYPE<unsigned char> &mask, float rcrit) ;


int main(int argc, char *argv[]) {
  GRIDTYPE<unsigned char> mask;
  GRIDTYPE<float> variance;
  GRIDTYPE<double> sum, sum2, tmp;
  mvector<ijpt> keys;
  GRIDTYPE<float> history[10957];
  int i, ndays = 10957;

  ijpt loc;
  float rcrit = 0.5, flag = -999.;
  FILE *fin;
  
  fin = fopen(argv[1],"r");
  mask.binin(fin);
  fclose(fin);

  sum.set(0.0);
  sum2.set(0.0);
  fin = fopen(argv[2], "r");
  for (i = 0; i < ndays; i++) {
    history[i].binin(fin);
    conv(history[i], tmp);
    for (loc.j = 0; loc.j < sum.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < sum.xpoints(); loc.i++) {
      if (tmp[loc] == flag) {
        mask[loc] = 157;
      }
    }
    }
    printf("%5d %f %f %f %f\n",i, tmp.gridmax(flag), tmp.gridmin(flag), tmp.average(flag), tmp.rms(flag) );
    sum += tmp;
    tmp *= tmp;
    sum2 += tmp;
  }
  sum /= ndays;
  printf("sum %f %f %f %f\n",sum.gridmax(flag), sum.gridmin(flag), sum.average(flag), sum.rms(flag) );
  sum2 /= ndays;
  printf("sum2 %f %f %f %f\n",sum2.gridmax(flag*flag), sum2.gridmin(flag*flag), sum2.average(flag*flag), sum2.rms(flag*flag) );

  for (loc.j = 0; loc.j < sum.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < sum.xpoints(); loc.i++) {
    variance[loc] = sum2[loc] - ndays*sum[loc]*sum[loc]; 
  }
  }
  printf("variance grid: %f %f %f %f\n",variance.gridmax(), variance.gridmin(), variance.average(), variance.rms() );

  return 1;
// create keys --
  score(history, ndays, keys, variance, mask, rcrit);

  return 0;
}
// given a set of key points, compute the score (= variance explained
float score(GRIDTYPE<float> *history, int ndays, mvector<ijpt> &keys, 
            GRIDTYPE<float> &variance, 
            GRIDTYPE<unsigned char> &mask, float rcrit) {
  float sum = 0.0;
  mvector<float> totals[keys.xpoints() ];
  ijpt loc;
  int i;

  // correlation grids for the N key points
  GRIDTYPE<float> correlations[keys.xpoints() ];  
  GRIDTYPE<short int> best, zone;

  zone.set(0);
  best.set(0);

// find the n correlation grids:
  for (i = 0; i < keys.xpoints(); i++) {
    correls(history, ndays, mask, keys[i], correlations[i]);
  }
// find the best match points == highest (most positive) correlations: 
  for (i = 1; i < keys.xpoints(); i++) { 
    for (loc.j = 0; loc.j < best.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < best.xpoints(); loc.i++) {
      if (correlations[i][loc] > correlations[best[loc] ][loc]) {
        best[loc] = i;
      }
    }
    }
  } 
// now flood out from keys for all contiguous points that are 'owned' by it, and
//   which have r > rcrit, and tag/flag by
// flood(i, key[i], correlation[i], best, zone, rcrit)
  for (i = 0; i < keys.xpoints(); i++) {
    sum = 0.0;
    for (loc.j = 0; loc.j < best.xpoints(); loc.j++) {
    for (loc.i = 0; loc.i < best.xpoints(); loc.i++) {
      if ((best[loc] == i) && (mask[loc] == 0) )  {
        sum += best.cellarea(loc)*variance[loc]*
           (correlations[i][loc]*correlations[i][loc]); // area * variance explained
      } 
    }
    }
    totals[i] = sum;
  }
  sum = 0.0;
  for (i = 0; i < keys.xpoints(); i++) {
    sum += totals[i];
  }

  return sum;
}
float correls(GRIDTYPE<float> *history, int ndays, GRIDTYPE<unsigned char> &mask, ijpt &key, GRIDTYPE<float> &r) {
  ijpt loc;
  GRIDTYPE<double> sumx, sumx2, sumxy, sumy, sumy2;
  GRIDTYPE<float> y;
  int i;

  sumx.set(0.0);
  sumy.set(0.0);
  sumxy.set(0.0);
  sumx2.set(0.0);
  sumy2.set(0.0);
  r.set((float) 0.0);

// virtually, there's an array of all observations through all time here
//
  
  for (i = 0; i < ndays; i++) {
    for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
      y[loc] = history[i][loc];
    }
    }
//////////////// --- access y here /////////////////////////////////
// don't need to multiply compute the key pts' sums
    sumx[key] += y[key];
    sumx2[key] += y[key];

    for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
    for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
      if (mask[loc] == 0 && (loc != key) ) {
        sumy[loc]  += y[loc];
        sumy2[loc] += y[loc]*y[loc];
        sumxy[loc] += y[loc]*y[key];
      }
    }
    }
  }

  for (loc.j = 0; loc.j < mask.ypoints(); loc.j++) {
  for (loc.i = 0; loc.i < mask.xpoints(); loc.i++) {
    if (mask[loc] == 0) {
      r[loc] = (    ndays*sumxy[loc] - sumx[loc]*sumy[loc]) / 
               sqrt(ndays*sumx2[loc] - sumx[loc]*sumx[loc]) / 
               sqrt(ndays*sumy2[loc] - sumy[loc]*sumy[loc]); 
    }
  }
  }

  return r[key];
}
