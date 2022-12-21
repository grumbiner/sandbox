#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ncepgrids.h"
#include "subs.C"

// Non-genetic (honest) variant to map out the entire parameter
//  space using just the lags and a bilinear regression

#define YEARS 90
#define LEADER 50

int main(int argc, char *argv[]) {
  mvector<float> icec(YEARS*12), iceref(YEARS*12), soiref(YEARS*12);
  mvector<float> soipred(YEARS*12), icepred(YEARS*12), iceshift(YEARS*12);
  GRIDTYPE<float> fullgrid;
  mvector<float> avgs(12), soiavg(12);
  int i, lead;
  int lagice, lagsoi;
  double a, b;
  float score;
  FILE *fin;
  ijpt loc;
  fijpt floc;
  latpt ll;

  // Get hold of the ice concentrations:
  fin = fopen(argv[1], "r");
  ll.lat = atof(argv[2]);
  ll.lon = atof(argv[3]);
  floc = fullgrid.locate(ll);
  loc = floc;
  printf("Location is %f %f  %d %d\n",ll.lat, ll.lon, loc.i, loc.j);
  for (i = 0; i < icec.xpoints(); i++) {
    fullgrid.binin(fin);
    icec[i] = fullgrid[loc];
  }
  fclose(fin);
  
  // Remove the monthly averaged values in icec so as to work with
  // ice concentration anomalies:
  avgs = (float) 0.0;
  soiavg = (float) 0.0;
  for (i = 0 ; i < icec.xpoints(); i++) {
    avgs[i%12] += icec[i];
  }
  avgs /= YEARS;
  soiavg /= YEARS;
  for (i = 0; i < icec.xpoints(); i++) {
    icec[i] -= avgs[i%12];
  }
  printf("Seasonal cycle in icec and soi\n");
  for (i = 0; i < 12; i++) {
    printf("%2d %f\n",i+1, avgs[i]);
  } 

// Trim down to the last 40 years -- less filling done
  trim(icec, 40);

  for (lead = 1; lead < LEADER; lead++) {
    //printf("lead = %d\n",lead); fflush(stdout);
    shift(icec, lead, iceshift);
    iceshift -= iceshift.average();
    //iceref = iceshift;
    for (lagice = 0; lagice < LEADER-1; lagice++) {
      shift(icec, -lagice, icepred); 
      icepred -= icepred.average();
      //soiref = icepred;
      for (lagsoi = lagice+1; lagsoi < LEADER; lagsoi++) {
        shift(icec, -lagsoi, soipred);

        score = bilinear(icepred, soipred, iceshift, a, b, icec.xpoints() - LEADER*2);
    
        if (score >= 0.10) {
          printf("lead %2d ice lag %2d lagsoi %2d  %f %f %f\n",
                  lead, lagice, lagsoi, score, a, b);
          fflush(stdout);
        }
      } //looping over soi lags
    }
  }

  printf("Finished checking\n"); fflush(stdout);

  return 0;

}
