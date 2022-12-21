#include <stdio.h>
#include <stdlib.h>
#include <math.h>
  
#include "ncepgrids.h"
    
#include "time_series.h"
  
#define YEARS 90
#define LEADER 50

float bilinear(mvector<float> &x, mvector<float> &y, mvector<float> &z,
               double &a, double &b, int n) ;
void shift(mvector<float> in, int delta, mvector<float> &out) ;
void trim(mvector<float> &x, int years) ;



void soiread(time_series<float> &soi) {
  float ori[12*120];
  int i=0, year = 0;
  char lines[900];
  FILE *fin;

  fin = fopen("bas.soi.dat","r");
  if (fin == (FILE *) NULL) {
    printf("Failed to open the bas soi file\n");
    fflush(stdout);
    return ;
  }
// Page through to 1901
  while (year < 1901) {
    fgets(lines, 899, fin);
    sscanf(lines, "%d", &year);
    //printf("%s\n",lines);
  }

// Read in 1901 through 1990
  while (year < 1990) {
    //printf("%s\n",lines);
    sscanf(lines, "%d %f %f %f %f %f %f %f %f %f %f %f %f",&year, &ori[i+0],
       &ori[i+1], &ori[i+2], &ori[i+3], &ori[i+4], &ori[i+5], &ori[i+6],
       &ori[i+7], &ori[i+8], &ori[i+9], &ori[i+10], &ori[i+11] );
    i += 12;
    fgets(lines, 899, fin);
  }

  if (soi.xpoints() == 90*12) {
    for (i = 0; i < soi.xpoints() ; i++) {
      soi[i] = ori[i];
    }
  }
  else {
    for (i = 0; i < soi.xpoints() ; i++) {
      soi[i] = ori[i + (90*12 - soi.xpoints() ) ];
    }
  }

  return;
}
  
/////////////////////////////////
// Shift the series forward or backward by a given amount
void shift(mvector<float> in, int lead, mvector<float> &out) {
  int i, j;

  // lead, being pulled back to present
  // lag -- lead is negative
  out = 0.0;
  for (i = 0; i < in.xpoints() - LEADER; i++) {
    out[i] = in[i+lead+LEADER];
  }

  return;
}

// Compute a bilinear regression, return the weights
//   assumes that the mvectors have already had their means
//   removed

float bilinear(mvector<float> &x, mvector<float> &y, mvector<float> &z,
               double &a, double &b, int n) {

  double szx = 0.0, szy = 0.0;
  double syy = 0.0, sxx = 0.0, szz = 0.0;
  double sxy = 0.0;
  double delta, residue = 0.0;
  int i;

  for (i = 0; i < n; i++) {
    szx += x[i]*z[i];
    szy += y[i]*z[i];
    sxx += x[i]*x[i];
    syy += y[i]*y[i];
    sxy += x[i]*y[i];
    szz += z[i]*z[i];
  }

  //Linear:
  //a = szx / sxx;
  //b = 0.;

  //Bilinear:
  delta = sxx*syy - sxy*sxy;
  if (delta == 0.) {
    printf("singular regression, setting to zero\n");
    a = 0.;
    b = 0.;
    return 0.;
  }
  a = (syy*szy - szy*sxy) / delta;
  b = (sxx*szy - szy*sxy) / delta;

  residue = 0.0;
  for (i = 0; i < n; i++) {
    residue += (z[i] - a*x[i] - b*y[i] )*(z[i] - a*x[i] - b*y[i] );
    //residue += (z[i] - a*x[i])*(z[i] - a*x[i]);
  }
  return (1. - residue/szz);

}
// Trim series to use last N years
void trim(mvector<float> &x, int years) {
  int i;
  mvector<float> dummy(x.xpoints()-years*12);

  for (i = 0; i < dummy.xpoints(); i++) {
     dummy[i] = x[years*12+i];
  }

  x.resize(dummy.xpoints());
  for (i = 0; i < dummy.xpoints(); i++) {
    x[i] = dummy[i];
  }
  return;
}

