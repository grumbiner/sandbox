#include "mvector.h"

// Compute a bilinear regression, return the weights
//   assumes that the mvectors have already had their means
//   removed

float bilinear(mvector<float> &x, mvector<float> &y, mvector<float> &z, 
               float &a, float &b, int n) {

  double szx = 0.0, szy = 0.0;
  double syy = 0.0, sxx = 0.0, szz = 0.0;
  double sxy = 0.0;
  double delta, residue = 0.0;
  int i;

  x -= x.average();
  y -= y.average();
  z -= z.average();
  for (i = 0; i < n; i++) {
    szx += x[i]*z[i];
    szy += y[i]*z[i];
    sxx += x[i]*x[i];
    syy += y[i]*y[i];
    sxy += x[i]*y[i];
    szz += z[i]*z[i];
  }

  delta = sxx*syy - sxy*sxy;
  if (delta == 0.) {
    printf("singular regression, setting to zero\n");
    a = 0.;
    b = 0.;
    return 0.;
  }

  a = (szy*syy - szy*sxy) / delta;
  b = (sxx*szy - sxy*szy) / delta;
  for (i = 0; i < n; i++) {
    residue += (z[i] - a*x[i] - b*y[i] )*(z[i] - a*x[i] - b*y[i] );
  }
  return (1. - residue/szz); 
  
} 
