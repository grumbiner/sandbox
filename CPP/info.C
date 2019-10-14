#include <math.h>
#include "mvector.h"
// Compute Shannon Information in an integer vector
// Robert Grumbine 30 Jul 2009
//
float information(mvector<int> &x) {
  int i, found=0;
  float p, info;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) found += 1;
  }
  p = (float) found / (float) x.xpoints() ;
  info  = -   p    * log10(p)    / log10(2.);
  info += - (1.-p) * log10(1.-p) / log10(2.);
  return info;
}

