//30 July 1999
#include "mvector.h"

float information(mvector<int> &x) ;
float information(mvector<int> &x, int cats) ;

// Compute the Shannon information of a (presumed) binary series
float information(mvector<int> &x) {
  int i, found=0;
  float p, info;
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] != 0) found += 1;
  }
  p = (float) found / (float) x.xpoints() ;
  if (p == 0. || p == 1.) return 0.;
  info  = -   p    * log10(p)    / log10(2.);
  info += - (1.-p) * log10(1.-p) / log10(2.);
  return info;
}
// Compute info for category ranges 0 - cats
float information(mvector<int> &x, int cats) {
  int i;
  mvector<int> found(cats);
  float p, info;
  for (i = 0; i < x.xpoints(); i++) {
    found[ x[i] ] += 1;
  }
  info = 0.;
  for (i = 0; i < cats; i++) {
    if (found[i] != 0) {
      p = (float) found[i] / (float) x.xpoints() ;
      info +=  -  p * log10(p) / log10(2.);
    }
  }
  return info;
}
