//11 March 2001

#include "mvector.h"
void sledgeft(mvector<float> &test, mvector<float> &sincoef, mvector<float> &coscoef) ;
void toampl(mvector<float> &sincoef, mvector<float> &coscoef,
            mvector<float> &ampl, mvector<float> &phase);

extern int PERIOD;
extern mvector<mvector<float> > sines, cosines;
template <class T>
extern float dot(mvector<T> &, mvector<T>&y);

// Assumes that:
// static mvector<mvector<float> > sines(PERIOD/2), cosines(PERIOD/2);
//  exists and is filled elsewhere with:
// //Set up the trig series
//  sines.resize(PERIOD/2);
//  cosines.resize(PERIOD/2);
//  for (n = 0; n < period/2; n++) {
//    sines[n].resize(PERIOD);
//    cosines[n].resize(PERIOD);
//    for (i = 0; i < period; i++) {
//      sines[n][i] = sin(2.*M_PI*n *i / period);
//      cosines[n][i] = cos(2.*M_PI*n *i / period);
//    }
//  }


void sledgeft(mvector<float> &test, mvector<float> &x, mvector<float> &y) {
  int n, period = test.xpoints();

  for (n = 1; n < period/2; n++) {
    x[n] = dot(test, sines[n]) * 2./period;
    y[n] = dot(test, cosines[n]) * 2./period;
  }
  x[0] = 0.0;
  y[0] = test.average();

}


void toampl(mvector<float> &sincoef, mvector<float> &coscoef,
            mvector<float> &ampl, mvector<float> &phase) {
  int i ;

  for (i = 0; i < ampl.xpoints(); i++) {
    ampl[i]  = sqrt(sincoef[i]*sincoef[i] + coscoef[i]*coscoef[i]);
    phase[i] = atan2(sincoef[i], coscoef[i]);
  }
  return;
}

