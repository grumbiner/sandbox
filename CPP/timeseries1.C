#include <math.h>
#include "mvector.h"

float dot(mvector<float> &x, mvector<float> &y) ;
void sledgeft(mvector<float> &test, mvector<float> &sincoef, mvector<float> &coscoef) ;

void toampl(mvector<float> &sincoef, mvector<float> &coscoef, 
            mvector<float> &ampl, mvector<float> &phase);


#define PERIOD (365*4)
static mvector<mvector<float> > sines(PERIOD/2), cosines(PERIOD/2);

int main(void) {
  mvector<float> test(PERIOD), sincoef(PERIOD/2), coscoef(PERIOD/2);
  mvector<float> ampl(PERIOD/2), phase(PERIOD/2);

  int i, n, period = PERIOD;

//Set up the trig series
  for (n = 0; n < period/2; n++) {
    sines[n].resize(PERIOD);
    cosines[n].resize(PERIOD);
    for (i = 0; i < period; i++) {
      sines[n][i] = sin(2.*M_PI*n *i / period);
      cosines[n][i] = cos(2.*M_PI*n *i / period);
    }
  }  

//Set up a test data series
  for (i = 0; i < period; i++ ) {
    test[i] = 0.1*sin(2.*M_PI * 2*i / period) + 0.9* cos(2.*M_PI *i / period); 
  }

  sledgeft(test, sincoef, coscoef);
  toampl(sincoef, coscoef, ampl, phase);
  for (i = 0; i < period/2; i++) {
    //printf("%d  %f sin cos %f %f\n",i, (float) PERIOD / (float) i, 
    printf("%d  %f sin cos %f %f A theta %f %f\n",
              i, (float) i / (float) PERIOD ,
              sincoef[i], coscoef[i], ampl[i], phase[i]);
  }


  return 0;
}

void sledgeft(mvector<float> &test, mvector<float> &x, mvector<float> &y) {
  int i, n, period = test.xpoints();

  for (n = 0; n < period/2; n++) {
    for (i = 0; i < period; i++) {
    }
    x[n] = dot(test, sines[n]) * 2./period;
    y[n] = dot(test, cosines[n]) * 2./period;
  }

}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  int i;
  for (i = 0; i < x.xpoints(); i++) {
    sum += x[i]*y[i];
  }
  return (float) sum;
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
