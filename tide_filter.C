#include <stdio.h>
#include "mvector.h"

#define PERIOD 24.83
//#define PERIOD 25.00
#define PARENTS 5
#define SELECTION 7

void convolve(mvector<float> &weights, mvector<float> &series) ;

int main(void) {
  mvector<float> series(PERIOD*10000);
  int i;
  mvector<mvector<float> > weights(PARENTS*SELECTION);
  mvector<mvector<float> > sdev(PARENTS*SELECTION);

// Initialize
  for (i = 0; i < weights.xpoints(); i++) {
    weights[i].resize(25);
    sdev[i].resize(25); 
  }

  for (i = 0; i < series.xpoints(); i++) {
    series[i] = 1. + sin(2.*M_PI*i/(float) PERIOD);
  }
 
  weights[0] = 1.;
  weights[0][5] = 0.5;
  weights[0][25-5] = 0.5;
  //weights[0].printer(stdout);
  convolve(weights[0], series);

  return 0;
}
void convolve(mvector<float> &weights, mvector<float> &series) {
  mvector<float> tmp(series.xpoints() - weights.xpoints() );
  int i, j, nx = series.xpoints();

  tmp = 0.0;

  for (i = weights.xpoints() / 2; 
       i < nx - weights.xpoints()/2; i++ ) {
    for (j = -weights.xpoints() / 2; j <= weights.xpoints()/2; j++) {
       //if (i == weights.xpoints() / 2) printf("j = %d\n",j); fflush(stdout);
       tmp[i] += series[i+j];
    }
  }
   
  printf("avg, rms %f %f\n", tmp.average(), tmp.rms() );

  return;
} 
