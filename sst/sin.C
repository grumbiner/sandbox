#include "mvector.h"
#include <math.h>

float si(mvector<int> &x) ;

int main(void) {
  float period = 365, dt = 0.0625/16/16, t;
  int i;
  mvector<float> ts;
  mvector<int> histogram;
  float x;

  i = (int) rint(period/dt);
  ts.resize(i); printf("using %d points\n", i); fflush(stdout);
  i = 0;
  for (i = 0; i < ts.xpoints(); i++) {
    t = (float) i * dt;
    ts[i] = sin(2.*3.141592654*t/period);
  }

  printf("now in precision loop \n"); fflush(stdout);
  for (float prec = 1.00; prec >= 1.e-5; prec /= 2.0) {
    printf("prec = %f ",prec); 
    histogram.resize(0);
    ts.histogram(histogram, prec); 
    x = si(histogram);
    printf(" si = %f vs. %f\n",x,log(1.+(int)(2./prec) )/log(2.) ); fflush(stdout);
  } 

  return 0;
}
float si(mvector<int> &x) {
  int i, n = 0;
  double shan = 0.;
  double p;
  for (i = 0; i < x.xpoints(); i++) {
    n += x[i];
  }
  for (i = 0; i < x.xpoints(); i++) {
    if (x[i] > 0) {
      p = (double) x[i] / (double) n;
      shan -= p*log(p);
    }
  }
  return shan / log(2.);
} 
