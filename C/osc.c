#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define LENGTH 43330
float y(float *ampl, float p1, float p2, float p3, float p4, int i);

int main(int argc, char *argv[]) {
  float p1 = 2*M_PI/416.69, p2 = 2.*M_PI/439.332, p3 = 2.*M_PI/398.88, p4=2.*M_PI/488.9;
  float p0 = 2*M_PI;
  float freq[5];
  float ampl[5];
  float x[LENGTH];
  int i;
  float damp = 1./100.;

  p0 = p0 / atof(argv[1]);
  damp = 1. / atof(argv[2]);
  ampl[0] = 0.;
  ampl[1] = 4.79;
  ampl[2] = 2.93;
  ampl[3] = 15.92;
  ampl[4] = 0.64;
  x[0] = 0; 
  x[1] = 0;
  for (i = 1; i < LENGTH; i++) {
// x(n+1) - 2*x[n] + x[n-1]  + omega^2*x(n) = dt^2*y(n)
    x[i+1] = 2*x[i] - x[i-1] - p0*p0*x[i] + y(ampl, p1, p2, p3, p4, i);
    // linear x[i+1] -= (x[i]-x[i-1])*damp;
    // quadratic:
    x[i+1] += (x[i]-x[i-1])*(x[i]-x[i-1])*damp;
    printf("%d\t%f\t%f\n",i,x[i], y(ampl, p1, p2, p3, p4, i) );
  }

  return 0;
}
float y(float *ampl, float p1, float p2, float p3, float p4, int i) {
  return ( ampl[1]*cos(p1*i)+ampl[2]*cos(p2*i) + ampl[3]*cos(p3*i) + ampl[4]*cos(p4*i) );
}
