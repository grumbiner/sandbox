#include <stdio.h>
#include <math.h>
#include <stdlib.h>

float finv(int t, int n, float *phases);
float rand2pi(void) ;

int main(void) {
  float obs[12*100];
  float phases[12*100];
  float sum = 0.;
  int i;
  
  srand48(0.);
  for (i = 0; i < 12*100; i++) {
    phases[i] = rand2pi();
  }
  for (i = 0 ;i < 12*100; i++) {
    obs[i] = finv(i, 12*100, phases);
    printf("%d\t%f\n",i,obs[i]);
  }
  for (i = 0; i < 12*100; i++) {
    sum += obs[i];
    if ((i+1)%12 == 0) {
      printf("%d\t%f\n",(i+1)/12, sum/12.);
      sum = 0.;
    }
  }

  return 0;
}
float finv(int t,  int n, float *phases) {
  int i;
  double sum = 0.0;
  for (i = 1; i < n/2; i++) {
    sum += 0.25*sqrt(1./(float) i) * cos( (2.*M_PI*(float) i/(float)(12*100)) * (float)t + phases[i] );
    //sum +=  cos( (2.*M_PI*(float) i/(float)(12*100)) * (float)t + phases[i] );
  }
  return (float) sum;
}
float rand2pi(void) {
  return 2.*M_PI*drand48();
}
