#include <stdio.h>
#include <math.h>
/* program to illustrate the spectral effects of averaging over 
   30 years, day by day, vs. periodic input signals */

#define FULL (30*365)

int main(void) {
  float period, tmp;
  double sum, sum2;
  int i, j;

  for (period = 2; period <= FULL; period++) {
    for (i = 0; i < 365; i++) {
      sum = 0;
      sum2 = 0;
      for (j = 0; j < 30; j++) {
        sum += cos(2*M_PI/period*(i+j*365.));
        sum2 += sin(2*M_PI/period*(i+j*365.));
      }
      tmp = sqrt(sum*sum + sum2*sum2)/30.;
      printf("%3d %5.0f  %f %f %f\n",i, period, sum/30, sum2/30, tmp);
    }
  }  

  return 0;
}
