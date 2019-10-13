#include <stdlib.h>
#include <stdio.h>
#include <math.h>
// Robert Grumbine 24 December 2008

//return normally-distributed random in range +-1
//Algorithm from Numerical Recipes
//
float randnorm(void) ;

#define ITER 100000
int main(void) {
  float temps[ITER], tmp;
  int i;

  srand(0);
    temps[0] = randnorm();
  for (i = 1; i < ITER; i++) {
    temps[i] = randnorm();
    printf("%6d %6.3f %6.3f\n",i,temps[i], temps[i]-temps[i-1]);
  }

  return 0;
}
float randnorm(void) {
  int i;
  float v1, v2, rsq, fac;
  
  do {
    v1 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v2 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v1 = 2.*v1 - 1.;
    v2 = 2.*v2 - 1.;
    rsq = v1*v1 + v2*v2;
  }
  while (rsq >= 1.0 || rsq == 0.0) ;

  fac = sqrt(-2.*log(rsq)/rsq);

  return v1*fac;

}
