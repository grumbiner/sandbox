#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float randnorm(void) ;

int main(void) {

  srand(0);
  for (int i = 0; i < 68668; i++) {
    printf("%7.2f\t%f\t%f\t%f\t%f\t%f\n",0.25*(float) i,
        randnorm(), randnorm(), randnorm(), randnorm(), randnorm() );
  }

  return 0;
}
float randnorm(void) {
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
