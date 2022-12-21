#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float grand(int *idum);
float ran1(int *idum) ;

int main(int argc, char *argv[]) {
  int i, j, tau = 1;
  float ar;
  float x[20000];
  int idum;

  srand(0);
  ar = atof(argv[1]);
  for (i = 0; i < tau; i++) {
    x[i] = grand(&idum); 
  }
  for (i = tau; i < tau+30*365+7; i++) {
    x[i] = grand(&idum);
    for (j = 1; j <= tau; j++) {
      x[i] += pow(ar,j)*x[i-j];
    } 
    printf("%d\t%f\n",i-tau,x[i]);
  }

  return 0;
}
float grand(int *idum) {
  /* Should use better random number generator than system rand */
  static int iset = 0;
  static float gset;
  float fac, r, v1, v2;

  if (iset == 0) { 
    do {
      v1 = 2.0*ran1(idum)-1.0;
      v2 = 2.0*ran1(idum)-1.0; 
      r = v1*v1 + v2*v2;
    }  while (r >= 1.0);
    fac = sqrt(-2.0*log(r)/r);
    gset = v1*fac;
    iset = 1;
    return v2*fac;
  }
  else {
    iset = 0.;
    return gset;
  }

}

float ran1(int *idum) {
  return ((float) rand()/(float) RAND_MAX);
}
