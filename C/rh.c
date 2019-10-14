#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float qa(float t) ;
float qp(float t, float press) ;
float e(float t) ;

int main(int argc, char *argv[]) {
  float t, rh, potential, press;

  printf("            RH");
  for (rh = .25; rh <= 1.01; rh += .075) { 
    printf("%6.2f",rh);
  }
  printf("\n");

  for (t = 20.0; t < 41.0; t += 1.25) {
    printf("%4.1f C %5.1f F",t, t*1.8 + 32.);
    for (rh = .25; rh <= 1.01; rh += .075) { 
      potential = 9.4E-3*(37. - t) + 23.3*(41.7E-3 - qa(t)*rh);
      printf("%6.2f",potential);
    }
    printf("\n");
  }

  return 0;
}
float qa(float t) {
  float eps = .62197;
  float pref = 1013.25;
  float q;
  t += 273.15;
  q = eps * e(t) / pref / (1.-e(t)/pref*(1.-eps) );
  return q;
}
float qp(float t, float press) {
  float eps = .62197;
  float q;
  t += 273.15;
  q = eps * e(t) / press / (1.-e(t)/press*(1.-eps) );
  return q;
}
float e(float t) {
  return 6.21*exp( 5417. *(1./273.15 - 1./t) );
} 
