#include <stdio.h>
#define NX 4*1024
#define NY 4*1024

int main(void) {
  float x[NX*NY], y[NX*NY], z[NX*NY];
  int i;

#pragma acc parallel loop
  for (i = 0; i < NX*NY; i++) {
    x[i] = i;
    y[i] = 2*i;
  }

#pragma acc parallel loop
  for (i = 0; i < NX*NY; i++) {
    z[i] = x[i]*y[i];
  }


  printf("%f %f %f\n",x[1024*1024], y[1024*1024], z[1024*1024]);

  return 0;
}
