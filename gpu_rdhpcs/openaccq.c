#include <stdio.h>
#define NX 32*1024
#define NY 32*1024

int main(void) {
  float x[NX*NY], y[NX*NY], z[NX*NY];
  int i;

#pragma acc parallel
  for (i = 0; i < NX*NY; i++) {
    x[i] = i;
    y[i] = 2*i;
  }

#pragma acc parallel
  for (i = 0; i < NX*NY; i++) {
    z[i] = x[i]*y[i];
  }



  return 0;
}
