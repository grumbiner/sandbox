#include <stdio.h>

#include "grid3.h"
#include "ncepgrids.h"

int main(void) {
  llgrid<float> dat(332, 210, 0.1, 0.1, 26.35, -83.05);
  ijpt x;
  int i, j;
  float f;

  dat.set(0.5);
  f = 0.0;
  for (j = 0; j < 100; j++) {
     printf("j = %d\n",j);
     for (x.j = 0; x.j < dat.ypoints() ; x.j++) {
     //printf("x.j = %d \n",x.j);
     for (x.i = 0; x.i < dat.xpoints() ; x.i++) {
       //printf("x.i = %d \n",x.i); fflush(stdout);
        f = f + dat[x];
     }
     }
  }

  printf("f = %f\n", f);
  return 0;
}
