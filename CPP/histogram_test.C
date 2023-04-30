#include "mvector.h"

#define NX 300
#define NY 300

int main(void) {
  mvector<int> x(NX*NY);
  int i, j;
  mvector<int> hist(35);
  float imin, res = 10.;

  for (j = 0; j < NY; j++) {
  for (i = 0; i < NX; i++) {
    x[i + j*NX] = i;
  }
  }
  imin = x.minimum();

  x.histogram(hist, res);
  printf("points %d\n",hist.xpoints() );

  for (i = 0; i < hist.xpoints(); i++ ) {
    //printf("val %f  count %d\n",imin + i*res, hist[i]);
    printf("val %d  count %d\n",(int)(imin + i*res), hist[i]);
  }
 
  return 0;
}
