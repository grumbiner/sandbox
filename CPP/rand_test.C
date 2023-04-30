#include "mvector.h"

#define NX 1000
#define NY 1000

int main(void) {
  mvector<float> x(NX*NY);
  int i, j;
  mvector<int> hist(35);
  float imin, res = 1./32.;

  x.random((float)0.,(float)1.);
  imin = x.minimum();

  x.histogram(hist, res);
  printf("points %d\n",hist.xpoints() );

  for (i = 0; i < hist.xpoints(); i++ ) {
    printf("val %f  count %d\n",imin + i*res, hist[i]);
    //printf("val %d  count %d\n",(int)(imin + i*res), hist[i]);
  }
 
  return 0;
}
