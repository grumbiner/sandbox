#include "mvector.h"

int main(void) {
  mvector<int> x(NX);
  int i;

  for (i = 0; i < x.xpoints(); i++) {
    x[i] = i;
  }
  x.shuffle();

  for (i = 0; i < x.xpoints(); i++ ) {
    printf("i %4d randi %4d  del %5d \n",i, x[i], i-x[i]);
  }
 
  return 0;
}
