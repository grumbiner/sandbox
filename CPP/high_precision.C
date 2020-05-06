#include <stdio.h>
//Robert Grumbine
//17 October 2014

int main(void) {
  long double x = 1e305;

  printf("long double bytes: %d\n",(int) sizeof(x));
  for (int i = 305; i < 4950; i++) {
    printf("%d %Le \n",i,x);
    x *= 10.;
  }

// not allowed in icc, ok in g++  __float128 y = 1e15;
  //printf("__float128 size: %d\n",(int) sizeof(y));
  for (int i = 305; i < 4950; i++) {
    //printf("%d %Le\n",i,y);
    //y *= 10.;
  }
  
  return 0;
}
