#include <stdio.h>
int st(char *x, int nx) {
  int i;
  for (i = 0; i < nx; i++) {
    printf("%d %c\n",i,x[i]);
  }
  return 0;
} 
