#include <stdio.h>
#include <stdlib.h>

#define NX 18048

int main(void) {
  double *x;
  int i;

  for (i = 8192; i < 18048; i += 256) {
     x = malloc(sizeof(double) * i * NX);
     if (x == (NULL) ) {
       printf("failed at %d to malloc %ld bytes\n",i, sizeof(double) * i * NX);
       break;
     }
     printf("i = %d Mallocated %ld bytes\n",i, sizeof(double) * i * NX);
     free(x);
  }

  return 0;
}
