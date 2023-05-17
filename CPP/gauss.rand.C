#include <stdlib.h>
#include <stdio.h>

#define SUM 6
#define LIM 1000

int main(void) {
  float sum;
  int i, j;

  srand(0);

  for (i = 0; i < LIM; i++) {
    sum = 0;
    for (j = 0; j < SUM; j++) {
      sum += (float) rand()/(float) RAND_MAX;
    }
    printf("%d %f\n",i,sum/SUM);
  }

  return 0;
} 

