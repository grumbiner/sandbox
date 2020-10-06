#include <stdio.h>

#include "points.h"

int main(void) {
  ijpt x[10][10], y[10][10];
  ijpt a(1,2,3);
  int i, j;

  for (j = 0; j < 10; j++) {
  for (i = 0; i < 10; i++) {
    x[j][i] = a;
    y[j][i] = a; y[j][i] *= 2;
  }
  }
  for (j = 0; j < 10; j++) {
  for (i = 0; i < 10; i++) {
    x[j][i] += y[j][i];
    x[j][i] -= y[j][i];
  }
  }

  return 0;
}
