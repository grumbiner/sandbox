#include <stdio.h>
#include <time.h>

int main(void) {
  time_t alpha, beta, gamma;
  char chartime[90];

  alpha = clock();
  printf("clock = %d\n", alpha);
  beta = time(&gamma);
  printf("time = %d\n", beta);
  printf("ctime(time) = %s\n", ctime(&beta) );

  sleep(10);
  alpha = clock();
  printf("clock = %d\n", alpha);
  beta = time(&gamma);
  printf("time = %d\n", beta);
  printf("ctime(time) = %s\n", ctime(&beta) );

  return 0;

}

