#include <stdio.h>
#include <math.h>

int main(int argc, char *argv[]) {
  FILE *fin;
  float x[11];
  double sum[11] , sumsq[11] ;
  char s[90];
  int i, j, n = 365*29;

  for (j = 1; j < 11; j++) {
    sum[j] = 0.0;
    sumsq[j] = 0.0;
  }

  fin = fopen(argv[1],"r");
  for (i = 0; i < n; i++) {
    fscanf(fin,"%s %f %f %f %f %f %f %f %f %f %f %f\n",s, &x[0], &x[1], 
                     &x[2], &x[3], &x[4], &x[5], &x[6], &x[7], &x[8], &x[9], &x[10]);
    //printf("%f\n",x[1]);
    for (j = 1; j < 11; j++) {
      sum[j] += x[j];
      sumsq[j] += x[j]*x[j];
    }
  }
  for (j = 1; j < 11; j++) {
    printf("%d %f %f  %f\n",j, sum[j]/n, sqrt(sumsq[j]/n), sqrt((sumsq[j]-sum[j]*sum[j]/n)/(n-1)) );
  }

  return 0;
}
