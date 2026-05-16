#include <stdio.h>
#include <math.h>

/* Summarize -- print mean, rms, sqrt(variance) -- scores from the contingency table output */
/* Robert Grumbine 17 May 2018 */ 

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
    for (j = 1; j < 11; j++) {
      sum[j] += x[j];
      sumsq[j] += x[j]*x[j];
    }
  }
  for (j = 5; j < 11; j++) {
    printf("%3d %f %f  %e\n",j, sum[j]/n, sqrt(sumsq[j]/n), sqrt((sumsq[j]-sum[j]*sum[j]/n)/(n-1)) );
  }

  return 0;
}
