#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXLEN 2000
#define NPTS 55516

int main(int argc, char *argv[]) {
  FILE *fin1, *fin2, *fout;
  float t1, t2;
  double t3, t4;
  float u1[NPTS], u2[NPTS];
  double r[NPTS], rdot[NPTS];
  double sum1, sum2, sum3, sum4;  
  float  j, k[NPTS];
  int i;
  char tab, line[MAXLEN*2], c1, c2, *num;

  tab = (char) 9;
  fin1 = fopen(argv[1], "r");
  fin2 = fopen(argv[2], "r");
  fout = fopen(argv[3], "w");
  i = 0;
  while ( !feof(fin1) && i < NPTS ) {
    fgets(line, MAXLEN, fin1);
    sscanf(line, "%f %f %f\n",&j,&t1, &t2);

    fgets(line, MAXLEN, fin2);
    sscanf(line, "%f %lf %lf\n",&k[i],&t3, &t4);

    u1[i] = t1;
    u2[i] = t2;
    r [i] = t3;
    rdot[i] = t4;

    i += 1;
  }

  sum1 = 0.0;
  sum2 = 0.0;
  sum3 = 0.0;
  sum4 = 0.0;
  for (i = 0; i < NPTS; i++) {
    sum1 += u1[i];
    sum2 += u2[i];
    sum3 += r[i];
    sum4 += rdot[i];
  }
  //printf("%lf %lf %lf %lf\n",sum1, sum2, sum3, sum4);

  sum1 /= (double) NPTS;
  sum2 /= (double) NPTS;
  sum3 /= (double) NPTS;
  sum4 /= (double) NPTS;
  printf("means %lf %lf %17.15lf %17.15lf\n",sum1, sum2, sum3, sum4 );

  for (i = 0; i < NPTS; i++) {
    printf("%9.2f%c%9.6f%c%9.6f%c%18.15lf%c%10.7lf\n", k[i], tab,
         u1[i] - sum1, tab,
         u2[i] - sum2, tab, 
         100.*(r[i] - sum3), tab,
         rdot[i] - sum4 );
  }

  return 0;
}
