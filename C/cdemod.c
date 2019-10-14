#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_REPORTS 10000
#define BUFLEN 120
#define VAR u

int main(int argc, char *argv[]) {
  FILE *fin;
  int hh[MAX_REPORTS], dd[MAX_REPORTS], mm[MAX_REPORTS], yy[MAX_REPORTS];
  int n[MAX_REPORTS];
  float sp[MAX_REPORTS], dir[MAX_REPORTS], u[MAX_REPORTS], v[MAX_REPORTS];
  float t[MAX_REPORTS], p[MAX_REPORTS]; 
  char bufr[BUFLEN];

  float real[MAX_REPORTS], imag[MAX_REPORTS];
  float r[MAX_REPORTS], theta[MAX_REPORTS];
  float freq;
  int avg, length;

  int i, k;
  float t1, t2, t3;

  fin=fopen(argv[1],"r");
  /* fscanf(fin,"%s",bufr); */
  fgets(bufr, BUFLEN, fin);
  avg = atoi(argv[2]);
  freq = atof(argv[3]);

  printf("averaging interval = %d\n",avg); fflush(stdout);
  printf("frequency to demodulate = %f\n",freq); fflush(stdout);

  i = 0;
  while (!feof(fin) ) {
    fgets(bufr, BUFLEN, fin);
    sscanf(bufr, "%d %d %d %d %f %f %f %f %f %f %d\n",
       &hh[i], &dd[i], &mm[i], &yy[i], &sp[i], &dir[i], &u[i], &v[i], 
              &t[i], &p[i], &n[i]);
    /*
    printf("%d %d %d %d %f %f %f %f %f %f %d\n",
       hh[i], dd[i], mm[i], yy[i], sp[i], dir[i], u[i], v[i], 
              t[i], p[i], n[i]);
    fflush(stdout);
    */
    i += 1;
  }
  length = i - 1;
  printf("found %d reports\n",length); fflush(stdout);
  t1 = 0.;
  t2 = 0.;
  t3 = 0.;
  for (i = 0; i < length; i++) {
    t1 += u[i];
    t2 += v[i];
    t3 += t[i];
  }
  t1 /= (float) length;
  t2 /= (float) length;
  t3 /= (float) length;
  for (i = 0; i < length; i++) {
    u[i] -= t1;
    v[i] -= t2;
    t[i] -= t3;
    u[i] = u[i]*v[i];
  }
  printf("averages: %f %f %f\n",t1, t2, t3);

    

  for (i = 0; i < length ; i++) {
     real[i] = VAR[i]*cos(freq*(float)i*2*M_PI ); 
     imag[i] = VAR[i]*sin(freq*(float)i*2*M_PI );
  }
  for (i = 0; i < length - avg; i++) {
    t1 = 0.0;
    t2 = 0.0;
    t3 = 0.0;
    for (k = 0; k < avg; k++) {
      t1 += real[i+k];
      t2 += imag[i+k];
      t3 += VAR[i+k];
    }
    t1 /= (float) avg;
    t2 /= (float) avg;
    t3 /= (float) avg;
    r[i] = sqrt(t1*t1 + t2*t2) ;
    theta[i] = atan2(t2, t1);
    printf("%4d %f %f %f\n",i, VAR[i], r[i], theta[i]); fflush(stdout);
  }

  return 0;
}

