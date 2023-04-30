#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mvector.h"

#define MAXLEN 2000
#define NPTS 55516
// u starts on jan 1 1948, 4xday
// rad, rdot start on jan 1 1962, 4xday
// iers starts on jan 1 1962, 1xday

int main(int argc, char *argv[]) {
  FILE *radin, *uin, *iersin, *fout;
  float t1, t2;
  double t3, t4;
  mvector<float> u(12);
  double r[NPTS], rdot[NPTS];
  double sum1, sum2, sum3, sum4;  
  float  k[NPTS];
  int i, j;
  float jd;
  char tab, line[MAXLEN*2], c1, c2, *num;

  tab = (char) 9;
  uin = fopen(argv[1], "r");
  radin = fopen(argv[2], "r");
  iersin = fopen(argv[3], "r");
  fout = fopen(argv[4],"w");

// Step up to the Jan 1 1962:
  float u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u0;
  for (i = 0; i < 365*14*4 + 4*4; i++) {
     fgets(line, MAXLEN, uin);
     sscanf(line, "%f %f %f %f %f %f %f %f %f %f %f %f\n",&jd, 
        &u0, &u1, &u2, &u3, &u4, &u5, &u6, &u7, &u8, &u9, &u10, &u11);
     u[0] = u0;
     u[1] = u1;
     u[2] = u2;
     u[3] = u3;
     u[4] = u4;
     u[5] = u5;
     u[6] = u6;
     u[7] = u7;
     u[8] = u8;
     u[9] = u9;
     u[10] = u10;
     u[11] = u11;
  }

//Now work through iers, radius, u
    
  i = 0;
  while ( !feof(uin) && i < NPTS ) {
     fgets(line, MAXLEN, uin);
     sscanf(line, "%f %f %f %f %f %f %f %f %f %f %f %f\n",&jd, 
        &u0, &u1, &u2, &u3, &u4, &u5, &u6, &u7, &u8, &u9, &u10, &u11);
     u[0] = u0;
     u[1] = u1;
     u[2] = u2;
     u[3] = u3;
     u[4] = u4;
     u[5] = u5;
     u[6] = u6;
     u[7] = u7;
     u[8] = u8;
     u[9] = u9;
     u[10] = u10;
     u[11] = u11;

    fgets(line, MAXLEN, radin);
    sscanf(line, "%f %lf %lf\n",&k[i],&t3, &t4);
    r [i] = t3;
    rdot[i] = t4;

    fprintf(fout,"%f%c%f%c%f",k[i],tab,t3,tab,t4);
    for (j = 0; j < 12; j++) {
      fprintf(fout,"%c%f",tab,u[j]);
    }
    fprintf(fout,"\n");

    i += 1;
  }


  return 0;
}
