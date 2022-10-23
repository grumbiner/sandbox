#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(void) {
  float f0, f1, f2, f3, f4, f5, f6;
  float chandler;
  float a0, a1, a2, a3, a4, a5, a6;
  int i, pmax;
  float x, xt, xm, ts = 1.0, damp;
  char tab;
  float noise = 10.0;

  srand(1);
  f0 = 2.*M_PI/292;
  f1 = 2.*M_PI/399;
  f2 = 2.*M_PI/417;
  f3 = 2.*M_PI/441;
  f4 = 2.*M_PI/584;
  f5 = 2.*M_PI/199.5;
  f6 = 2.*M_PI/1455;
  a0 = 1.59;
  a1 = 1.76; 
  a2 = 0.6; 
  a3 = 0.25;
  a4 = 0.526;
  a5 = 0.931;
  a6 = 0.223;

  chandler = 2.*M_PI/433.;
  damp = chandler / 50;
  tab = (char) 9;
 
  x = 1.0;
  xm = 0.0;
  pmax = 0;
   for (i = 0; i < 399*417; i++ ) { 
    xt = x;
    x = -xm *(1.- 2*damp*ts)/(1.+2*damp*ts)
        + x*(2.-chandler*chandler*ts*ts)/(1+2.*ts*damp);
    x += a1*cos(f1*i*ts) + a2*cos(f2*i*ts) + a3*cos(f3*i*ts);
    x += a0*cos(f0*i*ts);
    x += a4*cos(f4*i*ts) + a5*cos(f5*i*ts) + a6*cos(f6*i*ts);
    x += noise*2.*(((float)rand()/(float)RAND_MAX)-0.5);
    //x += noise*(((float)rand()/(float)RAND_MAX)-0.5) * cos(chandler*i*ts);
    //x += noise*cos(chandler*i*ts);
    printf("%d%c%f\n",i,tab,x);
    if (xt > xm && xt > x) {
      fprintf(stderr, "%d %d %f\n",i, i - pmax, xt);
      pmax = i-1;
    }
    xm = xt; 
  }

  return 0;
}
