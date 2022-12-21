#include "metric.h"

void walshtof(metricgrid<char> &c, metricgrid<float> &f) {
// Translate out of walsh character encoding to floating point concentrations
  ijpt loc;
  int tmpint;
  char *c1;

  c1 = new char[1];
  for (loc.j = 0; loc.j < c.ypoints() ; loc.j++) {
  for (loc.i = 0; loc.i < c.xpoints() ; loc.i++) {
    
    //printf("character %2d %2d %1c\n",loc.i, loc.j, c[loc]);

    if (isdigit(c[loc]) ) {
      c1[0] = c[loc];
      tmpint = atoi(c1);
      f[loc] = ((float) tmpint) / 10.0;
    }
    else if (c[loc] == '*') {
      f[loc] = 1.0;
    }
    else if (c[loc] == '.') {
      f[loc] = 1.57; // Land tag in RG systems
    }
    else {
      printf("Unknown character type %2d %2d %1c\n",loc.i, loc.j, c[loc]);
    }
  }
  }

  return;
}
void getvec(ijpt &loc, mvector<float> &x, metricgrid<float> *ary) {
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    printf("getvec, i, loc %d  %3d %3d\n",i, loc.i, loc.j); fflush(stdout);
    x[i] = ary[i].operator[](loc);
  }
  return;
}
float dot(mvector<float> &x, mvector<float> &y) {
  int i;
  float sum = 0.0;
  if (x.xpoints() != y.xpoints() ) {
    printf("cannot dot unequal sized mvectors\n");
    return -1.;
  }
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i]*y[i];
  }
  return sum;
}

