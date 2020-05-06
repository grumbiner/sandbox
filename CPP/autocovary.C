#include "time_series.h"

#define NYEAR 134

int main(void) {
  time_series<float> year(NYEAR), t(NYEAR), dt(NYEAR);
  int i;
  FILE *fin;
  float ty, tt;

  fin = fopen("a","r");
  for (i = 0; i < NYEAR; i++) {
    fscanf(fin, "%f %f\n",&ty, &tt);
    year[i] = ty;
    t[i]   = tt;
  }


  float a = -0.106, b = 0.0055025, c = 6.192e-5, d=5.706e-8;
  for (i = 0; i < NYEAR; i++) {
    ty = year[i] - 1940;
    dt[i] = t[i] - (a+b*ty+c*ty*ty+d*ty*ty*ty);
    printf("%3d\t%f\t%f\n",(int)year[i], t[i], dt[i]);
  }

  //dt -= dt.average();

  for (i = 0; i < 30; i++) {
    ty = dt.autocovary(i);
    printf("%2d  %6.3f\n",i, ty);
  }

  return 0;
}
