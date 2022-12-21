#include <stdio.h>
// Perform intersatellite regression on tb for passive microwave sea ice
// Robert Grumbine 21 April 2004 -- trimmed version

#include "mvector.h"
#include "ncepgrids.h"
#include "ssmiclass.h"

#define POINTS (10*1000*1000) 

void correl(mvector<float> &pred, mvector<float> &conc, float &r,
            float &a, float &b) ;
float dot(mvector<float> &x, mvector<float> &y) ;

int paramcount = 0;

int main(int argc, char *argv[]) {
  mvector<float> x(POINTS), y(POINTS), tmp;
  FILE *fin;
  int count, i;
  float r, a, b;

  fin = fopen(argv[1],"r");
  if (fin == (FILE *) NULL) {
    printf("failed to open %s\n",argv[1]);
    return -1;
  }

  count = 0;
  while (!feof(fin) ) {
    fscanf(fin,"%f %f %f\n",&x[count],&y[count],&r);
    //if (count < 10) printf("%f %f %f\n",x[count],y[count],r);
    count += 1;
  }
  fclose(fin);
  a = 0; b = 0;
  for (i = 0; i < count; i++) {
    a += x[i];
    b += y[i];
  }
  a /= count;
  b /= count;
  printf("averages %f %f\n",a,b);

  printf("Found %d points to work with\n", count); fflush(stdout);
  if (count == 0) {
    printf("there are no points that can be used for regression\n");
    return -1;
  }
  int err = 0;
  float ratio = 1.75;
  for (i = 0; i < count; i++) {
    if (x[i] > ratio*a || y[i] > ratio*b || x[i] < a/ratio || y[i] < b/ratio ||
        fabs(x[i]-y[i])/a > 0.25 ) {
      err += 1;
    }
    else {
      if (err != 0) {
        x[i-err] = x[i];
        y[i-err] = y[i];
      }
    }
  }
  printf("found %d error points, ratio %f\n",err, ratio);
  for (i = count - err; i < count; i++) {
    x[i] = 0.;
    y[i] = 0.;
  }

  y -= x;
  count -= err;
  tmp.resize(count);
  tmp = x;
  x.resize(count);
  x = tmp;
  tmp = y;
  y.resize(count);
  y = tmp;
 
  correl(x, y, r, a, b);
  printf("r a b %f %f %f\n",r, a, b);

  return 0;
}

void correl(mvector<float> &pred, mvector<float> &conc, float &r,
            float &a, float &b) {
  mvector<float> x(pred.xpoints()), y(pred.xpoints());
  float sx, sy, sxy, sxx;
  float det;
  float n;

  n = (float) pred.xpoints();
  x = pred;
  y = conc;

  if (x.xpoints() != y.xpoints() ) {
    printf("data volume mismatch in correl: %d %d\n",x.xpoints(), y.xpoints() );
  }
  sx = x.average()*n;
  sy = y.average()*n;
  sxy = dot(x, y);
  sxx = dot(x, x);

  det = n*sxx - sx*sx;
  if (det == 0.) {
    printf("Singular, cannot work with\n");
    a = 0;  b = 0; r = 0;
    return ;
  }

  b = (n * sxy  - sx*sy) / det;
  a = y.average() - b*x.average();

  r = (n * sxy  - sx*sy) / sqrt(det) / sqrt(n*dot(y,y) - sy*sy);

  return;

}
float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should test for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}
