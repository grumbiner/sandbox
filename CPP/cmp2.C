#include "ncepgrids.h"

void regress(grid2<unsigned char> &x, grid2<unsigned char> &y) ;

int main(int argc, char *argv[]) {
  southhigh<unsigned char> x,z,y;
  FILE *fin;

  fin = fopen(argv[1], "r");
  x.binin(fin);
  fclose(fin);
  fin = fopen(argv[2], "r");
  y.binin(fin);
  fclose(fin);
  fin = fopen(argv[3], "r");
  z.binin(fin);
  fclose(fin);

  printf("f16-17 "); regress(y,x);
  printf("f18-17 "); regress(y,z);
  printf("f16-18 "); regress(x,z);

  return 0;
}
void regress(grid2<unsigned char> &x, grid2<unsigned char> &y) {
  double sumx, sumy, sumx2, sumy2, sumxy, delta, delta2;
  int i, count = 0;
  float r;

  sumx = 0.0;
  sumy = 0.0;
  sumx2 = 0.0;
  sumy2 = 0.0;
  sumxy = 0.0;
  delta = 0.0;
  delta2 = 0.0;

  for (i = 0; i < x.xpoints()*x.ypoints(); i++) {
    if (x[i] < 128 && y[i] < 128 && x[i] > 0 && y[i] > 0) {
      x[i] = min(100, x[i]);
      y[i] = min(100, y[i]);
      sumx += x[i];
      sumy += y[i];
      sumx2 += x[i]*x[i];
      sumy2 += y[i]*y[i];
      sumxy += x[i]*y[i];
      delta  += ((double) x[i] - (double) y[i]);
      delta2 += ((double) x[i] - (double) y[i])*((double) x[i] - (double) y[i]);
      count += 1;
      //if ( abs(x[i]-y[i]) > 20) {
      //  printf("%3d %3d  %4d\n",x[i], y[i], (x[i]-y[i]) );
      //}
    }
  }

  float xm, ym, a, b;

  xm = sumx / (double) count;
  ym = sumy / (double) count;
  delta /= (float) count;

  b  = (sumxy - count*xm*ym) / (sumx2 - count*xm*xm);
  a = ym - b*xm;
  r = (sumxy - count*xm*ym)/sqrt(sumx2 - count*xm*xm)/sqrt(sumy2 - count*ym*ym);
  printf(" %d %f %f %f %f %f %f\n",count, a, b, xm - ym, delta, sqrt(delta2/count - delta*delta), r);

  return;
}
