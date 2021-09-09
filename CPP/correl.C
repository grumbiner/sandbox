#include "mvector.h"
#include "grid_base.h"

#define MAXPTS (5000*1000)

double demean(mvector<float> &x, int npts) ;
float correl(mvector<float> &x, mvector<float> &y, int npts);
double dot(mvector<float> &x, mvector<float> &y, int npts) ;
#include "unconstrained1.C"

int main(int argc, char* argv[]) {
  FILE *fin;
  mvector<float> icec(MAXPTS), uice(MAXPTS);
  mvector<float> tb[7];
  int i, npts;
  int tice, tflag;
  float tlat, tlon, t1, t2, t3, t4, t5, t6, t7;
  grid2_base<float> correlations(8,8);
  const char *pole;
  int satno = 15, ant = 0;

  fin = fopen(argv[1], "r");

  for (i = 0; i < 7; i++) {
    tb[i].resize(MAXPTS);
  }

  i = 0;
  while (i < MAXPTS && !feof(fin)) {
    fscanf(fin, "%d %d %f %f %f %f %f %f %f %f %f\n",&tice, &tflag, &tlat, &tlon,
        &t1, &t2, &t3, &t4, &t5, &t6, &t7);
    icec[i] = tice;
    tb[0][i] = t1;
    tb[1][i] = t2;
    tb[2][i] = t3;
    tb[3][i] = t4;
    tb[4][i] = t5;
    tb[5][i] = t6;
    tb[6][i] = t7;
    if (tlat > 0) {
      pole = "n";
    }
    else {
      pole = "s";
    }
    uice = nasa_team(t1, t2, t3, t4, t5, t6, t7, 
                *pole, ant, satno);
    printf("%5.1f %5.1f %6.1f  %7.2f %7.2f  %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n", 
             icec[i], uice[i], icec[i] - uice[i], tlat, tlon, t1, t2, t3, t4, t5, t6, t7);
    i++;
  }
  npts = i - 1;

  mvector<double> mean(8);
  mean[7] = demean(icec, npts);
  for (i = 0; i < 7; i++) {
    mean[i] = demean(tb[i], npts);
  }
  for (i = 0; i <= 7; i++) {
    printf("%d mean is %lf\n",i, mean[i]);
  }

  int j;
  for (i = 0; i < 7; i++) {
  for (j = i+1; j < 7; j++) {
    printf("%d %d  %f\n",i,j,correl(tb[i], tb[j], npts) );
  }
  }

  return 0;
}
double demean(mvector<float> &x, int npts) {
  long double sum = 0;
  for (int i = 0; i < npts; i++) {
    sum += x[i];
  }
  sum /= (long double) npts;
  for (int i = 0; i < npts; i++) {
    x[i] -= sum;
  }
  return (double) sum;
}


float correl(mvector<float> &x, mvector<float> &y, int npts) {
  double sumx = 0, sumy = 0, sumx2 = 0, sumy2 = 0, sumxy = 0;
  double avx, avy;
  float r;

  for (int i = 0; i < npts; i++) {
    sumx += x[i];
    sumy += y[i];
//    sumxy += x[i]*y[i];
//    sumx2 += x[i]*x[i];
//    sumy2 += y[i]*y[i];
  }
  sumxy = dot(x,y,npts);
  sumx2 = dot(x,x,npts);
  sumy2 = dot(y,y,npts);

  avx = sumx / (double) npts;
  avy = sumy / (double) npts;
  if (fabs(avx) > 1.e-6 || fabs(avy) > 1.e-6) {
    //printf("averages = %Le %Le\n", avx, avy);
    printf("averages = %e %e\n", avx, avy);
  }

  r = (npts*sumxy - sumx*sumy) / sqrt(npts*sumx2-sumx*sumx) / sqrt(npts*sumy2-sumy*sumy);

  return r;
}
double dot(mvector<float> &x, mvector<float> &y, int npts) {
  long double sumxy = 0;
  for (int i = 0; i < npts; i++) {
    sumxy += x[i]*y[i];
  }
  return (double) sumxy;
}
