#include "mvector.h"

#define CLIMVARS 15 
#define MAXCLIM (365 * 80)
double average(mvector<double> &x, double flag, int length) ;
int count(mvector<double> &x, double flag, int length) ;
int count(mvector<double> &x, mvector<double> &y, double flag, int length) ;
double sumx(mvector<double> &x, double flag, int length) ;
double sumx2(mvector<double> &x, double flag, int length) ;
double sumxy(mvector<double> &x, mvector<double> &y, double flag, int length) ;
double variance(mvector<double> &x, double flag, int length) ;
void normalize(mvector<double> &x, double flag, int length, double avg, double var) ;

double correl(mvector<double> &x, mvector<double> &y, double flag, int length) ;
float randnorm(void) ;
void orthog(mvector<double> &x, mvector<double> &y, double flag, int length) ;

int main(int argc, char *argv[] ) {
  mvector<double> climate[CLIMVARS];
  double flag = -99;
  mvector<double> avgs(CLIMVARS), vars(CLIMVARS);
  mvector<int> counts(CLIMVARS);
  int i, j, length;
  FILE *fin, *forthog;
  float t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15;

  for (i = 0; i < CLIMVARS; i++) {
    climate[i].resize(MAXCLIM);
  }

  fin = fopen(argv[1],"r");
  forthog = fopen(argv[2],"r");

  i = 0;
  while ( !feof(fin) ) {
    //fscanf(fin,"%f,%f,%f,%f,%f\n", &t1, &t2, &t3, &t4, &t5);
    fscanf(fin,"%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n", &t1, &t2, &t3, &t4, &t5, &t6, &t7, &t8, &t9, &t10, &t11, &t12, &t13, &t14, &t15);
    //printf("%f %f %f %f %f\n",t1,t2,t3,t4,t5);
    climate[0][i] = t1; 
    climate[1][i] = t2; 
    climate[2][i] = t3; 
    climate[3][i] = t4; 
    climate[4][i] = t5;
    climate[5][i] = t6;
    climate[6][i] = t7;
    climate[7][i] = t8;
    climate[8][i] = t9;
    climate[9][i] = t10;
    climate[10][i] = t11;
    climate[11][i] = t12;
    climate[12][i] = t13;
    climate[13][i] = t14;
    climate[14][i] = t15;
    i++;
  }
  length = i-1;
  printf("length = %d\n",length);
  fclose(fin);

// in this, we're assuming that we've got properly formed vectors --
//   no flags, unit variance, zero mean.
  while (!feof(forthog) ) {
    fscanf(forthog,"%d\n",&i);
    for (j = 0; j < 15; j++) {
      printf("orthogonalizing %2d vs %2d  ",j, i);
      if (i != j) orthog(climate[i], climate[j], flag, length);    
    }
  }
  fclose(forthog);

// Now look at the correlation matrix:
  double r, v, z;
  int n = length, ns = length;
  for (i = 0 ; i < 15; i++) {
    for (j = i; j < 15; j++) {
        r = correl(climate[i], climate[j], flag, length);
        v = 0.5 * log((1 + r) / (1 - r));
        z = v * sqrt(ns - 3);
        z = r / sqrt((1-r*r)/(ns -2) );
        printf("%2d %2d  %1d %1d  %1di %1di %2d  r = %9.6f %9.6f  %7.2f\n",i,j, i%5, j%5, i/5, j/5, j/5-i/5, r, r*r, fabs(z) );
    }
  }

// Print back out for later/different perusal:
  for (i = 0; i <= length; i++) {
    printf("%f  %f  %f  %f  %f  ",climate[0][i], climate[1][i],climate[2][i], climate[3][i], climate[4][i]);
    printf("%f  %f  %f  %f  %f  ",climate[0+5][i], climate[1+5][i],climate[2+5][i], climate[3+5][i], climate[4+5][i]);
    printf("%f  %f  %f  %f  %f\n",climate[0+5*2][i], climate[1+5*2][i],climate[2+5*2][i], climate[3+5*2][i], climate[4+5*2][i]);
  }

  return 0;
}
double average(mvector<double> &x, double flag, int length) {
  int lcount = count(x, flag, length);
  double sum = sumx(x, flag, length);
  
  if (lcount != 0) {
    return sum /= lcount;
  }
  else {
    return 0.0; 
  }

}
double variance(mvector<double> &x, double flag, int length) {
  int lcount = count(x, flag, length);
  double sumsq = sumx2(x, flag, length);
  double avg = average(x, flag, length); 
  
  if (lcount != 0) {
    return (sumsq / lcount - avg*avg);
  }
  else {
    return 0.0; 
  }
}
void normalize(mvector<double> &x, double flag, int length, double avg, double var) {
  int i;
  
  for (i = 0; i <= length; i++) {
    if (x[i] != flag) {
      x[i] -= avg;
      x[i] /= sqrt(var);
    }
  }

}
int count(mvector<double> &x, double flag, int length) {
  int i, lcount = 0;
  
  for (i = 0; i <= length; i++) {
    if (x[i] != flag) {
      lcount++;
    }
  }
  return lcount;
}
double sumx(mvector<double> &x, double flag, int length) {
  double sum = 0.0;
  
  for (int i = 0; i <= length; i++) {
    if (x[i] != flag) {
      sum += x[i];
    }
  }

  return sum;
}
double sumx2(mvector<double> &x, double flag, int length) {
  double sum = 0.0;
  
  for (int i = 0; i <= length; i++) {
    if (x[i] != flag) {
      sum += x[i]*x[i];
    }
  }
  return sum;
}
double sumxy(mvector<double> &x, mvector<double> &y, double flag, int length) {
  double sum = 0.0;
  
  for (int i = 0; i <= length; i++) {
    if (x[i] != flag && y[i] != flag) {
      sum += x[i]*y[i];
    }
  }
  return sum;
}
double correl(mvector<double> &x, mvector<double> &y, double flag, int length) {
  int i, lcount = 0;
  double sx = 0, sy = 0, sxy = 0, sx2 = 0, sy2 = 0;

  for (i = 0; i <= length; i++) {
    if (x[i] != flag && y[i] != flag) {
      sx += x[i];
      sy += y[i];
      sx2 += x[i]*x[i];
      sy2 += y[i]*y[i];
      sxy += x[i]*y[i];
      lcount++;
    }
  }

  double r = (lcount*sxy - sx*sy) / sqrt(lcount*sx2 - sx*sx) / sqrt(lcount*sy2 - sy*sy);

  return r;
}
float randnorm(void) {
  float v1, v2, rsq, fac;

  do {
    v1 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v2 =  (1.0*rand()) /(RAND_MAX+1.0) ;
    v1 = 2.*v1 - 1.;
    v2 = 2.*v2 - 1.;
    rsq = v1*v1 + v2*v2;
  }
  while (rsq >= 1.0 || rsq == 0.0) ;

  fac = sqrt(-2.*log(rsq)/rsq);

  return v1*fac;

}
void orthog(mvector<double> &x, mvector<double> &y, double flag, int length) {
// Remove the portion of y which is parallel to x.  
// Even though 'flag' is in arg list, ignore it for now.
  int i;
  double dot = 0, xdot = 0;
  for (i = 0; i <= length; i++) {
    dot += x[i]*y[i];
    xdot += x[i]*x[i];
  }
  printf("dot = %f %f\n",dot, xdot); 
  for (i = 0; i <= length; i++)  {
    y[i] -= dot*x[i] / xdot;
  }

  double avgs = average(y, flag, length);
  double vars = variance(y, flag, length);
  normalize(y, flag, length, avgs, vars);
  printf("normalized y with mean %f var %f\n",avgs, vars);

  return;
}
