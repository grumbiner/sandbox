#include "mvector.h"

#define CLIMVARS 22 
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

int main(int argc, char *argv[] ) {
  mvector<double> climate[CLIMVARS];
  double flag = -99;
  mvector<double> avgs(CLIMVARS), vars(CLIMVARS);
  mvector<int> counts(CLIMVARS);
  int i, j, length;
  FILE *fin;
  float t1, t2, t3, t4, t5;

  for (i = 0; i < CLIMVARS; i++) {
    climate[i].resize(MAXCLIM);
  }

  fin = fopen(argv[1],"r");

  i = 0;
  while ( !feof(fin) ) {
    fscanf(fin,"%f,%f,%f,%f,%f\n", &t1, &t2, &t3, &t4, &t5);
    //printf("%f %f %f %f %f\n",t1,t2,t3,t4,t5);
    climate[0][i] = t1; 
    climate[1][i] = t2; 
    climate[2][i] = t3; 
    climate[3][i] = t4; 
    climate[4][i] = t5;
    i++;
  }
  length = i-1;
  //printf("length = %d\n",length);

  for (j = 0; j < 5; j++) {
    avgs[j] = average(climate[j], flag, length);
    for (i = 0; i <= length; i++) {
      if (climate[j][i] == flag) climate[j][i] = avgs[j];
    }
    counts[j] = count(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("%d %d  avg, var = %f %f  ",j, counts[j], avgs[j], vars[j]);
    normalize(climate[j], flag, length, avgs[j], vars[j]);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("normed avg, var = %f %f\n",avgs[j], vars[j]);
  }

// First integral:
  climate[0+5][0] = climate[0][0]; 
  climate[1+5][0] = climate[1][0]; 
  climate[2+5][0] = climate[2][0]; 
  climate[3+5][0] = climate[3][0]; 
  climate[4+5][0] = climate[4][0];
  for (i = 1; i <= length; i++) {
    for (j = 0; j < 5; j++) {
      if (climate[j][i] != flag) {
        climate[j+5][i] = climate[j+5][i-1]+climate[j][i];
      }
      else {
        climate[j+5][i] = flag;
      }
    }
  }
  for (j = 0+5; j < 5+5; j++) {
    counts[j] = count(climate[j], flag, length);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("%d %d  avg, var = %f %f  ",j, counts[j], avgs[j], vars[j]);
    normalize(climate[j], flag, length, avgs[j], vars[j]);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("normed avg, var = %f %f\n",avgs[j], vars[j]);
  }

// Second integral:
  climate[0+5*2][0] = climate[0][0]; 
  climate[1+5*2][0] = climate[1][0]; 
  climate[2+5*2][0] = climate[2][0]; 
  climate[3+5*2][0] = climate[3][0]; 
  climate[4+5*2][0] = climate[4][0];
  for (i = 1; i <= length; i++) {
    for (j = 0; j < 5; j++) {
      if (climate[j+5][i] != flag && climate[j+5*2][i-1] != flag) {
        climate[j+5*2][i] = climate[j+5*2][i-1]+climate[j+5][i];
      }
      else {
        climate[j+5*2][i] = flag;
      }
    }
  }
  for (j = 0+5*2; j < 5+5*2; j++) {
    counts[j] = count(climate[j], flag, length);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("%d %d  avg, var = %f %f  ",j, counts[j], avgs[j], vars[j]);
    normalize(climate[j], flag, length, avgs[j], vars[j]);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("normed avg, var = %f %f\n",avgs[j], vars[j]);
  }

// Pad out with random vectors:
  for (j = 15; j < CLIMVARS; j++) {
    for (i = 0 ; i <= length; i++) {
      climate[j][i] = randnorm();
    }
    counts[j] = count(climate[j], flag, length);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("%d %d  avg, var = %f %f  ",j, counts[j], avgs[j], vars[j]);
    normalize(climate[j], flag, length, avgs[j], vars[j]);
    avgs[j] = average(climate[j], flag, length);
    vars[j] = variance(climate[j],flag, length);
    printf("normed avg, var = %f %f\n",avgs[j], vars[j]);
  }
    

// Now look at the correlation matrix:
  double r, v, z;
  int n, ns;
  for (i = 0 ; i < CLIMVARS; i++) {
    n = counts[i];
    for (j = i; j < CLIMVARS; j++) {
      if (counts[i] > 2 && counts[j] > 2) {
        r = correl(climate[i], climate[j], flag, length);
        if (counts[j] < n) { ns = counts[j]; } else { ns = n; } 
        v = 0.5 * log((1 + r) / (1 - r));
        z = v * sqrt(ns - 3);
        printf("%2d %2d  %1d %1d  %1di %1di %2d  r = %9.6f %9.6f  %7.2f\n",i,j, i%5, j%5, i/5, j/5, j/5-i/5, r, r*r, fabs(z) );
      }
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
// Assume that both already have 0 mean and unit variance.
// Even though 'flag' is in arg list, ignore it for now.
  int i;
  double dot = 0;
  for (i = 0; i <= length; i++) {
    dot += x[i]*y[i];
  }
  printf("dot = %f\n",dot); 
  for (i = 0; i <= length; i++)  {
    y[i] -= dot*x[i];
  }

  return;
}
