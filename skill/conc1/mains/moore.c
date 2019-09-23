#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

void gsorthog(float *vj, float *vi, int n);
float sumx(float *x, int n) ;
float sumxy(float *x, float *y, int n);
int regress(float *x, float *y, int n, 
  float *xbar, float *sigx, float *a, float *b, float *r, float *t);

#define VARS 18
#define ENTRIES 89

int main(void)
{
  float xbar[VARS], sigx[VARS], a[VARS], b[VARS], r[VARS], t[VARS];
  int n;
  float x[ENTRIES], y[ENTRIES];
  float table[VARS][ENTRIES];
  FILE *fin, *fout;
  char *county, *tmp, *field, delim;
  
  int i, j, tvars, tentry;
  
  field = malloc(9000*sizeof(char));
  county = malloc(9000*sizeof(char));
  if (field == NULL) {
    printf("malloc failed!\n");
    sleep(3);
    return -1;
  }
  fin = fopen("county.tab", "r");
  fout = fopen("statout", "w");
  delim = '\t';

  for (i = 0; i < ENTRIES; i++) {
    tmp = fgets(county, 9000, fin);
    field = strtok(county, &delim);
    tmp = NULL;
    delim = '\t';
    for (j = 0; j < VARS; j++) {
      field = strtok(tmp, &delim);
      table[j][i] = atof(field);
    }  
  }

  for (i = 0; i < ENTRIES; i++) {
    for (j = 0; j < VARS; j++) {
      fprintf(fout," %7.2f",table[j][i]);
    }
   fprintf(fout,"\n");
  }
  
/* Loop over all variables, considering the first one to be the figure of merit and
    the remainder to be attempts to explain it*/
  while (1 != 0)  {
    printf("\n       xbar     sigx,      a,      b,     r,     t\n");
    fprintf(fout,"\n       xbar     sigx,      a,      b,     r,     t\n");
    for (tvars = 0; tvars < VARS; tvars++) {
      printf("Calling regress\n");
      regress(&table[tvars][0], &table[0][0], ENTRIES, &xbar[tvars], &sigx[tvars], &a[tvars], &b[tvars], &r[tvars], &t[tvars]);
      printf("returned from regress\n");
      printf("%3d %8.2f %8.2f %f %f %5.2f %7.2f\n",tvars,xbar[tvars], sigx[tvars], a[tvars], b[tvars], r[tvars], t[tvars]);
      fprintf(fout,"%3d %7.2f %8.2f %f %f %5.2f %7.2f\n",tvars,xbar[tvars], sigx[tvars], a[tvars], b[tvars], r[tvars], t[tvars]);
    }
    
    /* If any of the t's are greater than 1.282, sort and look for which is largest */
    tentry = 1;
    for (tvars = 2; tvars < VARS; tvars++) {
      if (fabs(t[tvars]) > fabs(t[tentry]) ) {tentry = tvars;}  
    }
    if (fabs(t[tentry]) > 1.282) {
      printf("Orthogonalizing with respect to variable %d\n", tentry);
      fprintf(fout,"Orthogonalizing with respect to variable %d\n", tentry);
      /* Have a potentially significant entry, orthogonalize with respect to it*/
      for (i = 0; i < VARS; i++) {
        if (i != tentry) {   
          gsorthog(&table[tentry][0], &table[i][0], ENTRIES); 
        }
      }
    }
    else {
      break;
    }
  }

 

  return 0;
}

int regress(float *x, float *y, int n, 
  float *xbar, float *sigx, float *a, float *b, float *r, float *t)
{
  float sxy, sx, sy, sx2, sy2;
  float s, sigy;
  
  if (n < 3) return -1;
  
  sxy = sumxy(x, y, n);
  sx  = sumx(x, n);
  sy  = sumx(y, n);
  sx2 = sumxy(x, x, n);
  sy2 = sumxy(y, y, n);
  
  *xbar = sx / n;
  *sigx = sqrt(fabs(sx2 - sx*sx/n));
  sigy  = sqrt(fabs(sy2 - sy*sy/n));
  
  if (*sigx == 0. || sigy == 0.) {
    *a = 0.;
    *b = 0.;
    s  = 0.;
    *t = 0.;
    *r = 1.;
    return 0;
  }
  *b = (n*sxy - sx*sy)/(n*sx2 - sx*sx);
  *a = (sy - *b*sx)/n;
  s  = (sy2 - *a*sy - *b*sxy)/(n-2);
  if (s == 0.) { *t = 99.;}
  else { 
    *t = *b * *sigx / s;
  }
  *r = (sxy - sx*sy/n) / *sigx / sigy;
  
  return 0;
}
    

void gsorthog(float *vi, float *vj, int n)
{
  int i;
  float dotij, dotii, vibar, vjbar;
  
  vibar = sumx(vi, n) / n;
  vjbar = sumx(vj, n) / n;
  for (i = 0; i < n; i++) {
    vi[i] -= vibar;
    vj[i] -= vjbar;
  }
  
  dotij = sumxy(vj, vi, n);
  dotii = sumxy(vi, vi, n);  
  for (i = 0; i < n; i++) {
    vj[i] = vj[i] - dotij*vi[i]/dotii;
  }
  return;
}
float sumx(float *x, int n) 
{
  int i;
  float temp;
  
  temp = 0;
  
  for (i = 0; i < n; i++) {
    temp += x[i];
  }
  return temp;
}  
float sumxy(float *x, float *y, int n)
{
  int i;
  float temp;
  
  temp = 0.;
  for (i = 0; i < n; i++) {
    temp += x[i]*y[i];
  }
  return temp;
}
