#include "mvector.h"

void  cormat(mvector<float> &x1, mvector<float> &x2, mvector<float> &x3,
             mvector<float> &x4, mvector<float> &x5, mvector<float> &x6,
             mvector<float> &x7, mvector<float> &x8, int totpts) ;

extern "C" void correl_(float *x, float *y, int *n, float *r2, \
                  float *xbar, float *ybar, float *sig2x, float *sig2y);

float dot(mvector<float> &x, mvector<float> &y);
void gsorthog(mvector<float> &x1, mvector<float> &x2);
void swap(mvector<float> &x1, mvector<float> &x2) ;

void  cormat(mvector<float> &x1, mvector<float> &x2, mvector<float> &x3,
             mvector<float> &x4, mvector<float> &x5, mvector<float> &x6,
             mvector<float> &x7, mvector<float> &x8, int totpts) {
//Have extracted mvector of points, now see about simple correlation matric
  float cors[8][8];
  mvector<float> *vecs[8], *tvec;
  float r2, xbar, ybar, sig2x, sig2y;
  int i, j, nmaxcur;

  tvec = new mvector<float> (totpts);
  vecs[0] = new mvector<float> (totpts);
  vecs[1] = new mvector<float> (totpts);
  vecs[2] = new mvector<float> (totpts);
  vecs[3] = new mvector<float> (totpts);
  vecs[4] = new mvector<float> (totpts);
  vecs[5] = new mvector<float> (totpts);
  vecs[6] = new mvector<float> (totpts);
  vecs[7] = new mvector<float> (totpts);
  *vecs[0] = x1;
  *vecs[1] = x2;
  *vecs[2] = x3;
  *vecs[3] = x4;
  *vecs[4] = x5;
  *vecs[5] = x6;
  *vecs[6] = x7;
  *vecs[7] = *vecs[6];
  *vecs[7] *= *vecs[6];

  nmaxcur = 8;
//normalizing and removing averages leaves unchanged
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  //CDprintf("%d norm %f average %f\n",j,vecs[j]->norm(), vecs[j]->average() );
  }

  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j], vecs[i], &totpts, &r2, &xbar, &ybar, &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

// Now see about orthogonalizing w.r.t. 7 (PR)
  swap(*vecs[6], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;

//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j], vecs[i], &totpts, &r2, &xbar, &ybar, &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

// Repeat, now extract 7th param (PR^2)
  swap(*vecs[6], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;
//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j], vecs[i], &totpts, &r2, &xbar, &ybar, &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

// Repeat, now extract 3rd param (T85V)
  swap(*vecs[2], *vecs[nmaxcur-1]);
  for (j = 0; j < nmaxcur - 1; j++) {
    gsorthog(*vecs[nmaxcur-1], *vecs[j]);
  }
  //vecs[0]->printer(stdout);
  nmaxcur -= 1;
//Repeated code.  Note, however, that this is now all variables.
  for (j = 0; j < nmaxcur; j++) {
    *vecs[j] -= vecs[j]->average();
    vecs[j]->normalize() ;
  }
  for (j = 0; j < nmaxcur; j++) {
  for (i = 0; i <= j; i++) {
    correl_(vecs[j], vecs[i], &totpts, &r2, &xbar, &ybar, &sig2x, &sig2y);
    cors[j][i] = r2;
    cors[i][j] = r2;
  }
  }
  printf("\n Correlation table\n");
  for (j = 0; j < nmaxcur; j++) {
    for (i = 0; i < nmaxcur; i++) {
      printf("%6.3f ",cors[i][j]);
    }
    printf("\n");
  }

  return;
}

float dot(mvector<float> &x, mvector<float> &y) {
  double sum = 0.0;
  //should text for xpoints being same on both
  int i;
  for (i = 0; i < x.xpoints() ; i++) {
    sum += x[i] * y[i] ;
  }
  return (float) sum;
}

//Perform Gram-Schmidt orthogonalization, assuming that mvectors are already
//  normalized and zero mean x2 = x2 - dot(x1,x2)*x1
void gsorthog(mvector<float> &x1, mvector<float> &x2) {
  float tdot ;
  tdot = dot(x1,x2) / dot(x1,x1);
  //CDprintf("tdot = %f, mvector norms are %f %f\n",tdot, x1.norm(), x2.norm() );
  x2 -= x1 * tdot;
  return;
}
void swap(mvector<float> &x1, mvector<float> &x2) {
  mvector<float> y(x1.xpoints() );
  y = x2;
  x2 = x1;
  x1 = y;
  return;
}

