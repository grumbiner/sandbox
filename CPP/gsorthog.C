#include <stdio.h>
#include <math.h>

#include "points.h"
#include "mvector.h"
#include "grid_math.h"


#define PARAMETERS 18
#define MAXPTS 200000

void cormat(mvector<mvector<float> > &x, int npoints, int nvect) ;
float dot(mvector<float> &x, mvector<float> &y) ;
void gsorthog(mvector<float> &x1, mvector<float> &x2);


int main(void) {
  mvector< mvector<float> > dat(PARAMETERS);
  int i, j, k, l=0, npoints;
 
  for (k = 0; k < dat.xpoints(); k++) {
     dat[k].resize(MAXPTS);
     dat[k] = 0.;
  }

  while (!feof(stdin)) {
    l+= 1;
    scanf("%d %d ", &i, &j);
    for (k = 0; k < dat.xpoints(); k++) {
      scanf("%f",&dat[k][l]);
    }  
    //printf("l = %d\n",l);
  }
  npoints = l - 1;

  printf("There were %d valid points\n", npoints);

  cormat(dat, npoints, PARAMETERS);
 
  return 0;
}

void cormat(mvector<mvector<float> > &x, int npoints, int nvect) {
  mvector<mvector<float> > dat(nvect);
  int i, j, k;
  ijpt loc, loc2;
  grid2<float> correls(nvect, nvect);
  FILE *fout;

  fout = fopen("cormat.out","w");

// Normalize:
  for (k = 0; k < nvect; k++) {
     dat[k].resize(npoints);
     for (i = 0; i < npoints; i++) {
        dat[k][i] = x[k][i];
     }
     dat[k] -= dat[k].average();
     //dat[k].normalize();
     dat[k] /= sqrt(dot(dat[k], dat[k]) );
     //printf("k, normk %d %f %f\n",k, dat[k].norm(2), dot(dat[k],dat[k]) );
  }

// Compute and print initial correlations:
  for (j = 0; j < nvect; j++) {
  for (i = j; i < nvect; i++) {
    loc.i = i; loc.j = j;
    loc2.i = j; loc2.j = i;
    correls[loc] = dot(dat[i],dat[j]);
    correls[loc2] = correls[loc];
  }
  }

  for (j = 0; j < nvect; j++) {
  for (i = 0; i < nvect; i++) {
    loc.i = i; loc.j = j;
    printf("%5.2f ",correls[loc]);
  }
  printf("\n");
  }

// Orthogonalize w.r.t. param and repeat:
  k = 7;
  for (i = 0; i < nvect; i++) {
    if (i != k) gsorthog(dat[k], dat[i]);
  }
  printf(" \n");
  for (j = 0; j < nvect; j++) {
  for (i = j; i < nvect; i++) {
    loc.i = i; loc.j = j;
    loc2.i = j; loc2.j = i;
    correls[loc] = dot(dat[i],dat[j]);
    correls[loc2] = correls[loc];
  }
  }
  for (j = 0; j < nvect; j++) {
  for (i = 0; i < nvect; i++) {
    loc.i = i; loc.j = j;
    printf("%5.2f ",correls[loc]);
  }
  printf("\n");
  }
  for (j = 0; j < dat[0].xpoints(); j++) {
    for (i = 0; i < dat.xpoints(); i++) {
      fprintf(fout,"%f ",dat[i][j]);
    }
    fprintf(fout,"\n");
  }
  //return ;
 
   

// Orth w.r.t. 
  k = 15;
  for (i = 0; i < nvect; i++) {
    if (i != k) gsorthog(dat[k], dat[i]);
  }
  printf(" \n");
  for (j = 0; j < nvect; j++) {
  for (i = j; i < nvect; i++) {
    loc.i = i; loc.j = j;
    loc2.i = j; loc2.j = i;
    correls[loc] = dot(dat[i],dat[j]);
    correls[loc2] = correls[loc];
  }
  }
  for (j = 0; j < nvect; j++) {
  for (i = 0; i < nvect; i++) {
    loc.i = i; loc.j = j;
    printf("%5.2f ",correls[loc]);
  } 
  printf("\n");
  }



  fflush(stdout);

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
  mvector<float> y(x1.xpoints() );
  tdot = dot(x1,x2) / dot(x1,x1);
  y = x1;  y *= tdot;
  x2 -= y;
  x2 -= x2.average(); x2 /= sqrt(dot(x2, x2));
  return;
}

