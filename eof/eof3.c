#include <stdio.h>
#include <stdlib.h>

#include <essl.h>


int compeof(int n, float **evec, float *evals) {
  int iopt = 1;
  int lda = n, ldz = n;
  int naux = 0;
  int i, j;
  FILE *fin;  

  float **a;
  cmplx **z;
  cmplx *lambda;
  int *select;
  float *aux;

/* Allocate space for internals */
  lambda = (cmplx *) malloc(sizeof(cmplx)*n);
  select = (int   *) malloc(sizeof(int  )*n);
  aux    = (float *) malloc(sizeof(float)*n);
  a = (float **) malloc(sizeof(float)*n*n);
  z = (cmplx **) malloc(sizeof(cmplx)*n*n);
  for (i = 0; i < n; i++) {
    a[i] = (float *) malloc(sizeof(float*) *n);
    z[i] = (cmplx *) malloc(sizeof(cmplx*) *n);
  }

  fin = fopen("inp","r");
  for (j = 0; j < n; j++) {
  for (i = 0; i < n; i++) {
     fscanf(fin, "%f",a[i+n*j]);
  }
  }

  sgeev(iopt, a, lda, lambda, z, ldz, select, n, aux, naux);

  printf("Eigenvalues, vectors\n");
  for (j = 0; j < n; j++) {
    if (fabs(IM(lambda[j])) > 1.e-10) {
      printf("non-negligable complex eigenvalue %f\n",IM(lambda[j]));
    }
    printf("%7.4f : ",RE(lambda[j]) );
    for (i = 0; i < n; i++) {
      if ( fabs(IM(*z[j*n+i]) ) > 1.e-10) {
        printf("non-negligable complex eigenvector element %f\n",IM(*z[j*n+i]) );
      }
      printf(" %7.4f ",RE(*z[j*n+i]) );
    }
    printf("\n");
  }

  return 0;
}
