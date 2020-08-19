#include <stdio.h>
#include <stdlib.h>

#include <essl.h>

#define N 4

int main(void) {
  int iopt = 1;
  float a[N][N];
  cmplx lambda[N];
  cmplx z[N][N];
  int lda = N, ldz = N;
  int select[N];
  int n = N;
  float aux[N];
  int naux = 0;
  int i, j;
  

  fprintf(stderr, "about to call\n");

  a[0][0] = 1.0;
  a[1][0] = 0.71;
  a[2][0] = 0.70;
  a[3][0] = 0.52;
  
  a[0][1] = 0.71;
  a[1][1] = 1.00;
  a[2][1] = 0.80;
  a[3][1] = 0.63;
  
  a[0][2] = 0.70;
  a[1][2] = 0.80;
  a[2][2] = 1.00;
  a[3][2] = 0.79;
  
  a[0][3] = 0.52;
  a[1][3] = 0.63;
  a[2][3] = 0.79;
  a[3][3] = 1.00;
  
  sgeev(iopt, a, lda, lambda, z, ldz, select, n, aux, naux);


  printf("Eigenvalues, vectors\n");
  for (j = 0; j < N; j++) {
    printf("%7.4f %7.4f:",RE(lambda[j]), IM(lambda[j]) );
    for (i = 0; i < N; i++) {
      printf("%7.4f %7.4f  ",RE(z[j][i]), IM(z[j][i]) );
    }
    printf("\n");
  }


  return 0;
}
