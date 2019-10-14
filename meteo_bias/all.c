#include <stdio.h>
#include <math.h>

#define nf  49
#define ny 192
#define nx  94

void pull(float *all, float *x, const int k);
void correl(float *a1, float *a2, const int k, 
              float *r2, float *xbar, float *ybar, float *sig2x, float *sig2y);
float sumx(float *x, const int n);
float sumxy(float *x, float *y, const int n);
float sumx2(float *x, const int n);

int main(void)
{
  FILE *fin1, *fin2;
  float f1[ny][nx], f2[ny][nx];
  float all1[nf][ny][nx], all2[nf][ny][nx];

  int i, j, k;

  float iagree, ia, r2, xbar, ybar, sig2x, sig2y;

  fin1 = fopen("dump.ref", "r");
  fin2 = fopen("dump.thick", "r");

  fread(all1, sizeof(float), nf*nx*ny, fin1);
  fread(all2, sizeof(float), nf*nx*ny, fin2);

  ia = 0.;
  for ( k = 0; k < nf; k++) {

    pull(&all1[0][0][0], &f1[0][0], k);
    pull(&all2[0][0][0], &f2[0][0], k);

    correl(&f1[0][0], &f2[0][0], nx*ny, &r2, &xbar, &ybar, &sig2x, &sig2y);

    printf("%2d  %6.3f %6.3f %11.3f %11.3f %11.3f %11.3f\n",
        k+1, ia, r2, xbar, ybar, (float) sqrt(sig2x), (float) sqrt(sig2y) );

  }

  return;
}

  void pull(float *all, float *x, const int k)
{
      int i, j, index1, index2;

      for ( j = 0; j < ny; j++) {
      for ( i = 0; i < nx; i++) {
         index1 = i + j*nx + k*nx*ny;
         index2 = i + j*nx;
         x[index2] = all[index1] ;
      }
      }
     return;
}

void correl(float *x, float *y, const int k, float *r2, 
              float *xbar, float *ybar, float *sig2x, float *sig2y)
{
/* Converted from the fortran by Bob Grumbine 23 February 1996                   
C     Compute statistical parameters between two vectors:
C       mean for each, variance for each, and correlation between.
C     Uses unbiased estimator of variance.
C     Bob Grumbine 1/22/94.
C     LAST MODIFIED 8 April 1994.
*/
      
      float sx, sy, x2, y2, xy;

      sx = sumx(x, k);
      sy = sumx(y, k);
      x2 = sumx2(x, k);
      y2 = sumx2(y, k);
      xy = sumxy(x, y, k);

      *xbar = sx / (float)k;
      *ybar = sy/ (float)k;
      *sig2x = ( (float)k*x2-sx*sx)/ (float)k/ (float)(k-1);
      *sig2y = ( (float)k*y2-sy*sy)/ (float)k/ (float)(k-1);
      *r2    = ( (float)k*xy - sx*sy) / 
         sqrt( ( (float)k*x2-sx*sx)*( (float)k*y2-sy*sy) );

      return;
}
      float sumx(float *x, const int n)
{
/* Translated from the fortran by Bob Grumbine 23 Feburary 1996 
C     Function to compute the sum of a vector.  Use double precision
C       summation, but return a single precision answer.  
C     Bob Grumbine 12/09/93.
*/

      double sum;
      int i;
      
      sum = 0.;
      for (i = 0; i< n; i++) {
        sum += x[i];
      }

      return (float) sum;

}
      float sumx2(float *x, const int n)
{
/* Converted from the fortran Bob Grumbine 23 February 1996 
C     Function to compute the sum of the square of terms in a vector.
C       Use double precision summation, but return a single precision answer.  
C     Bob Grumbine 12/09/93.
*/

      double sum;
      int i;
      
      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum +=  (x[i]*x[i]);
      }

      return (float) sum ;
}
      float sumxy(float *x, float *y, const int n)
{
/* Converted from the fortran by Bob Grumbine 23 February 1996
C     Function to compute the sum of the product of two vectors.
C       Use double precision summation, but return a single precision answer.  
C     Bob Grumbine 12/09/93.
*/

      int i;
      double sum;

      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += x[i]*y[i];
      }

      return (float) sum;
}
