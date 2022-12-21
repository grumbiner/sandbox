#include <math.h>

void correl(float *a1, float *a2, const int k, 
              float *r2, float *xbar, float *ybar, float *sig2x, float *sig2y);
float sumx(float *x, const int n);
float sumxy(float *x, float *y, const int n);
float sumx2(float *x, const int n);

void correl(float *x, float *y, const int k, float *r2, 
              float *xbar, float *ybar, float *sig2x, float *sig2y)
{
/* Converted from the fortran by Robert Grumbine 23 February 1996                   
C     Compute statistical parameters between two vectors:
C       mean for each, variance for each, and correlation between.
C     Uses unbiased estimator of variance.
C     Robert Grumbine 1/22/94.
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
/* Translated from the fortran by Robert Grumbine 23 Feburary 1996 
C     Function to compute the sum of a vector.  Use double precision
C       summation, but return a single precision answer.  
C     Robert Grumbine 12/09/93.
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
/* Converted from the fortran Robert Grumbine 23 February 1996 
C     Function to compute the sum of the square of terms in a vector.
C       Use double precision summation, but return a single precision answer.  
C     Robert Grumbine 12/09/93.
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
/* Converted from the fortran by Robert Grumbine 23 February 1996
C     Function to compute the sum of the product of two vectors.
C       Use double precision summation, but return a single precision answer.  
C     Robert Grumbine 12/09/93.
*/

      int i;
      double sum;

      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += x[i]*y[i];
      }

      return (float) sum;
}
