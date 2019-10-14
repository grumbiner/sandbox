      SUBROUTINE correl(x, y, k, r2, xbar, ybar, sig2x, sig2y)
C     Compute various statistical parameters between two 
C       vectors.

      IMPLICIT none

      INTEGER k
      REAL x(k), y(k)
      REAL r2, xbar, ybar, sig2x, sig2y
      
      REAL sumx, sumx2, sumxy
      REAL sx, sy, x2, y2, xy

      sx = sumx(x, k)
      sy = sumx(y, k)
      x2 = sumx2(x, k)
      y2 = sumx2(y, k)
      xy = sumxy(x, y, k)

      xbar = sx/k
      ybar = sy/k
      sig2x = (k*x2-sx*sx)/k/(k-1)
      sig2y = (k*y2-sy*sy)/k/(k-1)
      r2    = (k*xy - sx*sy)/ SQRT( (k*x2-sx*sx)*(k*y2-sy*sy) )

      RETURN
      END 
