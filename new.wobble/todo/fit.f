      SUBROUTINE fit(x, y, n, a, b, r)
C     Robert Grumbine 15 December 1994

      IMPLICIT none

      INTEGER n
      REAL x(n), y(n)
      DOUBLE PRECISION a, b, r

      INTEGER i
      DOUBLE PRECISION sumxy, sumx, sumy, sumxx, sumyy
      DOUBLE PRECISION xm, ym

      !DEBUG PRINT *,'entered fit ',n
      !! DEBUG DO i = 1, n
      !! DEBUG    PRINT *,i,x(i),y(i)
      !! DEBUG ENDDO 

      sumx  = 0.0
      sumy  = 0.0
      sumxy = 0.0
      sumxx = 0.0
      sumyy = 0.0

      DO 1000 i = 1, n
        sumx  = sumx + x(i)
        sumxx = sumxx + x(i)*x(i)
        sumxy = sumxy + x(i)*y(i)
        sumyy = sumyy + y(i)*y(i)
        sumy  = sumy  + y(i)
 1000 CONTINUE

      xm = sumx / DBLE(n)
      ym = sumy / DBLE(n)

      b = (sumxy-n*xm*ym)/(sumxx - n*xm*xm)
      a = ym - b*xm
      r = (sumxy-n*xm*ym)/SQRT(sumxx-n*xm*xm)/SQRT(sumyy-n*ym*ym)

      RETURN
      END
