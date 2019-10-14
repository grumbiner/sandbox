      SUBROUTINE detrend(x, y, n, a, b, r)
      IMPLICIT none

      INTEGER n
      REAL x(n), y(n)
      REAL a, b, r

      INTEGER i
      DOUBLE PRECISION sumxy, sumx, sumy, sumxx, sumyy
      REAL xm, ym

      sumx  = 0.0
      sumy  = 0.0
      sumxy = 0.0
      sumxx = 0.0
      sumyy = 0.0

      DO i = 1, n 
        sumx  = sumx + x(i)
        sumxx = sumxx + x(i)*x(i)
        sumxy = sumxy + x(i)*y(i)
        sumyy = sumyy + y(i)*y(i)
        sumy  = sumy  + y(i)
      ENDDO

      xm = sumx / FLOAT(n)
      ym = sumy / FLOAT(n)

      b = (sumxy-n*xm*ym)/(sumxx - n*xm*xm)
      a = ym - b*xm
      r = (sumxy-n*xm*ym)/SQRT(sumxx-n*xm*xm)/SQRT(sumyy-n*ym*ym)

      DO i = 1, n
        y(i) = y(i) - a - b*x(i)
      ENDDO

      RETURN
      END
