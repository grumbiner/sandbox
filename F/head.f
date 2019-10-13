      PROGRAM messing
      IMPLICIT none
      INTEGER n
      PARAMETER (n = 10955)
      REAL x(n), y(n)
      INTEGER nlag
      REAL corrs(n), a, b, r

      INTEGER i, j

      !OPEN(10,FILE="out1", FORM="FORMATTED",STATUS="OLD")
      DO i = 1, n
        READ(*,*) j, x(i)
        y(i) = i
      ENDDO

      CALL detrend(y, x, n, a, b, r)
      PRINT *,'a b r', a, b, r

      y=x
      nlag = 365*10
      CALL autocor(x, y, n, corrs, nlag)

      DO i = 1, nlag
        PRINT *,i, corrs(i)
      ENDDO

      END
      SUBROUTINE autocor(x, y, n, corr, nlag)
C     Compute correlations between two vectors.  Truncate
C       rather than wrap around.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER n, nlag
      REAL x(n), y(n), corr(nlag)
      INTEGER lag
      REAL r2, mux, muy, sigx, sigy
      
      DO 1000 lag = 1, nlag
        CALL correl(x(1), y(lag), n-lag, r2, mux, muy, sigx, sigy)
        corr(lag) = r2
 1000 CONTINUE
 
      RETURN
      END
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
