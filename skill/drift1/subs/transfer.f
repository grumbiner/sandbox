      SUBROUTINE transfer(dd, ff, dir, dist, meth1, meth2, ndate)
C     Include file to be
      INTEGER nskile, ndays, ndates
      PARAMETER (nskile = 207)
      PARAMETER (ndays  =   6)
      PARAMETER (ndates = 200)
      INTEGER R, THETA
      PARAMETER (R = 1)
      PARAMETER (THETA = 2)

C     The day's forecast fields
      REAL dd(nskile+1, ndays), ff(nskile+1, ndays)
      REAL dir(4*nskile, ndays), dist(4*nskile, ndays)
C     The global forecast data fields
      REAL meth1(nskile, ndates, ndays, 2)
      REAL meth2(nskile, ndates, ndays, 2)
      INTEGER ndate

      INTEGER i, j

      DO 1000 j = 1, ndays
        DO 1100 i = 1, nskile
          meth1(i, ndate, j, R) = ff(i,j)
          meth1(i, ndate, j, THETA) = dd(i,j)
          meth2(i, ndate, j, R)     = dist(i,j)
          meth2(i, ndate, j, THETA) = dir(i,j)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
