      PROGRAM relative
C     minimize the squared relative error, and retain no average rel. err.
      IMPLICIT none

      INTEGER npts
      PARAMETER (npts = 100)

      REAL x(npts), y(npts)
      INTEGER i, j
      REAL a, b
      CHARACTER*60 fname

      PRINT *,'What is the name of the data file?'
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      i = 0
 1000 CONTINUE
        i = i + 1 
        READ (10,*,END=2000) x(i), y(i)
        IF (i .LT. npts) GO TO 1000
 2000 CONTINUE
      i = i - 1

      CALL solve(x, y, i, a, b)
      PRINT *,'Linear a, b ',a, b 
      DO 3000 j = 1, i
        print *,j, x(j), y(j), a+b*x(j), (y(j)-a-b*x(j))/y(j)
 3000 CONTINUE

      DO 4000 j = 1, i
        y(j) = LOG(y(j))
 4000 CONTINUE
      CALL solve(x, y, i, a, b)
      PRINT *,'Now try for a exponential version a, b ', a, b
      DO 4100 j = 1, i
        print *,j, x(j), y(j), a+b*x(j), (y(j)-a-b*x(j))/y(j)
 4100 CONTINUE
      DO 4200 j = 1, i
        x(j) = LOG(x(j))
 4200 CONTINUE
      CALL solve(x, y, i, a, b)
      PRINT *,'Now try for a power law version a, b ', a, b
      DO 4300 j = 1, i
        print *,j, x(j), y(j), a+b*x(j), (y(j)-a-b*x(j))/y(j)
 4300 CONTINUE

      STOP
      END
      SUBROUTINE solve(x, y, n, a, b)
      INTEGER n
      REAL x(n), y(n)
      REAL a, b

      INTEGER j
      REAL sy, sy2, sxy, sxy2
      REAL delta

      sy = 0.
      sy2 = 0.
      sxy = 0.
      sxy2 = 0.

      DO 1000 j = 1, n
        sy = sy + 1./y(j)
        sy2 = sy2 + 1./y(j)/y(j)
        sxy = sxy + x(j)/y(j)
        sxy2 = sxy2 + x(j)/y(j)/y(j)
 1000 CONTINUE

      delta = sy*sxy2 - sy2*sxy
      IF (delta .EQ. 0) THEN
        PRINT *,'delta = 0, no inversion possible'
        a = 0.
        b = 0.
       ELSE
        a = (FLOAT(n)*sxy2 - sy*sxy) / delta
        b = (sy*sy - FLOAT(n)*sy2) / delta
      ENDIF

      RETURN
      END

