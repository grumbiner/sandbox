      SUBROUTINE rmsval(x, n, rms, avg, sd)
C     Subroutine to compute the rms value of an input vector.
C     BG 7-8-86.

      INTEGER n
      REAL x(n), rms, avg, sd
      REAL sqsum, sum
      INTEGER i

      sqsum = 0.0
      sum   = 0.0

      DO 1000 i = 1, n
        sqsum = sqsum + x(i)*x(i)
        sum   = sum + x(i)
 1000 CONTINUE

      rms = (sqsum/FLOAT(n))**.5
      avg = sum/ FLOAT(n)
      sd  = ABS( (sqsum-sum*sum/FLOAT(n)) / (FLOAT(n)-1.) ) **.5

      RETURN
      END
