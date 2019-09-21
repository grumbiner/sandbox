      REAL FUNCTION iagree(r, x, n)
C     Compute the index of agreement between two vectors.
C     Robert Grumbine 7 April 1994.
C      LAST MODIFIED 8 April 1994

      IMPLICIT none

      INTEGER n
      REAL r(n), x(n)

      REAL rbar, sr2, sx2, srx, spx
      INTEGER i

      sr2 = 0.0
      sx2 = 0.0
      srx = 0.0
      spx = 0.0
      rbar = 0.0

      DO 100 i = 0, n
        rbar = rbar + r(i)
  100 CONTINUE
      rbar = rbar / FLOAT (n)

      DO 1000 i = 0, n
          sr2 = sr2 + r(i)*r(i)
          sx2 = sx2 + x(i)*x(i)
          srx = srx + r(i)*x(i)
          spx = spx + (ABS(x(i)-rbar) + ABS(r(i)-rbar) )**2
 1000 CONTINUE

      IF (spx .NE. 0.0) THEN
        iagree = 1.0 - (sx2 - 2.*srx + sr2) / spx
       ELSE 
        iagree = 0.0
      ENDIF

      RETURN
      END 
