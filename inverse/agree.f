      SUBROUTINE agree(r, rbar, x, n, ia)
C     Compute the index of agreement between two vectors
      REAL r(n), x(n)
      REAL rbar, ia

      REAL sr2, sx2, srx, spx
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
        ia = 1.0 - (sx2 - 2.*srx + sr2) / spx
       ELSE 
        ia = 0.0
      ENDIF

      RETURN
      END
