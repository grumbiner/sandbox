      SUBROUTINE vset(x, n, val)
C     Robert Grumbine 27 March 1996.
      INTEGER n
      REAL x(n), val
      INTEGER i

      DO 1000 i = 1, n
        x(i) = val
 1000 CONTINUE

      RETURN
      END
