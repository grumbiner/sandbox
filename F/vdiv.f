      SUBROUTINE vdiv(x, y, n)
C     Robert Grumbine 27 Sep 1995
      
      INTEGER i, n
      REAL x(n), y(n)

      DO 1000 i = 1, n
        x(i) = x(i) / y(i)
 1000 CONTINUE

      RETURN
      END
