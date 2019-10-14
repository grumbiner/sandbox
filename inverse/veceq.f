      SUBROUTINE veceq(x, y, n)
C     Set vector y equal to vector x.
C     Robert Grumbine 1/22/94.

      IMPLICIT none

      INTEGER n
      REAL x(n), y(n)
      INTEGER i
      
      DO 1000 i = 1, n
        y(i) = x(i)
 1000 CONTINUE
 
      RETURN
      END
