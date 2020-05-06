      SUBROUTINE mkrand(x, y, n, rsize, seed)   
C     Subroutine to add a uniformly distributed random variable
C       to a vector.  
C     Robert Grumbine 12/09/93.
      IMPLICIT none

      INTEGER n, seed
      REAL x(n), y(n), rsize
      REAL ran2, t

      INTEGER i

      t = RAN2(seed)

      DO 1000 i = 1, n
        y(i) = x(i) + rsize*(RAN2(seed)-0.5)*2.
 1000 CONTINUE

      RETURN
      END
