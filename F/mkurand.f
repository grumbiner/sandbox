      SUBROUTINE mkurand(x, n, rsize, seed)   
C     Subroutine to create a uniformly distributed random variable
C       vector.  
C     Robert Grumbine 12/09/93.

      IMPLICIT none

      INTEGER n, seed
      REAL x(n), rsize, RAN2, tmp
CD      EXTERNAL RAN2

      INTEGER i

      PRINT *,'Entered mkurand'
      DO 1000 i = 1, n
        PRINT *,'in mkurand, i = ',i
        x(i) = rsize*(RAN2(seed)-0.5)*2.
        PRINT *,'x(i) = ', x(i)
 1000 CONTINUE

      RETURN
      END
