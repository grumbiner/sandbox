      SUBROUTINE iarst2(x, nx, ny, const)
C     Set an array of integers equal to a constant.
C     Robert Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      INTEGER x(nx, ny), const
      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          x(i,j) = const
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
