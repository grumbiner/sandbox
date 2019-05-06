C***********************************************************----------!!
      SUBROUTINE arset (x, nx, ny, value)
C     Set all elements of array x equal to value.
C     Robert Grumbine 15 Feb 1995

      IMPLICIT none

      INTEGER nx, ny
      REAL x(nx, ny), value

      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          x(i,j) = value
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
