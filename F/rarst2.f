C***********************************************************----------!!
      SUBROUTINE rarst2(x, nx, ny, value)
C     Set all elements of array x equal to a scalar value.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      REAL x(nx*ny), value

      INTEGER j

      DO 1000 j = 1, ny*nx
          x(j) = value
 1000 CONTINUE

      RETURN
      END
