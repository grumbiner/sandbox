      SUBROUTINE v3add(a, b, c, nx)
C     Add two real vectors and put result in third.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER nx, j
      REAL a(nx), b(nx), c(nx)

      DO 1000 j = 1, nx
          c(j) = a(j) + b(j)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
