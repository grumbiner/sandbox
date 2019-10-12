      SUBROUTINE uset(u, v, nx, ny, iunit)
C     Initialize the velocity field for the inversion theory
C       test program.
C     Bob Grumbine 31 May 1996.

      IMPLICIT none

      INTEGER iunit, nx, ny
      REAL u(nx, ny), v(nx, ny)
      INTEGER i, j

      DO 1000 j = 1, ny
      DO 1100 i = 1, nx
        v(i,j) = 0.
        u(i,j) = 0.0
 1100 CONTINUE
 1000 CONTINUE

      RETURN
      END
