      SUBROUTINE gset(g, nthick, nstep, nx, ny, f, iunit)
C     Initialize the thickness distribution for the inversion theory
C       test program.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nthick, nstep, iunit, nx, ny
      REAL g(nthick, nx, ny, nstep), f(nthick, nx, ny)
      INTEGER i, j, k, l

      i = 1
      DO 1000 l = 1, ny
      DO 1000 k = 1, nx
      DO 1000 j = 1, nthick
        g(j, k, l, i) = 0.0
 1000 CONTINUE

      RETURN
      END
