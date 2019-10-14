      SUBROUTINE gset2d(g, nx, ny, nthick, nstep, f, iunit)
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

C     Advection test
      DO 2000 l = 1, ny
      DO 2000 k = 1, nx
        j = nthick/4
        g(j,k,l,i) = 2.5
        g(j-1,k,l,i) = 2.5/2.
        g(j+1,k,l,i) = 2.5/2.
 2000 CONTINUE


      RETURN
      END
