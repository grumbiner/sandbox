      SUBROUTINE fset(f, fref, h0, dh, dx, dy, dt, nx, ny, nthick, 
     1          iunit)
C     Initialize the freezing rate function for the inversion theory
C       test program.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nthick, iunit
      INTEGER nx, ny
      REAL dx, dy
      REAL dh, dt, h0, fref, f(nthick, nx, ny)
      REAL h, fdecay

      INTEGER i, j, k

      fdecay = h0

      DO 1200 k = 1, ny
CD        PRINT *,' y = ',k
        DO 1100 j = 1, nx
        DO 1000 i = 1, nthick
        h    = dh*(i-1)
        f(i, j, k) = fref*exp(-h/fdecay)*(-0.0+
     1                    exp( -(j-nx/2)*(j-nx/2)/10. ) )
CD        PRINT *,i,j,k,h,f(i,j,k)
 1000 CONTINUE
 1100 CONTINUE
 1200 CONTINUE

      RETURN
      END
