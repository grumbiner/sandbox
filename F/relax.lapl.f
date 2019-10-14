C********************************************
      SUBROUTINE iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
C     Relaxation solution to beta*lapl(a) = w*(a-a0) 
C     Robert Grumbine
C     Last Modified 28 May 1996

      IMPLICIT none

      INTEGER i, j, nx, ny
      REAL beta, dx, dy
      REAL a(nx, ny), a0(nx, ny), w(nx, ny), b(nx, ny)
      REAL dmax

      dmax = 0.
      DO 1000 j = 2, ny-1
      DO 1000 i = 2, nx-1
        b(i,j) = (a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) 
     1            + w(i,j)*dx**2/beta*a0(i,j) ) 
     2          / (4+w(i,j)*dx**2/beta )
     3          - a(i,j)
        dmax = AMAX1(dmax, ABS(b(i,j)) )
        a(i,j) = a(i,j) + b(i,j)
 1000 CONTINUE
CD      PRINT *,'dmax = ',dmax

      RETURN
      END
