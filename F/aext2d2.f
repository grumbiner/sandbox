      SUBROUTINE aext(a, f, u, v, g0, dh, nx, ny, nthick, nstep, 
     1             dx, dy, dt, tstep)
C     Extrapolate the ice concentration function for the inverse theory
C       test program.  Use simple minded Euler forward.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER tstep, nthick, nstep, nx, ny
      REAL dh, dt, dx, dy
      REAL a(nx, ny, nstep), f(nthick, nx, ny), g0(nx, ny, nstep)
      REAL u(nx, ny), v(nx, ny)

      INTEGER i, j

      DO 1000 j = 2, ny-1
      DO 1100 i = 2, nx-1
        a(i, j, tstep) = a(i, j, tstep-1) 
     1  + f(1, i, j)*g0(i, j, tstep-1)*dt/2.
     2  - u(i,j)*(a(i+1,j, tstep-1)-a(i-1,j, tstep-1))/dx/2.*dt
     3  - v(i,j)*(a(i,j+1, tstep-1)-a(i,j-1, tstep-1))/dy/2.*dt
     4  - a(i,j, tstep-1)*( (v(i,j+1)-v(i,j))/dy + (u(i+1,j)-u(i,j))/dx
     5         )*dt
 1100 CONTINUE
 1000 CONTINUE
   
      DO 2000 j = 1, ny
        a(1,j, tstep) = a(2,j, tstep)
        a(nx,j, tstep) = a(nx-1,j, tstep)
 2000 CONTINUE
      DO 2100 i = 1, nx
        a(i,ny, tstep) = a(i,ny-1, tstep)
        a(i,1, tstep)  = a(i,2, tstep)
 2100 CONTINUE

      RETURN
      END
