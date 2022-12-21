      SUBROUTINE gext(g, f, u, v, nx, ny, nstep, nthick, dt, dh, 
     1               dx, dy, g0, tstep)
C     Extrapolate the ice thickness distribution for the inversion
C       theory test program.  Use an Euler forward algorithm.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nx, ny, nstep, nthick, tstep
      REAL dt, dh, g0(nx, ny, nstep), dx, dy
      REAL g(nthick, nx, ny, nstep), f(nthick, nx, ny)
      REAL u(nx, ny), v(nx, ny)

      INTEGER i, j, k, told
      REAL diffu

      diffu = dh**2 / dt * 0.01
      told = tstep - 1

      i = 1
      DO 100 k = 1, ny
      DO 100 j = 1, nx
        g(i, j, k, tstep) = g0(j, k, tstep)
  100 CONTINUE

      DO 1200 k = 2, ny-1
      DO 1100 j = 2, nx-1
      
CD      i = 2
CDC     Change for the f1g1 term
CD        g(i, j, k, tstep) = g(i, j, k, told) -
CD     1     (dt/dh/2.)* 
CD     2       (f(i+1, j, k)*g(i+1, j, k, told) - 
CD     3        f(i-1, j, k)*g(i-1, j, k, tstep) )
CDCD     2  ( f(i,j,k)*(g(i+1,j,k,told) - g(i-1,j,k,told)) +   !explosively 
CDCD     3    g(i,j,k,told)*(f(i+1,j,k) - f(i-1,j,k) )       ) !unstable
CD     5  + diffu*(dt/dh**2)*(g(i+1, j, k,told)-2.*g(i, j, k,told)+
CD     6                      g(i-1, j, k,told) )
CD     7 - u(j, k)*(g(i, j+1, k, told) - g(i, j-1, k, told))*dt/dx/2.
CD     8 - v(j, k)*(g(i, j, k+1, told) - g(i, j, k-1, told))*dt/dy/2.
CD     9 - g(i,j,k,told)*( (u(j+1,k)-u(j-1,k))/dx/2. 
CD     1                  +(v(j,k+1)-v(j,k-1))/dy/2. )*dt

      DO 1000 i = 2, nthick-1
        g(i, j, k, tstep) = g(i, j, k, told) -
     1     (dt/dh/2.)* 
     2       (f(i+1, j, k)*g(i+1, j, k, told) - 
     3        f(i-1, j, k)*g(i-1, j, k, told) )
CD     2  ( f(i,j,k)*(g(i+1,j,k,told) - g(i-1,j,k,told)) +   !explosively 
CD     3    g(i,j,k,told)*(f(i+1,j,k) - f(i-1,j,k) )       ) !unstable
     5  + diffu*(dt/dh**2)*(g(i+1, j, k,told)-2.*g(i, j, k,told)+
     6                      g(i-1, j, k,told) )
     7 - u(j, k)*(g(i, j+1, k, told) - g(i, j-1, k, told))*dt/dx/2.
     8 - v(j, k)*(g(i, j, k+1, told) - g(i, j, k-1, told))*dt/dy/2.
     9 - g(i,j,k,told)*( (u(j+1,k)-u(j-1,k))/dx/2. 
     1                  +(v(j,k+1)-v(j,k-1))/dy/2. )*dt
 1000 CONTINUE
 1100 CONTINUE
 1200 CONTINUE

C     Have implied boundary condition that no ice is thicker than modelled.
      i = nthick
      DO 3000 j = 1, ny
      DO 3000 k = 1, nx
        g(i, k, j, tstep) = 0.0 
 3000 CONTINUE

       RETURN
       END
