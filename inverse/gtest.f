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
      REAL diffu, tmp


      diffu = dh**2 / dt * 0.0
      told = tstep - 1

      i = 1
      DO 100 k = 1, ny
      DO 100 j = 1, nx
        g(1, j, k, tstep) = g0(j, k, tstep)
CD        g(i, j, k, told) = g0(j, k, told)
  100 CONTINUE

      i = 1
      k = ny/2
      j = nx/2
      PRINT *,i,j,k,tstep, g0(j,k,tstep)

      IF (tstep .EQ. 1) RETURN

      DO 1200 k = 2, ny-1
      DO 1100 j = 2, nx-1

       i = 2
        g(i, j, k, tstep) = g(i, j, k, told) -
     1     (dt/dh/2.)* 
     2       2.*(f(i, j, k)*g(i, j, k, told) - 
     3        f(i-1, j, k)*g(i-1, j, k, told)  )
      DO 1000 i = 3, nthick-2

CD        g(i, j, k, tstep) = SQRT( 
        tmp = 
     1        g(i, j, k, told)*g(i,j,k,told) -
     1     (dt/dh)*g(i,j,k,told)*2.* 
     2   (
     4         f(i  , j, k)*g(i  , j, k, told)
     3       - f(i-1, j, k)*g(i-1, j, k, told)
     5   )
        IF (tmp .LT. 0.) THEN
          PRINT *, i, j, k, tmp
          tmp = 0
        ENDIF
        g(i,j,k,tstep) = SQRT(tmp)

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
CForward, third order
C     2   2.*(  f(i+2, j, k)*g(i+2, j, k, told)*(-0.5)
C     3       + f(i+1, j, k)*g(i+1, j, k, told)*(2.)
C     4       + f(i  , j, k)*g(i  , j, k, told)*(-1.5) )
CBackward, first order
CD     2       2.*(f(i, j, k)*g(i, j, k, told) - 
CD     3        f(i-1, j, k)*g(i-1, j, k, told)  )
CCentered, second
CD     2       (f(i+1, j, k)*g(i+1, j, k, told) - 
CD     3        f(i-1, j, k)*g(i-1, j, k, told)  )
CForward, first order
C     2       2.*(f(i+1, j, k)*g(i+1, j, k, told) - 
C     3        f(i, j, k)*g(i, j, k, told)  )
CUnstable
CD     2  ( f(i,j,k)*(g(i+1,j,k,told) - g(i-1,j,k,told)) +   !explosively 
CD     3    g(i,j,k,told)*(f(i+1,j,k) - f(i-1,j,k) )       ) !unstable
