      SUBROUTINE gext(g, f, nstep, nthick, dt, dh, g0, tstep)
C     Extrapolate the ice thickness distribution for the inversion
C       theory test program.  Use an Euler forward algorithm.
C     Robert Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nstep, nthick, tstep
      REAL dt, dh, g0
      REAL g(nthick, nstep), f(nthick)

      INTEGER i, told
      REAL diffu

      diffu = dh**2 / dt * 0.03

      i = 1
      g(i,tstep) = g0
      told = tstep - 1

      DO 1000 i = 2, nthick-1
        g(i, tstep) = g(i, told) -
     1     (dt/dh/2.)* (
     2       (f(i+1)*g(i+1, told)- f(i-1)*g(i-1,told) )
     4 )
     5  + diffu*(dt/dh**2)*(g(i+1,told)-2.*g(i,told)+g(i-1,told) )
 1000 CONTINUE

C     Have implied boundary condition that no ice is thicker than modelled.
      i = nthick
      g(i,tstep) = 0.0 

      DO 2000 i = 1, nthick
CD        g(i, tstep) = AMAX1(0., g(i,tstep))
 2000 CONTINUE

C     Alternate advection evaluation -- take d(fg)/dh evaluation alanytically.
CD     2     f(i)*     (g(i+1, tstep-1)-g(i-1,tstep-1) ) +
CD     3  g(i,tstep-1)*(f(i+1)         -f(i-1)         )

       RETURN
       END
