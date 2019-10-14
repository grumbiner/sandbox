      SUBROUTINE rk4(y, dydt, n, t, dt, yout, 
     1              epsilon, delta, derivs)
C     4th order Runge-Kutta solver, derived from Numerical Recipes.
C     Bob Grumbine 7 April 1994.
C     Note: currently derivs is passed extra parameters (due to
C       inverse theory testing version), which should be removed
C       here.

      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 12)
      INTEGER n
      REAL y(n), dydt(n), yout(n) 
      REAL t, dt, epsilon, delta

      REAL yt(nmax), dyt(nmax), dym(nmax)
      REAL hh, h6, xh
      INTEGER i

      EXTERNAL derivs

      IF (n .GT. nmax) THEN
        PRINT *,'You have too many variables for this solver.'
        PRINT *,'The limit is ',nmax
        STOP
      ENDIF

      hh = 0.5*dt
      h6 = dt/6.
      xh = t+hh

      DO 11 i = 1, n
        yt(i) = y(i) + hh*dydt(i)
 11   CONTINUE

      CALL derivs(xh, yt, dyt, epsilon, delta, dt)
      DO 12 i = 1, n
        yt(i) = y(i) + hh*dyt(i)
 12   CONTINUE

      CALL derivs(xh, yt, dym, epsilon, delta, dt)
      DO 13 i = 1, n
        yt(i) = y(i) + dt*dym(i)
        dym(i) = dyt(i) + dym(i)
 13   CONTINUE

      CALL derivs(t+dt, yt, dyt, epsilon, delta, dt)
      DO 14 i = 1, n
        yout(i) = y(i) + h6*(dydt(i) + dyt(i)+2.*dym(i))
 14   CONTINUE

      RETURN
      END
