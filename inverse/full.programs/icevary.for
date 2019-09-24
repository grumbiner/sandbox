      PROGRAM icevary
C     Variational inversion of concentration and growth rate information.
C     -- Coupled to a full ice thickness distribution evolution model.
C     Bob Grumbine 5/31/94.
 
      IMPLICIT none

C     Evolution parameters
      REAL h0, fref, dh, days, dtf
      INTEGER nstep, nthick
      PARAMETER (h0   = 0.10)
      PARAMETER (fref = 0.10)
      PARAMETER (dh   = 0.01)
      PARAMETER (days = 8.0 )
      PARAMETER (dtf   = dh/fref/days)
      PARAMETER (nstep = days/dtf)
      PARAMETER (nthick = fref/dh*5+1)
      REAL a(nstep), g0(nstep), g(nthick, nstep), f(nthick), hbar

C     Inversion parameters.
      INTEGER nvars
      REAL dt
      PARAMETER (nvars = 4)
      PARAMETER (dt    = 1.0)
      REAL epsilon, beta, delta
      REAL sset, a0, t, r0
      REAL alpha(0:nstep-1), p(0:nstep-1) 
      REAL r(0:nstep-1)
      EXTERNAL a0, r0, derivs, r0dot
      
      REAL ier, y(nvars), dydt(nvars), yout(nvars)
      INTEGER i, j
      REAL r0dot, rfrac
      
      OPEN (10, FILE='bering.dat', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='freezout', FORM='FORMATTED', STATUS='UNKNOWN')

      ier = sset(0.)
      IF (ier .NE. 0.) THEN
        PRINT *,'Error in reading from sset '
        STOP
      ENDIF

      PRINT *,'Enter epsilon, cr, beta'
      READ (*, 9001) epsilon
      READ (*, 9001) beta
      delta = beta/epsilon
 9001 FORMAT (E13.6)
      PRINT *,epsilon, beta, delta
      PRINT *,'What fraction of the whole run do you want?'
      READ (*,9001) rfrac

      y(1) = a0(0.)
      y(2) = -r0(0.)
      y(3) = r0(0.)
      y(4) = 0.0
      alpha(0) = a0(0.)
      p(0)     = -r0(0.)
      r(0)     = r0(0.)

C     Find the 'smoothed' values.
      PRINT *,'Will take ',INT(rfrac*nstep+0.5), ' steps'
      DO 1000 i = 1, INT(rfrac*nstep+0.5)
        t = i * dt
CD        PRINT *,'t = ',t, 'dt = ',dt
        CALL derivs(t, y, dydt, epsilon, delta, dt)
        CALL rk4(y, dydt, nvars, t, dt, yout, 
     1              epsilon, delta, derivs)
        alpha(i) = yout(1)
        p(i)     = yout(2)
        r(i)     = -p(i)
        yout(3) = 0.0
        yout(4) = 0.0
        
        DO 1010 j = 1, nvars
          y(j) = yout(j)
 1010   CONTINUE

        WRITE (12,9003) t,
     1         alpha(i), a0(t), r(i), r0(t), p(i)+r(i), 
     2        (a0(t+dt)-a0(t-dt))/2./dt + r0dot(r0, t, dt)
 1000 CONTINUE

 9003 FORMAT (F8.2, 2F8.4, 2F10.6, 2F8.4)

      STOP
      END
      SUBROUTINE derivs(t, y, dydt, epsi, delta, dt)

      IMPLICIT none
      REAL t, dt, epsi, delta, r0, r0dot
      INTEGER nvar
      PARAMETER (nvar = 4)
      REAL y(nvar), dydt(nvar)

      REAL alpha, p, r, a0
      EXTERNAL a0, r0, r0dot

      alpha = y(1)
      p     = y(2)
      r     = y(3)

CD      PRINT *,t, epsi, delta, 'dt = ', dt, r0(t)
      dydt(1) = p
      dydt(2) = -r0dot(r0, t, dt) - (alpha - a0(t) ) / epsi**2/delta**2
      dydt(3) = 0.0
      dydt(4) = 0.0


      RETURN
      END
      FUNCTION scalars(t)
      IMPLICIT none
      REAL t
      INTEGER nstep
      PARAMETER (nstep = 640)
      REAL alpha(0:nstep-1), r(0:nstep-1)

      INTEGER i
      REAL a0, r0, epsi, tau, scalars, sset

      SAVE alpha, r

      RETURN

      ENTRY sset(t)
      READ (10, 9001,ERR=9999) (alpha(i), r(i), i=0, nstep-1)
 9001 FORMAT (F6.4, 2x, F9.6)

      sset = 0.
      RETURN
 9999 CONTINUE
      sset = 1.0
      RETURN
      
      ENTRY r0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nstep-1)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nstep-1) THEN
        tau = nstep-2
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      r0 = r(INT(tau)) + 
     1        epsi * (r(INT(tau)+1) - r(int(tau)))
      RETURN

      ENTRY a0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nstep-1)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nstep-1) THEN
        tau = nstep-2
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      a0 = alpha(INT(tau)) + 
     1        epsi * (alpha(INT(tau)+1) - alpha(int(tau)))
      RETURN
      END
      REAL FUNCTION r0dot(r0, t, dt)
C     Function to take the derivative of an external function of one
C       variable.  Note that the external function must handle out of
C       bounds arguments.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      REAL r0, t, dt
      EXTERNAL r0
      REAL yp, ym

      yp = r0(t+dt)
      ym = r0(t-dt)
      r0dot = (yp-ym)/2./dt

      RETURN
      END
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
