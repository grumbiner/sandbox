      PROGRAM icevary
C     Variational inversion of concentration and growth rate information.
C     -- Coupled to a full ice thickness distribution evolution model.
C     Bob Grumbine 5/31/94.
 
      IMPLICIT none
      INCLUDE "inv.inc"

C     Evolution parameters
      REAL dtf
      PARAMETER (dtf   = dh/fref/days)
      REAL a(nstep), g0(nstep), g(nthick, nstep), f(nthick), hbar

C     Inversion parameters.
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
