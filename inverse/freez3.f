      PROGRAM freez3
C     Test extrapolating ice concentrations.
      IMPLICIT none
      INTEGER nmon, nvars, npmon
      PARAMETER (nmon = 639)
      PARAMETER (nvars = 4)
      PARAMETER (npmon = 1)
      REAL epsilon, beta, delta
      REAL sset, a0, t, dt, r0
      PARAMETER (dt = 1./npmon)
      REAL alpha(0:nmon*npmon), p(0:nmon*npmon) 
      REAL r(0:nmon*npmon), rho(0:nmon*npmon)
      
      REAL ier, y(nvars), dydt(nvars), yout(nvars)
      INTEGER i, j
      REAL r0dot, rfrac
      
      OPEN (10, FILE='bering.dat', FORM='FORMATTED', STATUS='OLD')
      OPEN (12, FILE='freezout', FORM='FORMATTED', STATUS='UNKNOWN')
      ier = sset(0.)
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
      rho(0)   = 0.0

C     Make up values for a straw man, assumes that input is perfect.
      CALL r0guess(dt, nmon)

C     Find the 'smoothed' values.
      DO 1000 i = 1, INT(rfrac*nmon/dt+0.5)
        t = i * dt
        CALL derivs(t, y, dydt, epsilon, delta, dt)
        CALL rk4(y, dydt, nvars, t, dt, yout, 
     1              epsilon, delta)
        alpha(i) = yout(1)
        p(i)     = yout(2)
        r(i)     = -p(i)
        rho(i)   = 0.0
        yout(3) = 0.0
        yout(4) = 0.0
        
        DO 1010 j = 1, nvars
          y(j) = yout(j)
 1010   CONTINUE

        WRITE (12,9003) t,
     1         alpha(i), a0(t), r(i), r0(t), p(i)+r(i), 
     2        (a0(t+dt)-a0(t-dt))/2./dt + r0dot(t, dt)
 1000 CONTINUE

      DO 2000 i = 0, INT(nmon/dt+0.5)
CD        t = i*dt
CD        WRITE (*,9003) t, alpha(i), r(i), 
CD     1                   p(i)+r(i), 
CD     2        (a0(t+dt)-a0(t-dt))/2./dt + r0dot(t, dt)
 2000 CONTINUE
 9003 FORMAT (F8.2, 6F8.4)

      PAUSE
      END
