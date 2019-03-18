      PROGRAM plan2
C     As part of variational study, try solving for motion of bodies
C       under central force, non-relativistic though possibly under 
C       'short range' potential.

      REAL tdot0, r0, rnot, k
      REAL dt, tau1, tau2, pi
      REAL r, theta, thetap1, rm1, rp1
      INTEGER i, nstep

      OPEN (10, FILE='planin', FORM='FORMATTED', STATUS='OLD')
      PRINT *,'Enter initial angular frequency'
      READ (10,9001) tdot0
      PRINT *,'Enter initial distance'
      READ (10,9001) r0
      PRINT *,'Enter length scale for force'
      READ (10,9001) rnot
      PRINT *,'Enter strength of potential field'
      READ (10,9001) k
      PRINT *,'Enter the number of steps to take'
      READ (10,9002) nstep
 9001 FORMAT (E13.6)
 9002 FORMAT (I5)

      tau1 = r0**2/tdot0
      tau2 = r0**1.5/k**0.5
      print *,'Time scales ',tau1, tau2

      dt = 0.1*AMIN1(tau1, tau2)
      rm1 = r0
      r   = r0
      theta = 0
      pi = ATAN(1.)*4.
      DO 1000 i = 1, nstep
        rp1 = 2*r-rm1+   tdot0*r0*r0/r**3*dt**2
     1      - k*exp(-r/rnot)/r*(1/r+1/rnot)*dt**2
        thetap1 = theta + dt* tdot0*r0**2/rp1**2
        print *, i, rp1, (rp1-r)/dt, thetap1
        rm1 = r
        r   = rp1
        theta = MOD(thetap1, 2.*pi)
 1000 CONTINUE

      STOP
      END
