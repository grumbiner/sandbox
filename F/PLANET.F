      PROGRAM planet

C     As part of variational study, try solving for motion of bodies

C       under central force, non-relativistic though possibly under 

C       'short range' potential.



      REAL tdot0, r0, rnot, k

      REAL dt, tau1, tau2

      REAL r, theta, rm1, rp1

      INTEGER i



      PRINT *,'Enter initial angular frequency'

      READ (*,9001) tdot0

      PRINT *,'Enter initial distance'

      READ (*,9001) r0

      PRINT *,'Enter length scale for force'

      READ (*,9001) rnot

      PRINT *,'Enter strength of potential field'

      READ (*,9001) k

 9001 FORMAT (E13.6)



      tau1 = r0**2/tdot0

      tau2 = r0**1.5/k**0.5

      print *,'Time scales ',tau1, tau2



      dt = 0.05*AMIN1(tau1, tau2)

      rm1 = r0

      r   = r0

      DO 1000 i = 1, 40

        rp1 = 2*r-rm1+tdot0*r0*r0/r**3 + k*exp(-r/rnot)/r*(1/r+1/rnot)

        theta = tdot0*r0**2/rp1**2

        print *, i, rp1, theta

        rm1 = r

        r   = rp1

 1000 CONTINUE



      STOP

      END

