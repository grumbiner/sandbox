      REAL FUNCTION r0ddot(r0, t, dt)
C     Function to take the second derivative of an external function of 
C       one variable.  Note that the external function must handle out 
C       of bounds arguments.
C     Robert Grumbine 5 April 1994.

      IMPLICIT none

      REAL r0, t, dt
      EXTERNAL r0
      REAL yp, ym

      yp = r0(t+dt)
      ym = r0(t-dt)
      r0ddot = (yp - 2*r0(t) + ym)/dt/dt

      RETURN
      END
