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
