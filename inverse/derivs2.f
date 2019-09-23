      SUBROUTINE derivs(t, y, dydt, epsi, delta, dt)

      IMPLICIT none
      REAL t, dt, epsi, delta, r0, r0dot
      INTEGER nvar
      PARAMETER (nvar = 4)
      REAL y(nvar), dydt(nvar)

      REAL alpha, p, r, rho, a0

      alpha = y(1)
      p     = y(2)
      r     = y(3)
      rho   = y(4)

      dydt(1) = p
      dydt(2) = -r0dot(t,dt) - (alpha - a0(t) ) / epsi**2/delta**2
      dydt(3) = 0.0
      dydt(4) = 0.0


      RETURN
      END
