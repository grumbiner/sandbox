      SUBROUTINE derivs(t, y, dydt, epsi, delta)

      IMPLICIT none
      INCLUDE "inv.inc"

      REAL t, epsi, delta, r0, r0dot
      REAL y(nvars), dydt(nvars)

      REAL alpha, p, r, a0
      EXTERNAL a0, r0, r0dot

      alpha = y(1)
      p     = y(2)
      r     = y(3)

      dydt(1) = p
      dydt(2) = -r0dot(r0, t, dt) - (alpha - a0(t) ) / epsi**2/delta**2
      dydt(3) = 0.0
      dydt(4) = 0.0


      RETURN
      END
