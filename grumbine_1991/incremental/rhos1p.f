      FUNCTION rhos1p(s, t, p)
C     This is an approximate representation of rho, fitted over a
C       characteristic range of t, s to the complete eqn. of state.
C     Compute the deviation from the reference density.
      REAL s, t, p, rhos1p
      REAL alpha, beta, gamma
      PARAMETER (alpha = -4.5795E-2)
      PARAMETER (beta  = -6.8927E-3)
      PARAMETER (gamma =  0.80908  )
C     reference values are T= -0.5, S=34.6, P = 0.0

      rhos1p = t*(alpha + beta*t) + gamma*s

      RETURN
      END
