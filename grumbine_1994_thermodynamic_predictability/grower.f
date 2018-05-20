      SUBROUTINE grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, ts, qextra, dhdt)

C     Compute the ice and snow growth rate, given the thermodynamics.
C     Need to add the snow growth/decay.

      IMPLICIT none

      REAL hs, hi, qextra, ts, dhdt, dhsdt

      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      dhdt = (-fw+qextra+ ks*ki*(tf-ts)/(ks*hi+ki*hs))/rhoi/lf
      dhsdt = 0.

      RETURN
      END
