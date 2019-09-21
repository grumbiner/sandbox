      SUBROUTINE grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, lwd, lwu,
     3  cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, ts, qextra, dhdt, hmin, delh, nx)

C     Compute the ice and snow growth rate, given the thermodynamics.
C     Need to add the snow growth/decay.

      IMPLICIT none

      REAL hs, hi, qextra, ts(0:nx), dhdt(0:nx)
      REAL lwd, lwu
      REAL delh, hmin
      INTEGER nx

      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0
      INTEGER i

      DO 1000 i = 0, nx
        hi = hmin + i*delh
        dhdt(i) = (-fw+qextra+ ks*ki*(tf-ts(i))/(ks*hi+ki*hs))/rhoi/lf
CD        dhsdt(i) = 0.
 1000 CONTINUE

      RETURN
      END
