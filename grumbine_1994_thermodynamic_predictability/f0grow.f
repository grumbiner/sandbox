      SUBROUTINE grower(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, lwd, lwu,
     3  cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, ts, qextra, dhdt, hmin, delh, nx, ny)

C     Compute the ice and snow growth rate, given the thermodynamics.
C     Need to add the snow growth/decay.

      IMPLICIT none

      REAL hs, hi, qextra(nx, ny), ts(nx, ny)
      REAL dhdt(nx, ny)
CD    REAL dhsdt(nx, ny)
      REAL delh, hmin
      INTEGER nx, ny

      REAL tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL cdq, cd, ua, cp, sigma, taus, taui, i0

      REAL ta(nx, ny), swdown(nx, ny), fw(nx, ny), qa(nx, ny)
      REAL lwd(nx, ny), lwu(nx, ny)
      INTEGER i, j

      hi = hmin
      IF (hi .GT. 0.0) THEN
        DO 1100 j = 1, ny
          DO 1000 i = 1, nx
            dhdt(i,j) = (-fw(i,j)+qextra(i,j) + ks*ki*(tf-ts(i,j))
     1                /(ks*hi+ki*hs))/rhoi/lf
CD            dhsdt(i,j) = 0.
 1000     CONTINUE
 1100   CONTINUE

       ELSE

        DO 2100 j = 1, ny
          DO 2000 i = 1, nx
            dhdt(i,j) = ( qextra(i,j) )/rhoi/lf
 2000     CONTINUE
 2100   CONTINUE

      ENDIF

      RETURN
      END
