      SUBROUTINE temper (
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, lwd, lwu,
     3  cdq, cd, ua, cp, sigma, taus, taui, fw, rh, i0,
     3  hi, hs, ts, qextra, hmin, delh, nx, ny   )

C     Compute the ice surface temperature and flux excess (if any)
      IMPLICIT none

      REAL hs, hi, ts(nx, ny), qextra(nx, ny)
      REAL t, tn, tnp1
      INTEGER iter, nx, ny

      REAL tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL cdq, cd, ua, cp, sigma, taus, taui, i0

      REAL ta(nx, ny), swdown(nx, ny), fw(nx, ny), rh(nx, ny)
      REAL lwd(nx, ny), lwu(nx, ny), albedo

      REAL hmin, delh

      REAL conduc, latent, sense, lwdown, lwup, swnet, qs
      REAL dfdt, dqdt, f

      INTEGER itmax, i, j
      REAL convtol
      PARAMETER (itmax = 10)
      PARAMETER (convtol = 5.E+0)
      REAL*8 w3fa09

C     Statement functions for the thermodynamics
      qs(t) = 0.622*w3fa09(t)*1000./101325. 

CD      conduc(t) = -ks*ki*(tf-t)/(ks*hi+ki*hs)
      conduc(t) = ks*ki*(tf-t)/(ks*hi+ki*hs)
      
      latent(t) = rhoa*cdq*ua*lv*(qs(t)*(1.-rh(i,j)/100.) )

      sense(t) = rhoa*cd*ua*cp*(t-ta(i,j))

      lwup(t) = ei*sigma*t**4

      swnet(t) = -(1.-albedo(ts(i,j), ta(i,j), 0.01+ hs, hi) )*
     1            swdown(i,j)*
     1            (1.-i0*exp(-(taus*hs+taui*hi)))

      f(t) = conduc(t) + latent(t) + sense(t) - lwd(i,j) +
     1                 lwup(t) + swnet(t) 

      dfdt(t) = ( f(t+0.01) - f(t-0.01) ) / 0.02

C     Have flux disequilibrium.
C     Now conduct Newton's method to arrive at equilibrium
      hi = hmin
CMIC$ DO ALL AUTOSCOPE
      DO 4000 j = 1, ny
        DO 4100 i = 1, nx

        tnp1 = ta(i,j)
        iter = 0
 1000   IF (ABS(f(tnp1)) .LE. convtol .OR. iter .GT. itmax) GO TO 2000
          tn = tnp1
          tnp1 = tn - f(tn)/dfdt(tn)
          IF (tnp1 .GT. 325. .OR. tnp1 .LE. 150.) THEN
            iter = itmax
            tnp1 = AMAX1(tnp1, 150.)
            tnp1 = AMIN1(tnp1, 325.)
            GO TO 2000
          ENDIF

          iter = iter + 1
          GO TO 1000

 2000   CONTINUE
        IF (iter .GT. itmax) THEN 
CD          PRINT *,'iter, f = ',iter, tnp1, f(tnp1)
            ts(i,j) = tf
            qextra(i,j) = -lf*rhoi/86400.
        ENDIF

        ts(i,j) = tnp1
        IF (ts(i,j) .GT. tf) THEN
CD          PRINT *,'Melting '
          ts(i,j) = tf
          qextra(i,j) = f(ts(i,j))
         ELSE
CD        PRINT *,'Freezing'
          qextra(i,j) = 0.0
        ENDIF

 4100   CONTINUE
 4000 CONTINUE

 9001 FORMAT (2I3, 2F6.1, F9.1, F8.1, F8.1, 2F9.1, F9.4)

      RETURN
      END
