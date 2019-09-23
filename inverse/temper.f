      SUBROUTINE temper (
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, ts, qextra, hmin, delh, nx   )

C     Compute the ice surface temperature and flux excess (if any)
      IMPLICIT none

      REAL hs, hi, ts(0:nx), qextra
      REAL t, tn, tnp1
      INTEGER iter, nx

      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0
      REAL hmin, delh

      REAL conduc, latent, sense, lwdown, lwup, swnet, qs
      REAL dfdt, dqdt, f

      INTEGER itmax, i
      REAL convtol
      PARAMETER (itmax = 10)
      PARAMETER (convtol = 5.E-4)

C     Statement functions for the thermodynamics
      qs(t) = 0.622*6.11/1013.25*EXP(LOG(10.)*9.5*(t-273.16)/(t-7.66))
      dqdt(t) = qs(t)*(273.16-7.66)/(t-7.66)**2*LOG(10.)*9.5

      conduc(t) = -ks*ki*(tf-t)/(ks*hi+ki*hs)
      
      latent(t) = rhoa*cdq*ua*lv*(qs(t)-qa)

      sense(t) = rhoa*cd*ua*cp*(t-ta)

      lwdown(t) = -ei*sigma*ta**4*(0.7855+0.2232*c**2.75)

      lwup(t) = ei*sigma*t**4

      swnet(t) = -(1.-alpha)*swdown*
     1            (1.-i0*exp(-(taus*hs+taui*hi)))

      dfdt(t) = + ks*ki/(ks*hi+ki*hs)
     1           + rhoa*cdq*lv*ua*dqdt(t)
     2           + rhoa*cd*cp*ua 
     3           + 4.*ei*sigma*t**3

      f(t) = conduc(t) + latent(t) + sense(t) + lwdown(t) +
     1                 lwup(t) + swnet(t)

C     Have flux disequilibrium.
C     Now conduct Newton's method to arrive at equilibrium
      DO 4000 i = 0, nx
      hi = hmin+i*delh

      tnp1 = ta
      iter = 0
 1000 IF (ABS(f(tnp1)) .LE. convtol .OR. iter .GT. itmax) GO TO 2000
        tn = tnp1
        tnp1 = tn - f(tn)/dfdt(tn)
        iter = iter + 1
        GO TO 1000

 2000 CONTINUE
      IF (iter .GT. itmax) 
     1      PRINT *,'iter, f = ',iter, f(tnp1)

      ts(i) = tnp1
      IF (ts(i) .GT. tf) THEN
CD        PRINT *,'Melting '
        ts(i) = tf
        qextra = f(ts(i))
       ELSE
CD        PRINT *,'Freezing'
        qextra = 0.0
      ENDIF

 4000 CONTINUE


      RETURN
      END
