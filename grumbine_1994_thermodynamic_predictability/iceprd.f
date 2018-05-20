      SUBROUTINE preder(
     1  ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c,
     2  swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0,
     3  hi, hs, ts, qextra, pred  )

C     Compute the predictability period of the ice given the atmosphere.
      IMPLICIT none

      REAL hs, hi, qextra, ts, pred
 
      REAL ta, tf, ks, ki, ei, alpha, rhoi, rhoa, lf, lv, c
      REAL swdown, cdq, cd, ua, cp, sigma, taus, taui, fw, qa, i0

      REAL conduc, swnet, dfdt, dqdt, qs, dtdh, t

      qs(t) = 0.622*6.11/1013.25*EXP(LOG(10.)*9.5*(t-273.16)/(t-7.66))
      dqdt(t) = qs(t)*(273.16-7.66)/(t-7.66)**2*LOG(10.)*9.5

      conduc(t) = - ks*ki*(tf-t)/(ks*hi+ki*hs)
      swnet(t)  = -(1-alpha)*swdown
     1          *(1.-i0*exp(-(taus*hs+taui*hi)))

      dfdt(t) = +ks*ki/(ks*hi+ki*hs)
     1           + rhoa*cdq*lv*ua*dqdt(t)
     2           + rhoa*cd*cp*ua 
     3           + 4.*ei*sigma*t**3

      dtdh(t) = (-conduc(t)*ks/(ks*hi+ki*hs)
     1           -(1-alpha)*swdown*i0*taui*EXP(-(hs*taus+hi*taui)) )
     2  / dfdt(t)  * 1.0

      pred = (1./rhoi/lf)*
     1 (+conduc(ts)*ks/(ks*hi+ki*hs) - ks*ki*dtdh(ts)/(ks*hi+ki*hs) )

      IF (pred .NE. 0.0) THEN
        pred = ABS(1./pred)
      ENDIF
 
      RETURN
      END
