C*************************************************----------++++++++++!!
      FUNCTION cco2(tco2, alk, temp, rsalt, depth)
C     Compute the concentration of co2 (Moles/kg) given
C       the total co2, alkalinity, temperature, salinity, 
C       and depth.
C     8/10/91. BG.
      IMPLICIT none
      
      DOUBLE PRECISION cco2
      DOUBLE PRECISION tco2, alk, temp, depth, salt
      REAL rsalt
      
      DOUBLE PRECISION  k1,  k2, tk
      DOUBLE PRECISION x, eps, z, dummy
      DOUBLE PRECISION pk1, pk2, pk10, pk20
      DOUBLE PRECISION beta, co2str
      DOUBLE PRECISION tb, kb, nkb, nkb0
      DOUBLE PRECISION enew, ystar
      DOUBLE PRECISION co2old
      
      beta = DLOG(10.D0)
      tk = temp+273.15D0
      salt = DBLE(rsalt)
      tb   = rsalt*12.1E-6
 
C     1987 Unesco values:
      pk10 =  6320.81/tk - 126.3405 + 19.568* LOG(tk)
      pk20 =  5143.69/tk -  90.1833 + 14.613* LOG(tk)
      nkb0 = -8966.90/tk + 148.0248 - 24.4344*LOG(tk)
      
      pk1 = pk10 + 0.0068*salt + 
     1 (19.894 - 840.39/tk - 3.0189*LOG(tk))*SQRT(salt)
             
      pk2 = pk20 + 0.0217*salt + 
     1 (17.176 - 690.59/tk - 2.6719*LOG(tk))*SQRT(salt)

      nkb = nkb0 - 0.01767*salt +
     1 (0.5998 -  75.25/tk )*SQRT(salt)
     
      k1 = DEXP(-beta*pk1)
      k2 = DEXP(-beta*pk2)
      kb = DEXP(nkb)

C     Make a first guess at the borate contribution to alkalinity.
      eps = tb/(1. + (k2/kb)*8.)

      z = alk - eps
      dummy = 1. - 4.*k2/k1
      
      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy) 
      x = x / dummy
      
      co2str = k2*2.*x*x/(alk-x-eps)/k1
      ystar  = tco2 - x - co2str
      co2old = co2str
      
CD      PRINT *,'eps, co2', 
CD     1         eps, co2str

C     Second guess, step in iteration.
      enew = tb/(1. + (k2/kb)*x/ystar)
      z = alk - enew
      dummy = 1. - 4.*k2/k1
      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy) 
      x = x / dummy
      co2str = k2*2.*x*x/(alk-x-eps)/k1
      ystar  = tco2 - x - co2str
      
CD      PRINT *,'deleps, co2, delco2', 
CD     1         (enew-eps)/eps, co2str, (co2str-co2old)/co2str
      eps = enew
      co2old = co2str

C     Third time through
      enew = tb/(1. + (k2/kb)*x/ystar)
      z = alk - enew
      dummy = 1. - 4.*k2/k1
      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy) 
      x = x / dummy
      co2str = k2*2.*x*x/(alk-x-eps)/k1
      ystar  = tco2 - x - co2str
      
CD      PRINT *,'deleps, co2, delco2', 
CD     1         (enew-eps)/eps, co2str, (co2str-co2old)/co2str     
CD      PRINT *,' '

      cco2 = co2str
      
      RETURN
      END
