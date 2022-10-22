C*************************************************----------++++++++++!!
      FUNCTION n2co2(tco2, alk, temp, rsalt, depth)
C     Compute the concentration of co2 (Moles/kg) given
C       the total co2, alkalinity, temperature, salinity, 
C       and depth.
C     8/10/91. BG.
      IMPLICIT none
      
      DOUBLE PRECISION n2co2
      DOUBLE PRECISION tco2, alk, temp, depth, salt
      REAL rsalt
      
      DOUBLE PRECISION  k1,  k2, tk
      DOUBLE PRECISION x, eps, z, dummy
      DOUBLE PRECISION pk1, pk2, pk10, pk20
      DOUBLE PRECISION beta, tempor
      
      beta = DLOG(10.D0)
      
      tk = temp+273.15D0
      salt = DBLE(rsalt)
 
CD      PRINT *,tco2, alk, temp, salt, depth
C     1987 Unesco values:
      pk10 = 6320.81/tk - 126.3405 + 19.568*LOG(tk)
      pk20 = 5143.69/tk -  90.1833 + 14.613*LOG(tk)
      
      pk1 = pk10 + 0.0068*salt + 
     1 (19.894 - 840.39/tk - 3.0189*LOG(tk))*SQRT(salt)
             
      pk2 = pk20 + 0.0217*salt + 
     1 (17.176 - 690.59/tk - 2.6719*LOG(tk))*SQRT(salt)
     
      k1 = DEXP(-beta*pk1)
      k2 = DEXP(-beta*pk2)

      eps = 0.0
      z = alk - eps
      dummy = 1. - 4.*k2/k1
      
      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy) 
      x = x / dummy
      
      tempor = k2*2.*x*x/(alk-x-eps)/k1
      
CD      PRINT *,'nco2, hco3, alk, tco2, dummy', 
CD	 1          tempor, x, alk, tco2, dummy
      PRINT *,'k1, k2, k2/k1, cco2', pk1, pk2, k2/k1, tempor

      n2co2 = tempor
      
      RETURN
      END
 