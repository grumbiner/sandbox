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
      DOUBLE PRECISION pk1, pk2
      DOUBLE PRECISION beta, tempor
      
      beta = DLOG(10.D0)
      
      tk = temp+273.15D0
      salt = DBLE(rsalt)
 
CD      PRINT *,tco2, alk, temp, salt, depth
      
      pk1 = -13.7201 + 0.031334*tk + 3235.76/tk 
     1      + 1.3D-5*tk*salt - 0.1032*SQRT(salt)
     
      pk2 = 5371.9645 - 2194.3055*DLOG10(tk)
     1      + DLOG10(salt)*(18.3802 - 5617.11/tk)
     2      -128375.28/tk + 1.671221*tk 
     3      + salt*(2.136/tk - 8.0944D-4*tk + 0.22913)
     
      k1 = DEXP(-beta*pk1)
      k2 = DEXP(-beta*pk2)

      eps = 0.0
      z = alk - eps
      dummy = 1. - 4.*k2/k1
      
      x = tco2 - SQRT(tco2*tco2-(2.*tco2-z)*z*dummy) 
      x = x / dummy
      
      tempor = k2*2.*x*x/(alk-x-eps)/k1
      
CD      PRINT *,'cco2, hco3, alk, tco2, dummy', 
CD	 1          tempor, x, alk, tco2, dummy
	 
      cco2 = tempor
      
      RETURN
      END