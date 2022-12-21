C Robert Grumbine 13 Dec 2004
C Elements to be incorporated in the hycom include files, or in a
C separate ice include file:
      REAL ks, ki, rhoa, cdq, lv, cd, cp, ei, sigma
      PARAMETER (sigma  = 5.67051E-8 )   !Stefan-Boltzmann constant
CORIG      PARAMETER (ki     = 2.1656)
      PARAMETER (ki     = 2.03)  !as used by winton
      PARAMETER (ks     = 0.31)
      PARAMETER (rhoa   = 1.29)
      PARAMETER (cdq    = 1.75E-3)
      PARAMETER (cd     = 1.75E-3)
      PARAMETER (lv     = 2.5008E6 )  !Gill 0C
      PARAMETER (cp     = 1004.6)     !NMC Handbook
      PARAMETER (ei     = 0.97) 

C Code elements to be declared before function:
      REAL dfdt

C Function must be passed:
C  ua  wind speed (scalar speed, not wind stress nor velocity component)
C  ts  surface temperature in Kelvin
C     Based on Grumbine 1994 code
C     Winton sbr computes thermal conduction term itself
C     Second two lines are computing dq/dt
      dfdt(ua, ts) = 4.*ei*sigma*ts**3 +  rhoa*cd*cp*ua + 
     1  rhoa*cdq*lv*ua* (273.16-7.66)/(ts-7.66)**2*LOG(10.)*9.5 *
     2  0.622*6.11/1013.25*EXP(LOG(10.)*9.5*(ts-273.16)/(ts-7.66))

