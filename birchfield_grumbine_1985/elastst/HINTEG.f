      FUNCTION HINTEG(ISNB) 
C     THIS FUNCTION PERFORMS A TRAPEZIODAL RULE INTEGRATION FROM
C       ISNB-1 TO ISSB
  
      INTEGER MXSB
      PARAMETER (MXSB  = 160) 
  
      COMMON/ISCOM/HGT(0:MXSB,0:1),H, HP, ACMRAT, SLOPE, TIMELP,
     1    OSCARG, TIMSTP, GRDSTP, SNLAMP, SNLMPT, SNLXIN, TRIPS,
     2    RNETAC, POLFLX, MSNAUT, MAPRIM, OUTDAT, DELTAX, DELTAT, 
     3    DHPDT(0:MXSB), HPCALL, ISSB 
  
      REAL HINTEG, HGT, ACMRAT, SLOPE, TIMELP, OSCARG 
      REAL TIMSTP, GRDSTP, SNLAMP, SNLMPT, SNLXIN 
      REAL RNETAC, POLFLX, MSNAUT, MAPRIM 
      REAL DELTAX, DELTAT, DHPDT
      INTEGER ISNB, ISSB, I,H, HP, TRIPS, HPCALL
      LOGICAL OUTDAT
  
      REAL VOL
  
      VOL = (HGT(ISNB-1,H)+HGT(ISNB-1,HP))/2.0
      VOL = VOL +(HGT(ISSB,H)+HGT(ISSB,HP))/2.0 
      DO 100 I=ISNB,ISSB-1
        VOL = VOL + HGT(I,H)+HGT(I,HP)
 100  CONTINUE
      HINTEG=VOL*DELTAX 
  
      RETURN
      END 
