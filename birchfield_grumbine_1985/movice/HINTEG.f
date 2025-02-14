      FUNCTION HINTEG(ISNB, DELTAX, ISSB) 
C     THIS FUNCTION PERFORMS A TRAPEZIODAL RULE INTEGRATION FROM
C       ISNB-1 TO ISSB
  
      INTEGER MXSB
      PARAMETER (MXSB  = 160) 
  
      COMMON/ISCOM/HGT(0:MXSB,0:1), H, HP, HTOPO(0:MXSB)
  
      REAL HINTEG, HGT, DELTAX
      INTEGER ISNB, ISSB, I, H, HP
  
      REAL VOL
  
      VOL = (HGT(ISNB-1,H)+HGT(ISNB-1,HP))/2.0
      VOL = VOL +(HGT(ISSB,H)+HGT(ISSB,HP))/2.0 
      DO 100 I=ISNB,ISSB-1
        VOL = VOL + HGT(I,H)+HGT(I,HP)
 100  CONTINUE
      HINTEG=VOL*DELTAX 
  
      RETURN
      END 
