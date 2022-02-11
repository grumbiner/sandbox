       FUNCTION FLAND(T,CON,TS,PS,H,DUMMY)
C      LAND ICE/SNOW BALANCE FUNCTION 
C      FLAND(TX)=0 IF NO MELTING
C      CON IS INSOLATION + LONG WAVE RAD. TERM
       COMMON/CONST/
     1 M,MM,MP,MPP,N, 
     2 DELPHI,DELT,PI,AEARTH,CA,
     3 CP,CO,CD,P00,P0, 
     4 SIGMA1,SIGMA2,XKAPPA,SIG,XLC,
     5 XLF,RHOW,RHOS,DIFF,XKS,
     6 XKI,XIMIN,SC,
     7 ITMAX,XLAM,XLS,PWLF, 
     8 PWLS,CDPW,CDCP,CDLC,CDLS,
     9 PWPS,XKSKI,RSEA,SGKP1,SGKP2, 
     1 ZSGKPA,ZSGKPB,CLRE,SQ1,SQ2 
       TT = T * T 
       FLAND = CON - SIG * TT * TT
C      SENSIBLE HEAT
     1 - CDCP * (T-TS)
      IF (H.GT.0.0) THEN
C      SUBLIMATION OF ICE 
       FLAND = FLAND - CDLS * (RMIXI(T,PS) - RMIXI(TS,PS) * 0.8)
      ELSE
C      LATENT HEAT OF LIQUID IN SATURATED 
C      LAND SURFACE 
       FLAND = FLAND - CDLC * (RMIXL(T,PS) - RMIXL(TS,PS) * 0.8)
      ENDIF 
      RETURN
      END 