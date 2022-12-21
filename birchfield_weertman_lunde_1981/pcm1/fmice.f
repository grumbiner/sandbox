      SUBROUTINE FMICE(LHLS, KRAT, MPREF, MZERO, NIX) 
C     INTERPOLATE FM TO ICE MODEL SCALE      11/28/79 
  
      COMMON W(46, 50)
      COMMON/MINOR/NMINOR, JFM, DUMM(25)
      COMMON/WSPACE/G(46) 
      COMMON/CONST/M, MM, MP, MPP, N, DUM(42) 
  
      REAL FMI(181) 
  
      NIX = 0 
      MX  = MP/MPREF
  
      IF (MX.GT.0.AND.MOD(KRAT, MX).EQ.0) THEN
        KRATX  = KRAT/MX
        LHLX   = 1+(LHLS-1)*MX
        MZEROX = 1+(MZERO-1)*MX 
        MZ = MZEROX 
        DO 10 L = 1, LHLX 
          G(L) = W(MZ, JFM) 
          MZ   = MZ+1 
  10    CONTINUE
        CALL INTSL(LHLX, KRATX, G, FMI, NIX)
        NIX = NIX*100 
       ELSE 
        NIX = 1 
      ENDIF 
  
      RETURN
      END 
