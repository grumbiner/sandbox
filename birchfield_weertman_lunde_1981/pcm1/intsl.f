      SUBROUTINE INTSL(LEN, KRAT, FLDS, FLDL, NIX)
C     REVERSE INDEXES, USE QUASI-HERMITE INTER.   10/4/79 
      REAL FLDS(LEN), FLDL(*) 
      REAL C(31, 3) 
      DATA IC/31/ 
      NIX  = 0
      M    = KRAT*(LEN-1)+1 
      IF ( LEN.LE.IC .AND. LEN.GE.4 ) GO TO 1 
      NIX  = 1
      RETURN
 1    CONTINUE
      LENH = (LEN+1)/2
      DO 20 L = 1, LENH 
        FX            = FLDS(L) 
        FLDS(L)       = FLDS(LEN-L+1) 
        FLDS(LEN-L+1) = FX
 20   CONTINUE
C     CALL IMSL ROUTINES MODIFIED FOR 
C     REGULAR GRID SPACINGS ( IN X, U ) 
      CALL IQHSCU(KRAT, FLDS, LEN, C, IC, IER)
  
      IF (IER.NE.0) THEN
         NIX = IER*10 
       ELSE 
         CALL ICSEVU(KRAT, FLDS, LEN, C, IC, FLDL, M, IER)
         IF (IER.EQ.0) RETURN 
         NIX = IER*100
      ENDIF 
  
      RETURN
      END 
