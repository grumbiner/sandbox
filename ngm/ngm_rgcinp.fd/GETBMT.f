            SUBROUTINE GETBMT(LM,PSTAR,P,KMAX)
      IMPLICIT REAL (A-H, O-Z)
      DIMENSION P(KMAX)
C
      DPK = P(2) - PSTAR
      DPK1 = P(1) - PSTAR
      DPK2 = DPK*DPK
      DPK12 = DPK1*DPK1
        IF ( DPK2 .GT. DPK12 ) THEN
           LM = 1
        ELSE
           LM = KMAX
        ENDIF
C
        DO 10 K=2,KMAX
        DPK = P(K) - PSTAR
        DPK1 = P(K-1) - PSTAR
        DPCK = DPK * DPK1
        IF( DPCK .LE. 0.0E0 ) THEN
           DPK2 = DPK*DPK
           DPK12 = DPK1*DPK1
           IF( DPK2 .LE. DPK12 ) THEN
              LM = K
           ELSE
              LM = K - 1
           ENDIF
           GO TO 100
        ENDIF
 10     CONTINUE
 100    CONTINUE
C
        RETURN
        END
