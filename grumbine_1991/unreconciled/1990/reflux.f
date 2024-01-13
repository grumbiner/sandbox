      SUBROUTINE REFLUX(VT, VC, SS, SD, H, FLM, FLS,
     1                          SCRIT, NX, NY, DX, DY)
C     RECOMPUTE FLUXES USING A BOTTOM WATER DEFINITION

      INTEGER NX, NY
      REAL VT(NX, NY), VC(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY), FLM(NY), FLS(NY)
      REAL DX, DY, SCRIT
      INTEGER I, J

      DO 1000 J = 1, NY
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, NX
C         LOWER LAYER
          IF (VT(I,J)-VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)-SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) - VC(I,J))
              FLS(J) = FLS(J) +H(I,J)*0.5*
     1                    (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))
            ENDIF
          ENDIF
C         UPPER LAYER
          IF (VT(I,J)+VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)+SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + H(I,J)*0.5*(VT(I,J) + VC(I,J))
              FLS(J) = FLS(J) +H(I,J)*0.5*
     1                    (VT(I,J) + VC(I,J))*(SS(I,J)+SD(I,J))
            ENDIF
          ENDIF
 1010   CONTINUE
        FLM(J) = FLM(J)*DX
        FLS(J) = FLS(J)*DX
 1000 CONTINUE

      RETURN
      END
