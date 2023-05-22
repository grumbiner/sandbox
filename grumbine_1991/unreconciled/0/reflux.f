C***********************************************************----------!!
      SUBROUTINE REFLUX(VT, VC, SS, SD, H, FLM, FLS,
     1                          SCRIT, nx, ny, DX   )
C     RECOMPUTE FLUXES USING A BOTTOM WATER DEFINITION

      INTEGER nx, ny
      REAL VT(nx, ny), VC(nx, ny), SS(nx, ny), SD(nx, ny)
      REAL H(nx, ny), FLM(ny), FLS(ny)
      REAL DX, DY, SCRIT
      INTEGER I, J
      REAL iconst

      iconst = DX*0.5*H(1,1)
      DO 1000 J = 1, ny
        FLM(J) = 0.0
        FLS(J) = 0.0
        DO 1010 I = 1, nx
C         LOWER LAYER
          IF (VT(I,J)-VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)-SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + (VT(I,J) - VC(I,J))
              FLS(J) = FLS(J) +
     1                    (VT(I,J) - VC(I,J))*(SS(I,J)-SD(I,J))
            ENDIF
          ENDIF
C         UPPER LAYER
          IF (VT(I,J)+VC(I,J) .GT. 0.0) THEN
            IF (SS(I,J)+SD(I,J) .GT. SCRIT) THEN
              FLM(J) = FLM(J) + (VT(I,J) + VC(I,J))
              FLS(J) = FLS(J) +
     1                    (VT(I,J) + VC(I,J))*(SS(I,J)+SD(I,J))
            ENDIF
          ENDIF
 1010   CONTINUE
        FLM(J) = FLM(J)*iconst
        FLS(J) = FLS(J)*iconst
 1000 CONTINUE

      RETURN
      END
