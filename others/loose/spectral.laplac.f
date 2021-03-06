      SUBROUTINE LAPLAC
     1(IDIR,WAVE,WAVED2,MAXWV,IROMB)
C
C  Mark Iredell 2 May 1995
C  TAKES LAPLACIAN OR INVERSE LAPLACIAN IN SPECTRAL SPACE
C
C  IDIR.GT.0  TAKE LAPLACIAN:  FIELD WAVED2 OUTPUT
C  IDIR.LT.0  TAKE INVERSE LAPLACIAN:  FIELD WAVE OUTPUT
C
C  WAVE ARE THE 2*NMMAX SPECTRAL COEFFICIENTS OF THE SCALAR FIELD
C  WAVED2 ARE THE 2*NMMAX SPECTRAL COEFFICIENTS OF ITS LAPLACIAN
C  NOTE THAT WAVE AND WAVED2 MAY BE THE SAME FIELD (LAPLACE IN PLACE)
C  ALSO NOTE THAT THE 0,0 SPECTRAL COMPONENT IS NOT CHANGED
C
C  WAVED2 = DEL2(WAVE)
C
C  IROMB.EQ.1 FOR RHOMBOIDAL TRUNCATION:  NMMAX=(MAXWV+1)*(MAXWV+1)
C  IROMB.EQ.0 FOR TRIANGULAR TRUNCATION:  NMMAX=(MAXWV+1)*(MAXWV+2)/2
C
      REAL WAVE(2,(MAXWV+1)*(MAXWV+2) )
      REAL WAVED2(2,(MAXWV+1)*(MAXWV+2) )
      DATA RERTH/6.3712E6/
C***********************************************************************
C  TAKE LAPLACIAN
      IF (IDIR.GT.0)  THEN
        NM = 0
        DO 10 M=0,MAXWV
        DO 10 N=M,MAXWV+IROMB*M
          RNN1A2 = -N*(N+1)/RERTH**2
          NM = NM + 1
          IF (N.GT.0)  THEN
            WAVED2(1,NM) = WAVE(1,NM)*RNN1A2
            WAVED2(2,NM) = WAVE(2,NM)*RNN1A2
          ENDIF
   10   CONTINUE
C***********************************************************************
C  TAKE INVERSE LAPLACIAN
      ELSE IF (IDIR.LT.0)  THEN
        NM = 0
        DO 20 M=0,MAXWV
        DO 20 N=M,MAXWV+IROMB*M
          RNN1A2 = -N*(N+1)/RERTH**2
          NM = NM + 1
          IF (N.GT.0)  THEN
            WAVE(1,NM) = WAVED2(1,NM)/RNN1A2
            WAVE(2,NM) = WAVED2(2,NM)/RNN1A2
          ENDIF
   20   CONTINUE
      ENDIF
C***********************************************************************
      RETURN
      END
