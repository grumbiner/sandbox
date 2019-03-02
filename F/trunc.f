      SUBROUTINE TRUNC(MWAVE,MWAVEX,WAVE)
C
      DIMENSION WAVE(*)
C
      MWAVEO=IABS(MWAVEX)
C
      IF(MWAVE.LE.MWAVEO) RETURN
C
      NM=0
      DO 10 M=1,MWAVE+1
      NE=MWAVE+1
      DO 10 N=M,NE
      NM=NM+2
      IF(MWAVEX.GT.0) THEN
        IF(N.GT.MWAVEO+1) THEN
          WAVE(NM-1)=0.
          WAVE(NM  )=0.
        ENDIF
      ELSE
        IF(N.NE.MWAVEO+1) THEN
          WAVE(NM-1)=0.
          WAVE(NM  )=0.
        ENDIF
      ENDIF
   10 CONTINUE
C
      RETURN
      END
