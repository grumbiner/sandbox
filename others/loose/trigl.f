      SUBROUTINE TRIGL
     1(IDIR,RLAT,SINL,COSL,JMAX)
C
C  RETURN LATITUDES AND THEIR SINES AND COSINES
C
C       IDIR= -1 ...... GAUSSIAN GRID
C           -101 ...... EQUIDISTANT LAT/LON GRID
C
C  RLAT ARE THE OUTPUT JMAX LATITUDES
C
C  SINL ARE THE OUTPUT JMAX SINES OF LATITUDE
C
C  COSL ARE THE OUTPUT JMAX COSINES OF LATITUDE
C
      DIMENSION RLAT(JMAX), SINL(JMAX), COSL(JMAX)
      DATA PI/3.141593/
C***********************************************************************
C  GET COLATITUDES
      IF (IABS(IDIR).LT.100) THEN
        CALL GAULAT (RLAT,JMAX)
      ELSE
        DO 10 J=1,JMAX
          RLAT(J) = 180.*(J-1)/(JMAX-1)
   10   CONTINUE
      ENDIF
 
C  COMPUTE LATITUDES, SINES AND COSINES
        DO 20 J=1,JMAX
          RLAT(J) = 90. - RLAT(J)
          SINL(J) = SIN(RLAT(J)*PI/180.)
          COSL(J) = SQRT(1. - SINL(J)**2)
   20   CONTINUE
C***********************************************************************
      RETURN
      END
