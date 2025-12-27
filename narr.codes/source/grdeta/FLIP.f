C-----------------------------------------------------------------------
      SUBROUTINE FLIP(KD)
c     SUBROUTINE FLIP(AE,KD)
C
C SUBPROGRAM: FLIP          REVERSE THE VERTICAL INDEXING OF ARRAY AE
C   PRGMMR: T.BLACK         ORG: W/NMC22    DATE: ??-??-??
C
C ABSTRACT: REVERSES THE VERTICAL INDEXING OF ARRAY AE, WHICH IS REVERSE
C           FROM VERICAL INDEXING IN THE ETA MODEL
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T.BLACK
C
C USAGE     CALL FLIP(AE,KD)
C   INPUT ARGUMENT LIST:
C     AE       - INPUT ARRAY
C     KD       - NUMBER OF VERTICAL LEVELS
C
C   OUTPUT ARGUMENT LIST:
C     AE       - FLIPPED OUTPUT ARRAY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C------------------------------------------------------------------------
      INCLUDE "parmeta.h"
      COMMON /BIGGBL/ AL(IMAX,JMAX,LMAX)
C
      DIMENSION TEMP(IMAX,JMAX)
c     DIMENSION AL(IMAX,JMAX,KD),TEMP(IMAX,JMAX)
C
      KDH = KD / 2
      DO 100 KN = 1, KDH
        K = KD + 1 - KN
        DO 50 J = 1, JMAX
        DO 50 I = 1, IMAX
          TEMP(I,J) = AL(I,J,KN)
          AL(I,J,KN) = AL(I,J,K)
          AL(I,J,K) = TEMP(I,J)
  50      CONTINUE
 100  CONTINUE
      RETURN
      END
