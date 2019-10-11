      SUBROUTINE VAPOR(T,EST,K1,K)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     C.KOCH                  UNI, BONN                             1986
C  MODIFIED BY:
C     A.STOESSEL              MPI, HAMBURG                          1989
C  PURPOSE:
C     -CALCULATION OF SATURATION VAPOR PRESSURE FOR AIR TEMPERATURE
C       (K1=1), OVER ICE (K1=2) AND OVER WATER (K1=3)
C  INTERFACE:
C     -T:   TEMPERATURE OF ATMOSPHERE, ICE OR OCEAN
C     -EST: SATURATION VAPOR PRESSURE
C     -K1:  INDEX FOR CHOICE OF QUANTITY TO BE CALCULATED
C     -K:   INDEX FOR SELECTIVE LOOP
C  LAST MODIFIED: 7 June 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL T(LMDP),EST(LMDP)
      INTEGER K, K1
      INTEGER N
      INTEGER AIR, ICE, WATER
      PARAMETER (AIR = 1)
      PARAMETER (ICE = 2)
      PARAMETER (WATER = 3)
C=======================================================================

      IF (K1 .EQ. AIR) THEN
      DO 11 N=1,K
        EST(N)=611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),200.)-273.15+257.87))
   11 CONTINUE

       ELSE IF (K1 .EQ. ICE) THEN
      DO 22 N=1,K
        EST(N)=611.15*EXP((23.036-(MIN(T(N),273.15)-273.15)/333.7)*
     1        (MIN(T(N),273.15)-273.15)/(MAX(T(N),200.)-273.15+279.82))
   22 CONTINUE

       ELSE IF (K1 .EQ. WATER) THEN
    3 CONTINUE
      DO 33 N=1,K
        EST(N)=0.9815*611.21*EXP((18.729-(MIN(T(N),300.)-273.15)/227.3)*
     1        (MIN(T(N),300.)-273.15)/(MAX(T(N),260.)-273.15+257.87))
   33 CONTINUE

      ENDIF

      RETURN
      END
