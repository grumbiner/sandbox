      SUBROUTINE RWSTAB(RI, ZO, K, PA1, PH1, PH2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STABILITY FUNCTIONS FM AMD FH ACCORDING TO
C       LOUIS(79) FOR VARIABLE ROUGHNESS LENGTHS
C  INTERFACE:
C     -RI:  RICHARDSON NUMBER
C     -ZO:  ROUGHNESS LENGTH (VARIABLE)
C     -K:   MAXIMUM GRID POINTS TO BE TREATED
C     -PA1: SURFACE AIR PRESSURE
C  LAST MODIFIED: 5 January 1993.
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL PH1(LMDP), PH2(LMDP)
      REAL RI(LMDP),PA1(LMDP),ZO(LMDP)
      INTEGER K
      INTEGER N
      REAL ZA, FAKT, CM, CH, FM, FH
C=======================================================================
      DO 2 N=1,K
       IF (RI(N).GE.0.) GOTO 1
       ZA=MAX(30.,((PA1(N)-100000.)*0.08))
       FAKT=(.4/LOG(ZA/ZO(N)))**2*9.4*SQRT(ZA/ZO(N))
       CM=7.4*FAKT
       CH=5.3*FAKT
       FM=1.-9.4*RI(N)/(1.+CM*SQRT(ABS(RI(N))))
       FH=1.-9.4*RI(N)/(1.+CH*SQRT(ABS(RI(N))))
       GOTO 3
    1  CONTINUE
       FM=1./(1.+2.*4.7*RI(N))**2
       FH=FM
    3  CONTINUE
       PH1(N)=FH
       PH2(N)=FM
    2 CONTINUE
      RETURN
      END
