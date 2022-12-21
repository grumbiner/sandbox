      SUBROUTINE SHDEF(LRHS, OPEW)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CREATION OF EXTRA OPEN WATER DUE TO SHEAR DEFORMATION (HIBLER 84)
C  METHOD:
C     -ADDITIONAL DYNAMIC TERM ENTERING THE COMPACTNESS EQUATION
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR NEW TIME STEP
C     -OPEW: STRAIN RATE PART OF EQ.A7 IN STOESSEL (90)
C  EXTERNALS:
C     -STRAIN: CALCULATES STRAIN RATE TENSOR
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
C     COMMON viscp moved to rheology.inc
      INTEGER LRHS
      COMMON/WORK/TMP(1:L,1:M), E11(1:L,1:M), E22(1:L,1:M), E12(1:L,1:M)
     1 ,  SPACE(1:L,1:M,8)
      REAL TMP, E11, E22, E12, SPACE
      REAL OPEW(0:L,0:M)
      INTEGER I, J
      REAL DELT, DELT1
C=======================================================================
      CALL STRAIN(LRHS, E11, E12, E22)
      DO 10 J=1,MM
      DO 10 I=1,LM
       DELT=(E11(I,J)**2+E22(I,J)**2)*(1.0+ECM2)+4.0*ECM2*E12(I,J)**2
     1     +2.0*E11(I,J)*E22(I,J)*(1.0-ECM2)
       DELT1=SQRT(DELT)
       OPEW(I,J)=0.5*(DELT1-E11(I,J)-E22(I,J))
   10 CONTINUE
      RETURN
      END
