      SUBROUTINE STRAIN(LRHS, E11, E12, E22, U, V, PM, PN)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -CALCULATION OF THE STRAIN RATE TENSOR
C  METHOD:
C     -DETERMINATION OF STRAIN RATES AT GRID CENTER POINTS BY FIRST
C       INTERPOLATING TO 1/2 WAY BETWEEN THE GRID EDGE POINTS, AND
C       THEN DIFFERENTIATING
C     -USE OF EQ.(A5) IN LEPPAERANTA AND HIBLER (85)
C  INTERFACE:
C     -LRHS: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LRHS
      REAL U(L, M, 3), V(L, M, 3)
      REAL E11(L,M), E12(L,M), E22(L,M)

      REAL PM(0:L,0:M), PN(0:L,0:M)
C=======================================================================
C     -E11:  X-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E22:  Y-COMPONENT OF VOLUMETRIC (BULK) STRAIN RATE
C     -E12:  DEVIATORIC (SHEAR) STRAIN RATE
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  CALCULATE D(U)/DX
C-----------------------------------------------------------------------
      DO 10 J=1,MM
      DO 10 I=1,LM
       E11(I,J)=0.5*PM(I,J)*((U(I+1,J,LRHS)+U(I+1,J+1,LRHS))
     1                      -(U(I,J,LRHS)  +U(I,J+1,LRHS)))/DX
C-----------------------------------------------------------------------
C  CALCULATE D(V)/DY
C-----------------------------------------------------------------------
       E22(I,J)=0.5*PN(I,J)*((V(I,J+1,LRHS)+V(I+1,J+1,LRHS))
     1                      -(V(I,J,LRHS)  +V(I+1,J,LRHS)))/DY
C-----------------------------------------------------------------------
C  CALCULATE 0.5[D(V)/DX+D(U)/DY]
C-----------------------------------------------------------------------
       E12(I,J)=0.25*PM(I,J)*((V(I+1,J,LRHS)+V(I+1,J+1,LRHS))
     1                       -(V(I,J,LRHS)  +V(I,J+1,LRHS)))/DX
     2         +0.25*PN(I,J)*((U(I,J+1,LRHS)+U(I+1,J+1,LRHS))
     3                       -(U(I,J,LRHS)  +U(I+1,J,LRHS)))/DY
   10 CONTINUE
      RETURN
      END
