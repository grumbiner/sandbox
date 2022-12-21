      SUBROUTINE OUTSM(H, LNEW, HOSUM, HOSM,
     1  PN, PM, OM, NOUT, IOUT, JOUT)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  Modified By:
C     Robert Grumbine       NCEP, Camp Springs MD                 Jan 93
C  PURPOSE:
C     -SMOOTH OUTFLOW POINTS TO REDUCE ADVECTION
C     -DETERMINE STATISTICS OF OUTFLOW
C  METHOD:
C     -REPLACES OUTFLOW VALUES WITH AVERAGE OF THEIR NEIGHBORS
C  INTERFACE:
C     -H:     VARIABLE AT GRID CENTER POINT
C     -LNEW:  RUNNING INDEX VALUE
C     -HOSUM: TOTAL OUTFLOW PER TIME STEP
C     -HOSM:  SPATIALLY AVERAGED TOTAL OUTFLOW PER TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL HOSUM, HOSM
      INTEGER LNEW
      REAL PM(0:L,0:M), PN(0:L,0:M)
      REAL OM(0:L,0:M)
      INTEGER NOUT, IOUT(LDO), JOUT(LDO)
      REAL H(0:L,0:M,2), TMP(0:L,0:M)
C=======================================================================
C     -TMP: TEMPORARY REGISTER
C=======================================================================
      INTEGER I, J, N
C=======================================================================
      HOSUM=0.0
      HOSM=0.0
      DO 80 N=1, NOUT
       I=IOUT(N)
       J=JOUT(N)
C-----------------------------------------------------------------------
C  DETERMINE TOTAL OUTFLOW
C-----------------------------------------------------------------------
       HOSUM=HOSUM+H(I,J,LNEW)/(PN(I,J)*PM(I,J))
C-----------------------------------------------------------------------
C  DETERMINE SPATIAL AVERAGE OF OUTFLOW VARIABLE
C-----------------------------------------------------------------------
       TMP(I,J)=(OM(I-1,J-1)+OM(I-1,J)+OM(I-1,J+1)+OM(I,J-1)+OM(I,J+1)+
     1 OM(I+1,J-1)+OM(I+1,J)+OM(I+1,J+1))
       TMP(I,J)=AMAX1(TMP(I,J),0.1)
       H(I,J,LNEW)=( H(I-1,J-1,LNEW)*OM(I-1,J-1)
     1              +H(I-1,J,LNEW)  *OM(I-1,J)
     2              +H(I-1,J+1,LNEW)*OM(I-1,J+1)
     3              +H(I,J-1,LNEW)  *OM(I,J-1)
     4              +H(I,J+1,LNEW)  *OM(I,J+1)
     5              +H(I+1,J-1,LNEW)*OM(I+1,J-1)
     6              +H(I+1,J,LNEW)  *OM(I+1,J)
     7              +H(I+1,J+1,LNEW)*OM(I+1,J+1))/TMP(I,J)
       HOSM=HOSM+H(I,J,LNEW)/(PN(I,J)*PM(I,J))
   80 CONTINUE
      RETURN
      END
