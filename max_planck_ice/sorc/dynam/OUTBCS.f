      SUBROUTINE OUTBCS(ETA, ZETA)
      IMPLICIT none
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
C  PURPOSE:
C     -SETS FORCE DUE TO INTERNAL ICE STRESS TO 0 AT OUTFLOW POINTS
C  METHOD:
C     -VISCOSITIES AND ICE STRENGTH ARE SET TO 0 AT OUTFLOW POINTS
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      COMMON/PRESS/P(L,M)
      REAL P
      COMMON/OUTFLOW/NOUT,IOUT(LDO),JOUT(LDO)
      INTEGER NOUT, IOUT, JOUT
CD      COMMON/WORK/WRK(1:L,1:M,10),ZETA(1:L,1:M),ETA(1:L,1:M)
CD      REAL WRK, ZETA, ETA
      REAL ZETA(L, M), ETA(L, M)
C=======================================================================
C     -WRK:  DUMMY REGISTERS
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER N, I, J
C=======================================================================
      DO 210 N=1,NOUT
       I=IOUT(N)
       J=JOUT(N)
       ETA(I,J)=0.0
       ZETA(I,J)=0.0
       P(I,J)=0.0
  210 CONTINUE
      RETURN
      END
