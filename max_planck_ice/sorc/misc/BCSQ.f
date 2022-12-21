      SUBROUTINE BCSQ(Q)
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID EDGE
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -Q:    VARIABLE TO BE TREATED
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      DIMENSION Q(0:L,0:M)
C=======================================================================
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S
C-----------------------------------------------------------------------
      DO 30 J=0,M
       Q(0,J)=Q(LM2,J)
       Q(1,J)=Q(LM,J)
       Q(L,J)=Q(2,J)
   30 CONTINUE
      RETURN
      END
