      SUBROUTINE BCSH(H, LNEW, OM)
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID CENTER
C     -CUTS OUT DOMAIN
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -H:    VARIABLE TO BE TREATED
C     -LNEW: RUNNING INDEX VALUE
C  MPI Original
C  Robert Grumbine
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      IMPLICIT none

      INCLUDE "icegrid.inc"
C=======================================================================
      REAL OM(0:L,0:M)
      REAL H(0:L,0:M,2)
      INTEGER LNEW
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=0,M
      DO 10 I=0,L
       H(I,J,LNEW)=OM(I,J)*H(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       H(0,J,LNEW)=H(LM2,J,LNEW)
       H(1,J,LNEW)=H(LM ,J,LNEW)
       H(L,J,LNEW)=H(2  ,J,LNEW)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
