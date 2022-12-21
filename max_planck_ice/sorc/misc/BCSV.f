      SUBROUTINE BCSV(U, V, LNEW, VM)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -SETS CYCLIC BOUNDARY COND. FOR VALUES AT GRID EDGE
C     -CUTS OUT DOMAIN
C  METHOD:
C     -OVERLAP OF VARIABLES AT THE SEAM
C  INTERFACE:
C     -U:    X-COMPONENT OF VARIABLE TO BE TREATED
C     -V:    Y-COMPONENT OF VARIABLE TO BE TREATED
C     -LNEW: RUNNING INDEX VALUE
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL VM(L,M)
      REAL U(L,M,3), V(L,M,3)
C=======================================================================
      INTEGER LNEW, I, J
C-----------------------------------------------------------------------
C  SET VARIABLES OUTSIDE DEFINED DOMAIN TO 0
C-----------------------------------------------------------------------
      DO 10 J=1,M
      DO 10 I=1,L
       U(I,J,LNEW)=VM(I,J)*U(I,J,LNEW)
       V(I,J,LNEW)=VM(I,J)*V(I,J,LNEW)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CARRY OUT CYCLIC BC'S AS NEEDED
C-----------------------------------------------------------------------
      IF (PTYPE .EQ. 3) GO TO 9999
      DO 30 J=2,MM
       U(1,J,LNEW)=U(LM,J,LNEW)
       V(1,J,LNEW)=V(LM,J,LNEW)
       U(L,J,LNEW)=U(2 ,J,LNEW)
       V(L,J,LNEW)=V(2 ,J,LNEW)
   30 CONTINUE

 9999 CONTINUE

      RETURN
      END
