      SUBROUTINE VECMAXC(F1, F2, F3)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL F1(0:L,0:M), F2(0:L,0:M), F3(0:L,0:M)
      INTEGER I, J
      REAL FLAG
C=======================================================================
      DO 1 J=0,M
      DO 1 I=0,L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.+FLAG)+F2(I,J)*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMINC(F1, F2, F3)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING AN ARRAY VARIABLE
C=======================================================================
      DO 2 J=0, M
      DO 2 I=0, L
       FLAG=SIGN(1.,F1(I,J)-F2(I,J))
       F3(I,J)=0.5*(F1(I,J)*(1.-FLAG)+F2(I,J)*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
