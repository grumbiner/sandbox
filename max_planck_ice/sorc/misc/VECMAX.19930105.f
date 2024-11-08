      SUBROUTINE VECMAX(F1, VALUE, F2)
      IMPLICIT none
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MAXIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C  LAST MODIFIED: 5 January 1993
C  MPI Original
C  Robert Grumbine
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL F1(0:L,0:M), F2(0:L,0:M), VALUE
      INTEGER I, J
      REAL FLAG
C=======================================================================
      DO 1 J=0, M
      DO 1 I=0, L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.+FLAG)+VALUE*(1.-FLAG))
    1 CONTINUE
      RETURN
C=======================================================================
      ENTRY VECMIN(F1, VALUE, F2)
C=======================================================================
C  PURPOSE:
C     -THIRD ARGUMENT IS MINIMUM OF FIRST AND SECOND ARGUMENT, THE
C       SECOND ONE BEING A CONSTANT
C=======================================================================
      DO 2 J=0, M
      DO 2 I=0, L
       FLAG=SIGN(1.,F1(I,J)-VALUE)
       F2(I,J)=0.5*(F1(I,J)*(1.-FLAG)+VALUE*(1.+FLAG))
    2 CONTINUE
      RETURN
      END
