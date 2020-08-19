      SUBROUTINE VERDIF(QDS, QDT, QS, QT, HS, HT, QSB, QTB, QH, QHB)
C=======================================================================
C  PROGRAMMED BY:
C     -P.LEMKE                MPI, HAMBURG                          1987
C  PURPOSE:
C     -SIMULATION OF VERTICAL DIFFUSION
C  EXTERNALS:
C     -VECMAX:  THE THIRD ARGUMENT IS MAXIMUM OF THE FIRST TWO ARGUMENTS
C  LAST MODIFIED: 5 January 1993.
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
C=======================================================================

        REAL QS(0:L,0:M), QT(0:L,0:M), QH(0:L,0:M), QSB(0:L,0:M),
     1 QTB(0:L,0:M), QHB(0:L,0:M), QDS(0:L,0:M), QDT(0:L,0:M),
     2 HS(0:L,0:M), HT(0:L,0:M)

C=======================================================================
      INTEGER I, J
C=======================================================================
      CALL VECMAX(QDS,5.,QDS)
      CALL VECMAX(QDT,5.,QDT)
      DO 150 J=1,MM
      DO 150 I=0,L
       QS(I,J)=(HS(I,J)-QSB(I,J)*QHB(I,J))/(QH(I,J)+QDS(I,J))+QSB(I,J)
       QT(I,J)=(HT(I,J)-QTB(I,J)*QHB(I,J))/(QH(I,J)+QDT(I,J))+QTB(I,J)
  150 CONTINUE
      RETURN
      END
