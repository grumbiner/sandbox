      SUBROUTINE SADVECT(RH, H, U, V, PM, PN, LRHS, LADV)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE ADVECT IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     Robert Grumbine          NCEP, Camp Springs, MD          Jan. 1993
C  PURPOSE:
C     -CALCULATION OF ADVECTION OF A SCALAR VARIABLE FOR THE CONTINUITY
C       EQUATIONS (EQ.13 AND 14 IN HIBLER 79 AND EQ.8 IN OWENS AND
C       LEMKE 90)
C  METHOD:
C     -FORWARD-BACKWARD (MATSUNO) SCHEME (SEE MESINGER AND ARAKAWA 76)
C  INTERFACE:
C     -RH:   CHANGE OF SCALAR VARIABLE
C     -H:    SCALAR VARIABLE
C     -U:    X-COMPONENT OF VELOCITY
C     -V:    Y-COMPONENT OF VELOCITY
C     -LRHS: RUNNING INDEX FOR OLD OR NEW TIME STEP
C     -LADV: RUNNING INDEX FOR NEW TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
CD      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
CD      REAL PM, PN, DNDX, DMDY

      REAL RH(0:L,0:M), H(0:L,0:M,2), U(L,M,2), V(L,M,2)
      REAL PM(0:L,0:M), PN(0:L,0:M)

      INTEGER I, J, LADV, LRHS
C=======================================================================
C  ADD -D(UH)/DX TO CONTINUITY EQUATION:
      DO 10 J=1,MM
      DO 10 I=1,LM
       RH(I,J)=-0.5*((U(I+1,J,LADV)+U(I+1,J+1,LADV))
     1          *(H(I+1,J,LRHS)+H(I,J,LRHS))  /(PN(I+1,J)+PN(I,J))
     2              -(U(I,J,LADV)  +U(I,J+1,LADV))
     3          *(H(I,J,LRHS)  +H(I-1,J,LRHS))/(PN(I,J)  +PN(I-1,J)))/DX
C  ADD -D(VH)/DY:
       RH(I,J)=RH(I,J)
     1         -0.5*((V(I,J+1,LADV)+V(I+1,J+1,LADV))
     2          *(H(I,J+1,LRHS)+H(I,J,LRHS))  /(PM(I,J+1)+PM(I,J))
     3              -(V(I,J,LADV)  +V(I+1,J,LADV))
     4          *(H(I,J,LRHS)  +H(I,J-1,LRHS))/(PM(I,J)  +PM(I,J-1)))/DY
       RH(I,J)=PM(I,J)*PN(I,J)*RH(I,J)
   10 CONTINUE
      RETURN
      END
