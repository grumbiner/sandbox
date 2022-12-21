      SUBROUTINE MADV(LR, LN, K, RU, RV, AMAS, U, V, PM, PN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -CALCULATION OF HORIZONTAL ADVECTION TERMS
C  METHOD:
C     -STRAIGHTFORWARD WITH CENTERED DIFFERENCES
C     -NOTE: WE CAN NOT USE THE FLUX FORM AS IS THE NORMAL PRACTICE
C       SINCE THE FLOW CAN BE COMPRESSIBLE
C  INTERFACE:
C     -LR:  RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C     -LN:  RUNNING INDEX FOR INTERMEDIATE OR NEW TIME STEP
C     -K:   INDEX FOR ALTERNATING PATTERN
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LR, LN, K
CD      COMMON/COORD/PM(0:L,0:M),PN(0:L,0:M),DNDX(L,M),DMDY(L,M)
CD      REAL PM, PN, DNDX, DMDY
      REAL PM(0:L,0:M), PN(0:L,0:M)
      REAL U(L, M, 3), V(L, M, 3)
      REAL RU(L,M), RV(L,M), AMAS(L,M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -RU:   X-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -RV:   V-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -AMAS: ICE MASS
C=======================================================================
      INTEGER I, J, IB
      REAL TMP(L,M)
C=======================================================================
      DO 10 J=2,MM
       IB=MOD(J+K,2)+2
       DO 10 I=IB,LM,2
C  ADD IN -M*U*D(U)/DX-M*V*D(U)/DY TO U EQUATION:
        RU(I,J)=RU(I,J)-AMAS(I,J)*U(I,J,LR)
     1                  *(U(I+1,J,LN)/(PN(I,J)  +PN(I,J-1))
     2                   -U(I-1,J,LN)/(PN(I-1,J)+PN(I-1,J-1)))/DX
     3                 -AMAS(I,J)*V(I,J,LR)
     4                  *(U(I,J+1,LN)/(PM(I,J)  +PM(I-1,J))
     5                   -U(I,J-1,LN)/(PM(I,J-1)+PM(I-1,J-1)))/DY
C  ADD IN -M*U*D(V)/DX-M*V*D(V)/DY TO V EQUATION:
        RV(I,J)=RV(I,J)-AMAS(I,J)*U(I,J,LR)
     1                  *(V(I+1,J,LN)/(PN(I,J)  +PN(I,J-1))
     2                   -V(I-1,J,LN)/(PN(I-1,J)+PN(I-1,J-1)))/DX
     3                 -AMAS(I,J)*V(I,J,LR)
     4                  *(V(I,J+1,LN)/(PM(I,J)  +PM(I-1,J))
     5                   -V(I,J-1,LN)/(PM(I,J-1)+PM(I-1,J-1)))/DY
C  ADJUST FOR COORDINATE TRANSFORMATION:
        TMP(I,J)=0.25*(PM(I,J)  *PN(I,J)  +PM(I,J-1)  *PN(I,J-1)
     1                +PM(I-1,J)*PN(I-1,J)+PM(I-1,J-1)*PN(I-1,J-1))
        RU(I,J)=RU(I,J)*TMP(I,J)
        RV(I,J)=RV(I,J)*TMP(I,J)
   10 CONTINUE
      RETURN
      END
