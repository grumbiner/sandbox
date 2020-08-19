      SUBROUTINE BCOEF(LRHS, BU, BV, ZETA, ETA, AMAS, U, V)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS              MPI, HAMBURG                    1987
C  PURPOSE:
C     -CALCULATION OF THE SYMMETRIC COEFFICIENT CONTRIBUTION TO THE IN-
C       TERNAL ICE STRESS
C     -CALCULATION OF COORDINATE TRANSFORMATION CONTRIBUTION TO THE
C       ADVECTION TERMS
C  METHOD:
C     -SOLVES THOSE PARTS OF THE FINITE DIFFERENCE APPROXIMATION OF
C       EQS.5 AND 6 IN HIBLER (79) THAT DO NOT DEPEND ON THE VELOCITIES,
C       I.E. DIFFERENTIATES PARTIALLY THE VISCOSITIES
C  INTERFACE:
C     -LRHS: RUNNING INDEX OF OLD OR INTERMEDIATE TIME STEP
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      REAL U(L, M, 3), V(L, M, 3)
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY
      REAL AMAS(L,M), BU(L,M), BV(L,M), ZETA(L,M), ETA(L,M)
C=======================================================================
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
C=======================================================================
      DO 10 J=2,MM
      DO 10 I=2,LM
C-----------------------------------------------------------------------
C  FIRST DIFFERENTIATE THE BULK VISCOSITIES
C-----------------------------------------------------------------------
C  CONTRIBUTION TO -D[ZETA*D(U)/DX]/DX FOR U EQUATION:
       BU(I,J)=SX2*((ZETA(I,J)  +ZETA(I,J-1))
     1            /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2             +(ZETA(I-1,J)+ZETA(I-1,J-1))
     3            /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C  CONTRIBUTION TO -D[ZETA*D(V)/DY]/DY FOR V EQUATION:
       BV(I,J)=SY2*((ZETA(I,J)  +ZETA(I-1,J))
     1            /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2             +(ZETA(I,J-1)+ZETA(I-1,J-1))
     3            /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD CONTRIBUTION TO -D[ETA*D(U)/DX]/DX TO U EQUATION:
       BU(I,J)=BU(I,J)+SX2*((ETA(I,J)  +ETA(I,J-1))
     1                /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2                     +(ETA(I-1,J)+ETA(I-1,J-1))
     3                /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(V)/DY]/DY TO V EQUATION:
       BV(I,J)=BV(I,J)+SY2*((ETA(I,J)  +ETA(I-1,J))
     1                /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2                     +(ETA(I,J-1)+ETA(I-1,J-1))
     3                /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(U)/DY]/DY TO U EQUATION:
       BU(I,J)=BU(I,J)+SY2*((ETA(I,J)  +ETA(I-1,J))
     1                /(PM(I,J)  +PM(I-1,J))  *(PN(I,J)  +PN(I-1,J))
     2                     +(ETA(I,J-1)+ETA(I-1,J-1))
     3                /(PM(I,J-1)+PM(I-1,J-1))*(PN(I,J-1)+PN(I-1,J-1)))
C  ADD CONTRIBUTION TO -D[ETA*D(V)/DX]/DX TO V EQUATION:
       BV(I,J)=BV(I,J)+SX2*((ETA(I,J)  +ETA(I,J-1))
     1                /(PN(I,J)  +PN(I,J-1))  *(PM(I,J)  +PM(I,J-1))
     2                     +(ETA(I-1,J)+ETA(I-1,J-1))
     3                /(PN(I-1,J)+PN(I-1,J-1))*(PM(I-1,J)+PM(I-1,J-1)))
C-----------------------------------------------------------------------
C  ADD IN CONTRIBUTION TO ADVECTION DUE TO COORDINATE TRANSFORMATION
C-----------------------------------------------------------------------
C  ADD COEFFICIENT FOR M*U*D(U)/DX:
       BU(I,J)=BU(I,J)+AMAS(I,J)*U(I,J,LRHS)
     1        *(1./(PN(I,J)+PN(I,J-1))-1./(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD COEFFICIENT FOR M*V*D(U)/DY:
       BU(I,J)=BU(I,J)+AMAS(I,J)*V(I,J,LRHS)
     1        *(1./(PM(I,J)+PM(I-1,J))-1./(PM(I,J-1)+PN(I-1,J-1)))/DY
C  ADD COEFFICIENT FOR M*U*D(V)/DX:
       BV(I,J)=BV(I,J)+AMAS(I,J)*U(I,J,LRHS)
     1        *(1./(PN(I,J)+PN(I,J-1))-1./(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD COEFFICIENT FOR M*V*D(V)/DY:
       BV(I,J)=BV(I,J)+AMAS(I,J)*V(I,J,LRHS)
     1        *(1./(PM(I,J)+PM(I-1,J))-1./(PM(I,J-1)+PN(I-1,J-1)))/DY
   10 CONTINUE
      RETURN
      END
