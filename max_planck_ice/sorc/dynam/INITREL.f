CD      SUBROUTINE INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, DT,
      SUBROUTINE INITREL(U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, ASY,
     1    SURTYP, SURFWIN, IIC)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     Robert Grumbine          NCEP, Camp Springs, MD               1993
C     Robert Grumbine          NCEP Camp Springs, MD         19 Feb 1997
C  LAST MODIFIED: 6 January 1993
C  LAST MODIFIED: 19 February 1997
C    -- Removed common blocks  
C  PURPOSE:
C     -PERFORMES INITIAL RELAXATION TO BALANCE INITIAL FIELDS
C  METHOD:
C     -SOLVES THE MOMENTUM EQUATION NEGLECTING THE INERTIAL TERMS
C  EXTERNALS:
C     -OUTBCS: SETS VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW POINTS
C     -RELCON: CALCULATES DIAGNOSTIC TERMS OF MOMENTUM BALANCE
C     -RELAX:  SOLVES MOMENTUM BALANCE BY OVERRELAXATION
C=======================================================================
      INCLUDE "icegrid.inc"
C=======================================================================
      REAL U(L, M, 3), V(L, M, 3)
      REAL H(0:L, 0:M, 2)
      REAL AMAS(L, M), BU(L, M), BV(L, M), FX(L, M), FY(L, M)
      REAL ZETA(L, M), ETA(L, M), ASY(L,M)
      REAL SURTYP, SURFWIN
      INTEGER IIC
C=======================================================================
C     -TRK:  DUMMY REGISTERS
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC TERMS OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC TERMS OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC TERMS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  SET UP INITIAL VISCOSITIES
C-----------------------------------------------------------------------
      DO 10 J=1,MM
      DO 10 I=1,L
       ZETA(I,J)=H(I,J,1)*1.E+11
       ETA(I,J)=ZETA(I,J)/4.0
   10 CONTINUE
C-----------------------------------------------------------------------
C  SET VISCOSITIES AND ICE STRENGTH TO 0 AT OUTFLOW GRID CELLS
C-----------------------------------------------------------------------
      CALL OUTBCS(ETA, ZETA)
C-----------------------------------------------------------------------
C  SET UP DIAGNOSTIC PART OF THE MOMENTUM EQUATION
C-----------------------------------------------------------------------
      CALL RELCON(1,1, U, V, BU, BV, AMAS, FX, FY, H, 
     1              ETA, ZETA, ASY, SURTYP, SURFWIN)
C-----------------------------------------------------------------------
C  DELETE THE ACCELERATION TERMS
C-----------------------------------------------------------------------
      DO 20 J=2,MM
      DO 20 I=1,L
       IF (BU(I,J) .NE. 0)
     1       BU(I,J)=1./(1./BU(I,J)-AMAS(I,J)/DT)
       IF (BV(I,J) .NE. 0)
     1       BV(I,J)=1./(1./BV(I,J)-AMAS(I,J)/DT)
       FX(I,J)=FX(I,J)-AMAS(I,J)*U(I,J,1)/DT
       FY(I,J)=FY(I,J)-AMAS(I,J)*V(I,J,1)/DT
       AMAS(I,J)=0.0
   20 CONTINUE
C-----------------------------------------------------------------------
C  SOLVE THE MOMENTUM EQUATION BY OVERRELAXATION (ONLY ONCE)
C-----------------------------------------------------------------------
      CALL RELAX(1,2, U, V, BU, BV, AMAS, FX, FY, H, 
     1             ETA, ZETA, ASY, IIC)
      DO 30 J=1,M
      DO 30 I=1,L
       U(I,J,1)=U(I,J,2)
       V(I,J,1)=V(I,J,2)
   30 CONTINUE

      RETURN
      END
