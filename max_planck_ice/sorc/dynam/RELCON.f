      SUBROUTINE RELCON(LOLD,LRHS, U, V, BU, BV, AMAS, FX, FY, H, 
     1       ETA, ZETA, ASY, SURTYP, SURFWIN)
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE FORM IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  PURPOSE:
C     -LINEARIZATION OF THE MOMENTUM EQUATION TO BE SOLVED IN SUBROUTINE
C       RELAX
C  METHOD:
C     -THIS ROUTINE CALCULATES THOSE PARTS OF THE MOMENTUM EQUATIONS
C       THAT DO NOT DEPEND ON THE U AND V VALUES OF THE NEW TIME STEP
C     -ADDITIONALLY, THE ROUTINE PROVIDES THE SYMMETRICAL AND ASYM-
C       METRICAL COEFFICIENTS FOR U AND V AT THE NEW TIME STEP
C  INTERFACE:
C     -LOLD: RUNNING INDEX FOR OLD TIME STEP
C     -LRHS: RUNNING INDEX FOR THE OLD OR INTERMEDIATE TIME STEP
C  EXTERNALS:
C     -BCOEF: CALCULATES DERIVATIVES OF VISCOSITIES
C  LAST MODIFIED: 27 January 1993
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================

      INTEGER LOLD, LRHS
      
      REAL SURTYP, SURFWIN

      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      REAL FM, F, COSPHI, SINPHI

      COMMON/IPARM/H0,HNU,HNU2,ARMIN,ARMAX,HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN

      COMMON/DRV/DXSQ,DYSQ,SX2,SY2,SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY

      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL U(L, M, 3), V(L, M, 3)

      REAL H(0:L, 0:M, 2)

      COMMON/FRWND/CDWIN, SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN

      COMMON/FRWAT/SINWAT,COSWAT,UWAT(L,M), VWAT(L,M)
      REAL SINWAT, COSWAT, UWAT, VWAT

      COMMON/PRESS/P(L,M)
      REAL P

      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY

      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      REAL CD, SINBET, COSBET, BETA, TAUX, TAUY

      REAL AMAS(L,M), BU(L,M), BV(L,M), FX(L,M), FY(L,M), ASY(L,M)
      REAL ZETA(L,M), ETA(L,M)

      REAL TMP(L,M)
      INTEGER I, J
      
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -WRK:  DUMMY ARRAYS
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC COEFFICIENTS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
C-----------------------------------------------------------------------
C  CALCULATE THE ICE MASS
C-----------------------------------------------------------------------
      DO 10 J=2,MM
      DO 10 I=1,L
        AMAS(I,J)=RHOICE*0.25*(H(I-1,J,LOLD)  +H(I,J,LOLD)
     1                      +H(I-1,J-1,LOLD)+H(I,J-1,LOLD))
   10 CONTINUE
C-----------------------------------------------------------------------
C  DET.SYM.PARTS OF INT.ICE STRESS AND ADV.CONTR.DUE TO COORD.TRANSFORM.
C-----------------------------------------------------------------------
      CALL BCOEF(LRHS, BU, BV, ZETA, ETA, AMAS, U, V)
C-----------------------------------------------------------------------
C  START LOOP OVER ALL POINTS FOR THE OTHER TERMS
C-----------------------------------------------------------------------
      DO 600 J=2,MM
C     Loop index changed to avoid referencing non-extant
C       element of P. BG.
         DO 600 I = 2,L
C-----------------------------------------------------------------------
C  ADD IN THE ICE STRENGTH
C-----------------------------------------------------------------------
C  ADD D(-P/2)/DX TO U EQUATION:
       FX(I,J)=-0.5*((P(I,J)  +P(I,J-1))  /(PN(I,J)  +PN(I,J-1))
     1              -(P(I-1,J)+P(I-1,J-1))/(PN(I-1,J)+PN(I-1,J-1)))/DX
C  ADD D(-P/2)/DY TO V EQUATION:
       FY(I,J)=-0.5*((P(I,J)  +P(I-1,J))  /(PM(I,J)  +PM(I-1,J))
     2              -(P(I,J-1)+P(I-1,J-1))/(PM(I,J-1)+PM(I-1,J-1)))/DY
C-----------------------------------------------------------------------
C  MULTIPLY THE APPROPRIATE COORDINATE TRANSFORMATION TERMS
C-----------------------------------------------------------------------
       TMP(I,J)=0.25*(PM(I,J)*PN(I,J)    +PM(I-1,J)*PN(I-1,J)
     1               +PM(I,J-1)*PN(I,J-1)+PM(I-1,J-1)*PN(I-1,J-1))
       BU(I,J)=BU(I,J)*TMP(I,J)
       BV(I,J)=BV(I,J)*TMP(I,J)
       FX(I,J)=FX(I,J)*TMP(I,J)
       FY(I,J)=FY(I,J)*TMP(I,J)
C-----------------------------------------------------------------------
C  ADD IN THE LOCAL RATE OF CHANGE TO THE SYMMETRIC TERMS
C-----------------------------------------------------------------------
       BU(I,J)=BU(I,J)+AMAS(I,J)/DT
       BV(I,J)=BV(I,J)+AMAS(I,J)/DT
C-----------------------------------------------------------------------
C  SEPARATE THE ICE/OCEAN STRESS TO THE THREE MAJOR TERMS(BU/V,ASY,FX/Y)
C-----------------------------------------------------------------------
       TMP(I,J)=(UWAT(I,J)-U(I,J,LRHS))**2+(VWAT(I,J)-V(I,J,LRHS))**2
C**AVOID DIVISION BY ZERO WHEN TAKING THE RECIPROCAL OF BU AND BV:
       TMP(I,J)=RHOWAT*CDWAT*SQRT(TMP(I,J))+.000001
       BU(I,J)=BU(I,J)+TMP(I,J)*COSWAT
       BV(I,J)=BV(I,J)+TMP(I,J)*COSWAT
       ASY(I,J)=SINWAT*TMP(I,J)
       FX(I,J)=FX(I,J)+TMP(I,J)*(COSWAT*UWAT(I,J)-SINWAT*VWAT(I,J))
       FY(I,J)=FY(I,J)+TMP(I,J)*(SINWAT*UWAT(I,J)+COSWAT*VWAT(I,J))
C-----------------------------------------------------------------------
C  ADD COORDINATE TRANSFORMATION TERM (TANGENT TERMS)
C-----------------------------------------------------------------------
       TMP(I,J)=0.25*((V(I,J,LRHS)  +V(I,J-1,LRHS)
     1                +V(I-1,J,LRHS)+V(I-1,J-1,LRHS))*DNDX(I,J)
     2               -(U(I,J,LRHS)  +U(I-1,J,LRHS)
     3                +U(I-1,J,LRHS)+U(I-1,J-1,LRHS))*DMDY(I,J))
C-----------------------------------------------------------------------
C  ADD COEFFICIENT FOR CORIOLIS FORCE
C-----------------------------------------------------------------------
       ASY(I,J)=ASY(I,J)+AMAS(I,J)*(F(I,J)+TMP(I,J))
C-----------------------------------------------------------------------
C  ADD CONTRIBUTION FROM THE SEA SURFACE TILT
C-----------------------------------------------------------------------
       FX(I,J)=FX(I,J)-VWAT(I,J)*AMAS(I,J)*F(I,J)
       FY(I,J)=FY(I,J)+UWAT(I,J)*AMAS(I,J)*F(I,J)
C-----------------------------------------------------------------------
C  ADD THE AIR/ICE STRESS
C-----------------------------------------------------------------------
       IF (SURTYP .EQ. 0.) THEN
         CDWIN = CDWIN
        ELSE
         CDWIN=.25*(CD(I,J)+CD(I-1,J)+CD(I,J-1)+CD(I-1,J-1))
       ENDIF
       SINWIN=.25*(SINBET(I,J)+SINBET(I-1,J)+SINBET(I,J-1)
     1            +SINBET(I-1,J-1))*(1.-SURFWIN)     +SINWIN*SURFWIN
       COSWIN=.25*(COSBET(I,J)+COSBET(I-1,J)+COSBET(I,J-1)
     1            +COSBET(I-1,J-1))*(1.-SURFWIN)     +COSWIN*SURFWIN
       TMP(I,J)=RHOAIR*CDWIN*SQRT(UWIN(I,J)**2+VWIN(I,J)**2)
       TAUX(I,J)=TMP(I,J)*(COSWIN*UWIN(I,J)-SINWIN*VWIN(I,J))
       TAUY(I,J)=TMP(I,J)*(SINWIN*UWIN(I,J)+COSWIN*VWIN(I,J))
       FX(I,J)=FX(I,J)+TAUX(I,J)
       FY(I,J)=FY(I,J)+TAUY(I,J)
C-----------------------------------------------------------------------
C  ADD IN CONTRIBUTION FROM LOCAL RATE OF CHANGE FROM OLD TIME STEP
C-----------------------------------------------------------------------
       FX(I,J)=FX(I,J)+AMAS(I,J)*U(I,J,LOLD)/DT
       FY(I,J)=FY(I,J)+AMAS(I,J)*V(I,J,LOLD)/DT
C-----------------------------------------------------------------------
C  GET THE RECIPROCALS OF BU AND BV
C-----------------------------------------------------------------------
       IF (BU(I,J) .NE. 0.) BU(I,J)=1./BU(I,J)
       IF (BV(I,J) .NE. 0.) BV(I,J)=1./BV(I,J)
  600 CONTINUE

      RETURN
      END
