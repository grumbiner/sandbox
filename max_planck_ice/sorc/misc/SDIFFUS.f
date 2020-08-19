      SUBROUTINE SDIFFUS(RH, H, LRHS)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE DIFFUS IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C     A.STOESSEL               MPI, HAMBURG                         1991
C     Robert Grumbine          NCEP Camp Springs, MD                1993
C  PURPOSE:
C     -CALCULATION OF DIFFUSION OF A SCALAR VARIABLE FOR THE CONTINUITY
C       EQUATIONS (EQ.13 AND 14 IN HIBLER 79 AND EQ.8 IN OWENS AND
C       LEMKE 90)
C  METHOD:
C     -FORWARD (EULER) SCHEME FOR TIME INTEGRATION
C     -EMPLOYMENT OF LAPLACIAN AND BIHARMONIC DIFFUSION TERMS (APP.A IN
C       HIBLER 79)
C     -SUBROUTINE BCSFLX WILL BE CALLED WHICH SETS THE DIFFUSIVE FLUXES
C       EQUAL TO ZERO AT THE BOUNDARIES TO INSURE THAT THE TOTAL ICE
C       MASS DOES NOT CHANGE
C  INTERFACE:
C     -RH:   CHANGE OF SCALAR VARIABLE
C     -H:    SCALAR VARIABLE
C     -LRHS: RUNNING INDEX FOR OLD TIME STEP
C  EXTERNALS:
C     -BCSFLX: SETS FLUXES NORMAL TO BOUNDARIES TO 0
C  LAST MODIFIED: 5 January 1993
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      INTEGER LRHS
      COMMON/IPARM/H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      REAL H0, HNU, HNU2, ARMIN, ARMAX, HMIN
      COMMON/DRV/DXSQ, DYSQ, SX2, SY2, SXY
      REAL DXSQ, DYSQ, SX2, SY2, SXY
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DUMMY(2*L*M)
      REAL PM, PN, DUMMY

      REAL TMP(0:L,0:M), DFX(0:L,0:M), DFY(0:L,0:M), RH(0:L,0:M),
     1     H(0:L,0:M,2)
C=======================================================================
C     -TMP: TEMPORARY ARRAY
C     -DFX: FLUX IN X-DIRECTION
C     -DFY: FLUX IN Y-DIRECTION
C     -RH:  CONTRIBUTION TO CONTINUITY EQUATION
C     -H:   SCALAR VARIABLE
C=======================================================================
      INTEGER I, J
C-----------------------------------------------------------------------
C  FIRST CALCULATE THE LAPLACIAN (HARMONIC) DIFFUSION TERMS
C-----------------------------------------------------------------------
C  CALCULATE D(H)/DX:
      DO 110 J=1,M
      DO 110 I=1,L
       DFX(I,J)=PM(I,J)*H(I,J,LRHS)-PM(I-1,J)*H(I-1,J,LRHS)
C  CALCULATE D(H)/DY:
       DFY(I,J)=PN(I,J)*H(I,J,LRHS)-PN(I,J-1)*H(I,J-1,LRHS)
  110 CONTINUE
C  SET THE BOUNDARY FLUXES EQUAL TO ZERO:
      CALL BCSFLX(DFX,DFY)
      DO 130 J=1,MM
      DO 130 I=1,LM
C  ADD D[HNU*D(H)/DX]/DX AND D[HNU*D(H)/DY]/DY TO CONTINUITY EQUATION:
       TMP(I,J)=((DFX(I+1,J)/PN(I+1,J)-DFX(I,J)/PN(I,J))/DXSQ
     1          +(DFY(I,J+1)/PM(I,J+1)-DFY(I,J)/PM(I,J))/DYSQ)
     2         *PM(I,J)*PN(I,J)
       RH(I,J)=RH(I,J)+HNU*TMP(I,J)/(PM(I,J)*PN(I,J))
  130 CONTINUE
C-----------------------------------------------------------------------
C  NOW CALCULATE THE BIHARMONIC DIFFUSION TERM
C-----------------------------------------------------------------------
      DO 210 J=1,M
      DO 210 I=1,L
       DFX(I,J)=PM(I,J)*TMP(I,J)-PM(I-1,J)*TMP(I-1,J)
       DFY(I,J)=PN(I,J)*TMP(I,J)-PN(I,J-1)*TMP(I,J-1)
  210 CONTINUE
      CALL BCSFLX(DFX,DFY)
      DO 230 J=1,MM
      DO 230 I=1,LM
       TMP(I,J)=((DFX(I+1,J)/PN(I+1,J)-DFX(I,J)/PN(I,J))/DXSQ
     1          +(DFY(I,J+1)/PM(I,J+1)-DFY(I,J)/PM(I,J))/DYSQ)
     2         *PM(I,J)*PN(I,J)
       RH(I,J)=RH(I,J)-HNU2*TMP(I,J)/(PM(I,J)*PN(I,J))**3
  230 CONTINUE
      RETURN
      END
