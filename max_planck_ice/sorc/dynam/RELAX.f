      SUBROUTINE RELAX(LR,LN, U, V, BU, BV, AMAS, FX, FY, H, ETA, ZETA, 
     1                  ASY, IIC)
C=======================================================================
C  PROGRAMMED BY:
C     WILLIAM D. HIBLER        CRREL, HANOVER, N.H.                 1979
C     (SUBROUTINE RELAX IN HIBLER 80)
C  MODIFIED BY:
C     W.BRECHNER OWENS         MPI, HAMBURG                         1987
C  LAST MODIFIED: 16 August 1994.
C  LAST MODIFIED: 21 February 1997
C  PURPOSE:
C     -CALCULATES THE ICE VELOCITIES
C     -SOLVES THAT PART OF THE MOMENTUM EQUATION THAT DEPENDS ON THE
C       VELOCITIES AT THE CURRENT TIME STEP
C  METHOD:
C     -SUCCESIVE OVERRELAXATION WITH CHEBYSHEV ACCELERATION (HOCKNEY
C       AND JESSHOPE (1981), PGS.334-341
C     -IN THIS CASE WE ARE UPDATING A CHECKERBOARD, ALTERNATING COLORS,
C       IE FIRST RED, THEN BLACK
C     -THE VELOCITY ARRAYS WILL BE USED AS FOLLOWS:
C       U OR V(I,J,LN)=VELOCITIES AT THE INTERMEDIATE OR NEW TIME STEP
C       U OR V(I,J,LR)=VELOCITIES AT THE OLD OR INTERMEDIATE TIME STEP
C     -WE ARE SOLVING THE EQUATIONS:
C       BU*U-ASY*V=RU+FX
C       BV*V+ASY*U=RV+FY
C       WHICH AFTER TAKING THE RECIPROCAL OF THE SYMMETRIC COEFFICIENTS
C       (BU,BV) AT THE END OF SUBROUTINE RELCON AND SOLVING FOR U AND V
C       YIELDS:
C       U-(ASY*BU)*V=(RU+FX)*BU
C       V+(ASY*BU)*U=(RV+FY)*BV
C  INTERFACE:
C     -LR: RUNNING INDEX FOR OLD OR INTERMEDIATE TIME STEP
C     -LN: RUNNING INDEX FOR INTERMEDIATE OR NEW TIME STEP
C  EXTERNALS:
C     -DDX:  CALCULATES X-DERIVATIVES FOR THE INTERNAL ICE STRESS
C     -DDY:  CALCULATES Y-DERIVATIVES FOR THE INTERNAL ICE STRESS
C     -MADV: CALCULATES THE ADVECTION TERMS
C     -BCSV: SETS CYCLIC BOUNDARY CONDITIONS FOR VELOCITIES
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "rheology.inc"
C=======================================================================
      REAL H(0:L,0:M,2)
      INTEGER LR, LN
C=======================================================================
      INTEGER IIC

      REAL U(L,M,3), V(L,M,3)

      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      REAL VM, HM, OM, FLM

C COORD needed for call to ddx, ddy
      COMMON/COORD/PM(0:L,0:M), PN(0:L,0:M), DNDX(L,M), DMDY(L,M)
      REAL PM, PN, DNDX, DMDY

      REAL AMAS(L,M), BU(L,M), BV(L,M), FX(L,M), FY(L,M), ASY(L,M)
      REAL ETA(L,M), ZETA(L,M)
      REAL TMP(L,M), RU(L,M), RV(L,M), DEN(L,M)
C=======================================================================
C     -TMP:  TEMPORARY ARRAY
C     -RU:   X-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -RV:   V-COMP.OF TERMS THAT DO NOT DEPEND ON VAL.AT LOCAL GRID PT.
C     -DEN:  DENOMINATOR OF THE SOLUTION EQUATION
C     -AMAS: ICE MASS
C     -BU:   RECIPR.OF THE X-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -BV:   RECIPR.OF THE Y-COMP.OF THE SYMMETRIC COEFF. OF THE MOM.EQ.
C     -FX:   X-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -FY:   Y-COMP. OF TERMS THAT DO NOT DEPEND ON CURRENT TIME STEP
C     -ASY:  ASYMMETRIC COEFFICIENTS OF THE MOMENTUM EQUATION
C     -ZETA: BULK VISCOSITY
C     -ETA:  SHEAR VISCOSITY
C=======================================================================
      INTEGER I, J
      INTEGER MRELAX, K, IB
      REAL WTA, VERR
C-----------------------------------------------------------------------
C  MAKE THE FIRST GUESS FOR THE VELOCITIES
C-----------------------------------------------------------------------
      DO 10 J=2,MM
      DO 10 I=2,LM
       U(I,J,LN)=U(I,J,LR)
       V(I,J,LN)=V(I,J,LR)
   10 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE DENOMINATOR OF THE SOLUTION EQUATION
C-----------------------------------------------------------------------
      DO 20 J=2,MM
      DO 20 I=2,LM
        DEN(I,J)=1./(1.+(VM(I,J)*ASY(I,J))**2*BU(I,J)*BV(I,J))
   20 CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION AT 0
C-----------------------------------------------------------------------
      MRELAX=0
C-----------------------------------------------------------------------
C  START OF RELAXATION LOOP
C-----------------------------------------------------------------------
  120 CONTINUE
C  STOP IF MAXIMUM NUMBER OF ITERATIONS IS EXCEEDED:
      IF (MRELAX.GT.MMAX) GOTO 400
C-----------------------------------------------------------------------
C  DETERMINE THE RELAXATION FACTOR FOR THE RESIDUAL
C-----------------------------------------------------------------------
      IF ((MRELAX.GT. 0.667*MMAX).OR.(MRELAX.EQ.0))THEN
       WTA=1.0
      ELSE
       WTA=WT
      END IF
C-----------------------------------------------------------------------
C  DO RELAXATION ALTERNATING COLORS FOR CHECKERBOARD PATTERN
C-----------------------------------------------------------------------
      DO 170 K=0,1
C-----------------------------------------------------------------------
C  FIRST DIFFERENTIATE THE BULK VISCOSITIES
C-----------------------------------------------------------------------
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U, V, ZETA, LN, K, TMP, PN, PM)
       DO 125 J=2,MM
        IB=MOD(J+K,2)+2
        DO 125 I=IB,LM,2
        RU(I,J)=TMP(I,J)+TMP(I+1,J)
  125  CONTINUE
C  ENTER D{ZETA*[D(U)/DX+D(V)/DY]}/DY TO V EQUATION:
       CALL DDY(V, U, ZETA, LN, K, TMP, PN, PM)
       DO 130 J=2,MM
        IB=MOD(J+K,2)+2
        DO 130 I=IB,LM,2
        RV(I,J)=TMP(I,J)+TMP(I+1,J)
  130  CONTINUE
C-----------------------------------------------------------------------
C  NEXT DIFFERENTIATE THE SHEAR VISCOSITIES
C-----------------------------------------------------------------------
C  ADD D{ETA*[D(U)/DX-D(V)/DY]}/DX TO U EQUATION:
       CALL DDX(U, V, ETA, LN, K, TMP, PN, PM)
       DO 135 J=2,MM
        IB=MOD(J+K,2)+2
        DO 135 I=IB,LM,2
        RU(I,J)=RU(I,J)+TMP(I,J)-TMP(I+1,J)
  135  CONTINUE
C  ADD D{ETA*[D(V)/DY-D(U)/DX]}/DY TO V EQUATION:
       CALL DDY(V, U, ETA, LN, K, TMP, PN, PM)
       DO 140 J=2,MM
        IB=MOD(J+K,2)+2
        DO 140 I=IB,LM,2
        RV(I,J)=RV(I,J)+TMP(I,J)-TMP(I+1,J)
  140  CONTINUE
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DY TO U EQUATION:
       CALL DDY(U, V, ETA, LN, K, TMP, PN, PM)
       DO 145 J=2,MM
        IB=MOD(J+K,2)+2
        DO 145 I=IB,LM,2
        RU(I,J)=RU(I,J)+TMP(I,J)+TMP(I+1,J)
  145  CONTINUE
C  ADD D{ETA*[D(U)/DY+D(V)/DX]}/DX TO V EQUATION:
       CALL DDX(V, U, ETA, LN, K, TMP, PN, PM)
       DO 150 J=2,MM
        IB=MOD(J+K,2)+2
        DO 150 I=IB,LM,2
        RV(I,J)=RV(I,J)+TMP(I,J)+TMP(I+1,J)
  150  CONTINUE
C-----------------------------------------------------------------------
C  ADD IN THE ADVECTION TERMS
C-----------------------------------------------------------------------
       CALL MADV(LR,LN,K, RU, RV, AMAS, U, V, PM, PN)
C-----------------------------------------------------------------------
C  CALCULATE CHANGES IN VELOCITIES
C-----------------------------------------------------------------------
       DO 155 J=2,MM
        IB=MOD(J+K,2)+2
        DO 155 I=IB,LM,2
C  ADD THE OTHER PARTS OF THE MOM.EQ. AND DIVIDE BY THE SYMMETRIC COEFF:
         TMP(I,J)  =VM(I,J)*(RU(I,J)+FX(I,J))*BU(I,J)
         TMP(I+1,J)=VM(I,J)*(RV(I,J)+FY(I,J))*BV(I,J)
C  SOLVE FOR THE NEXT GUESS OF THE VELOCITIES:
         RU(I,J)=(TMP(I,J)+ASY(I,J)*BU(I,J)*TMP(I+1,J))*DEN(I,J)
         RV(I,J)=(TMP(I+1,J)-ASY(I,J)*BV(I,J)*TMP(I,J))*DEN(I,J)
C  DETERMINE THE RESIDUAL
         RU(I,J)=WTA*(RU(I,J)-U(I,J,LN))
         RV(I,J)=WTA*(RV(I,J)-V(I,J,LN))
  155  CONTINUE
C-----------------------------------------------------------------------
C  UPDATE THE VELOCITIES
C-----------------------------------------------------------------------
       DO 160 J=2,MM
        IB=MOD(J+K,2)+2
        DO 160 I=IB,LM,2
         U(I,J,LN)=U(I,J,LN)+RU(I,J)
         V(I,J,LN)=V(I,J,LN)+RV(I,J)
  160  CONTINUE
C-----------------------------------------------------------------------
C  SET CYCLIC BOUNDARY CONDITIONS
C-----------------------------------------------------------------------
       CALL BCSV(U,V,LN, VM)
  170 CONTINUE
C-----------------------------------------------------------------------
C  CHECK TO SEE IF CHANGES IN VELOCITIES ARE SMALL ENOUGH FOR US TO STOP
C-----------------------------------------------------------------------
      MRELAX=MRELAX+1
      VERR=0.0
      DO 180 J=2,MM
      DO 180 I=2,LM
       VERR=AMAX1(ABS(RU(I,J)),VERR)
       VERR=AMAX1(ABS(RV(I,J)),VERR)
  180 CONTINUE

      IF (VERR.GT.VRMAX) GOTO 120
      PRINT *,'Relax took ',MRELAX,' iterations to converge '
C**WE HAVE SUCCEEDED IN FINDING A NEW SOLUTION:
      RETURN

C**WE HAVE NOT FOUND A SOLUTION AFTER MMAX ITERATIONS:
  400 CONTINUE

      WRITE (*,2000) IIC,MRELAX
 2000 FORMAT ('At time step ',I3,' after ',I3,' iterations, no solution
     1 found.  Zeroing velocity field')

C     If no solution is found, continue with the old velocity field.
C     Robert Grumbine 17 July 1994.
C     No, If no solution is found, zero the velocity field, due to
C       the time stepping procedure.  24 February 1997
C     If point converged, use velocity, else zero.  8 August 2002
      DO j = 1, M
      DO i = 1, L
        IF (ABS(RU(i,j)) .GT. 10.*VRMAX .OR. 
     1      ABS(RV(i,j)) .GT. 10.*VRMAX) THEN
          PRINT *,'point ',i,j,' nonconvergent ',
CD     1          RU(i,j),RV(i,j),SQRT(RU(i,j)*RU(i,j)+RV(i,j)*RV(i,j) )    
     1          SQRT(RU(i,j)*RU(i,j)+RV(i,j)*RV(i,j) )    
        ENDIF
        IF (ABS(RU(i,j)) .GT. VRMAX .OR. 
     1      ABS(RV(i,j)) .GT. VRMAX) THEN
            U(i,j,LN) = 0.0
            V(i,j,LN) = 0.0
        ENDIF
      ENDDO
      ENDDO

      RETURN
      END
