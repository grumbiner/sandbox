      SUBROUTINE UTROP(DX, DY, AM, F, BETA, G, RHOREF,
     1                 H, WE, UT, VT, UC, VC, SS, SD, TS, TD,
     2                 NX, NY, CONTOL, ITMAX, L               )
C     COMPUTE THE STEADY BAROTROPIC VELOCITY FIELD.
C     VERSION WITHOUT BOUNDARY LAYERS STARTED 12-20-88.
C     BOUNDARY LAYERS BEGUN 1-11-89.

      INTEGER NX, NY
      REAL DX, DY, AM, F, BETA, G, RHOREF
      REAL H(NX, NY), WE(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL UC(NX, NY), VC(NX, NY)
      REAL SS(NX, NY), SD(NX, NY), TS(NX, NY), TD(NX, NY)
      REAL CONTOL, RHOS1P
      INTEGER ITMAX, L

C     LOCAL VARIABLES - INTERNAL VELOCITY FIELD
      INTEGER NNX, NNY
      PARAMETER (NNX = 20)
      PARAMETER (NNY = 20)
      REAL FORCE(NNX, NNY), UN(NNX, NNY), UNP1(NNX, NNY)
      REAL DHDX(NNX, NNY), DHDY(NNX, NNY), TOPBET(NNX, NNY)
      REAL WPCLIN(NNX, NNY), DEN(NNX, NNY), BUOYVR(NNX, NNY)
      REAL FDIV(NNX, NNY), FVOR(NNX, NNY), Q(NNX, NNY)
      INTEGER I, J, ITERS
      REAL NORM, DBGSCA, DBGARY(NNX, NNY)
      LOGICAL FLATX
C     LOCAL VARIABLES FOR BOUNDARY LAYER SOLN:
      INTEGER K, NB
      REAL LB, BETTMN, UB(NNX, NNY), VB(NNX, NNY)
      REAL PI
      PARAMETER (PI = 3.141592654)
C       FOR LINPACK INVERSION OF BOUNDARY LAYERS
      INTEGER IPVT(NNY)
      REAL COEF(NNY, NNX), RHS(NNY), Z(NNY), RCOND

C***********************************************************----------!!

C     COMPUTE CONSTANTS:

      DO 100 J = 1, NY
        DO 101 I = 2, NX-1
          DHDX(I,J) = (H(I+1,J) - H(I-1,J))/2./DX
  101   CONTINUE
  100 CONTINUE
      DO 102 J = 1, NY
        I = 1
        DHDX(1,J) = (H(I+1,J)-H(I,J))/DX
        I = NX
        DHDX(I,J) = (H(I,J)-H(I-1,J))/DX
  102 CONTINUE
      DO 110 J = 2, NY-1
        DO 111 I = 1, NX
          DHDY(I,J) = (H(I,J+1)-H(I,J-1))/2./DY
  111   CONTINUE
  110 CONTINUE
      DO 112 I = 1, NX
        J = 1
        DHDY(I,J) = (H(I,J+1)-H(I,J))/DY
        J = NY
        DHDY(I,J) = (H(I,J)-H(I,J-1))/DY
  112 CONTINUE

      BETTMN = 0.0
      FLATX  = .TRUE.
      DO 200 J = 1, NY
        DO 210 I = 1, NX
          TOPBET(I,J) = BETA - F*DHDY(I,J)/H(I,J)
          BETTMN      = BETTMN + TOPBET(I,J)
          Q(I,J)      = F*DHDX(I,J)/TOPBET(I,J)
          FLATX       = FLATX .AND. (Q(I,J) .LE. 0.1*H(I,J))
  210   CONTINUE
  200 CONTINUE
      BETTMN = BETTMN/FLOAT(NX*NY)
      LB     = 4.*PI*(AM/BETTMN)**(1./3.)/SQRT(3.)
      NB     = AMAX0(5, INT(LB/DX + 0.5) )

C***********************************************************----------!!
C     COMPUTE ARRAYS WHICH CHANGE BETWEEN CALLS:
      DO 300 J = 1, NY
        DO 301 I = 1, NX
          DEN(I,J) = 3.*RHOS1P(SS(I,J)+SD(I,J),TS(I,J)+TD(I,J),0.)
     1                + RHOS1P(SS(I,J)-SD(I,J),TS(I,J)-TD(I,J),0.)
  301   CONTINUE
  300 CONTINUE
      DO 310 J = 2, NY-1
        DO 311 I = 2, NX-1
          BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
  311   CONTINUE
  310 CONTINUE
      DO 312 J = 2, NY-1
        I = 1
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I,J))/DX )
        I = NX
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J-1))/2./DY
     2                 -DHDY(I,J)*(DEN(I,J)-DEN(I-1,J))/DX )
  312 CONTINUE
      DO 313 I = 2, NX-1
        J = 1
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J+1)-DEN(I,J))/DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
        J = NY
        BUOYVR(I,J) = (G/4./RHOREF)*
     1                ( DHDX(I,J)*(DEN(I,J)-DEN(I,J-1))/DY
     2                 -DHDY(I,J)*(DEN(I+1,J)-DEN(I-1,J))/2./DX )
  313 CONTINUE
      BUOYVR(1,1)   = 0.0
      BUOYVR(1,NY)  = 0.0
      BUOYVR(NX,1)  = 0.0
      BUOYVR(NX,NY) = 0.0

      DO 400 J = 1, NY
        DO 401 I = 1, NX
          WPCLIN(I,J) = -WE(I,J)+UC(I,J)*DHDX(I,J)+VC(I,J)*DHDY(I,J)
          FVOR(I,J)   = -WPCLIN(I,J)*F/H(I,J) - BUOYVR(I,J)
  401   CONTINUE
  400 CONTINUE

      DO 500 J = 2, NY-1
        DO 501 I = 1, NX
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J) * (WPCLIN(I,J+1)-WPCLIN(I,J-1))/2./DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J+1)-DHDY(I,J-1))/2./DY  - DHDY(I,J)**2/H(I,J) )
  501   CONTINUE
  500 CONTINUE
      DO 502 I = 1, NX
        J = NY
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J)* (WPCLIN(I,J)-WPCLIN(I,J-1))/DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J)-DHDY(I,J-1))/DY  - DHDY(I,J)**2/H(I,J) )
        J = 1
          FDIV(I,J) = WPCLIN(I,J)
     1 - F/TOPBET(I,J)* (WPCLIN(I,J+1)-WPCLIN(I,J))/DY
     2 + F*F/TOPBET(I,J)/TOPBET(I,J)*WPCLIN(I,J)/H(I,J)
     3   *( (DHDY(I,J+1)-DHDY(I,J))/DY  - DHDY(I,J)**2/H(I,J) )
  502 CONTINUE

C***********************************************************----------!!

C     NOW ENTER THE XECTION TO EXTRAPOLATE THE INTERIOR SOLUTION:
      IF (.NOT. FLATX) THEN
        DO 1011 J = 2, NY-1
          DO 1012 I = 1, NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J+1)-Q(I,J-1))/2./DY
     2  - Q(I,J)* (UN(I,J+1)-UN(I,J-1))/2./DY
 1012     CONTINUE
 1011   CONTINUE
        DO 1013 I = 1, NY
          J = 1
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J+1)-Q(I,J))/DY
     2  - Q(I,J)* (UN(I,J+1)-UN(I,J))/DY
          J = NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
     1  - UN(I,J) * (Q(I,J)-Q(I,J-1))/DY
     2  - Q(I,J)* (UN(I,J)-UN(I,J-1))/DY
 1013   CONTINUE

       ELSE
        DO 1000 J = 1, NY
          DO 1010 I = 1, NX
            UN(I,J) = UT(I,J)
            FORCE(I,J) = FDIV(I,J)
 1010     CONTINUE
 1000   CONTINUE

      ENDIF

      ITERS = 0
 9999 CONTINUE
        ITERS = ITERS + 1

        DO 2000 J = 1, NY
          UNP1(NX, J) = 0.0
          DO 2010 I = NX-1, 1, -1
            UNP1(I, J) = UNP1(I+1,J) + FORCE(I+1,J) + FORCE(I,J)
 2010     CONTINUE
          DO 2020 I = 1, NX-1
            UNP1(I,J) = UNP1(I,J)*DX/2./H(I,J)
 2020     CONTINUE
 2000   CONTINUE

        IF (.NOT. FLATX .AND. NORM(UNP1, UN, NX*NY, L) .GT. CONTOL
     1                  .AND. ITERS .LT. ITMAX) THEN
          DO 3000 J = 2, NY-1
            DO 3010 I = 1, NX
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J+1)-Q(I,J-1))/2./DY
     2    - Q(I,J)* (UNP1(I,J+1)-UNP1(I,J-1))/2./DY
 3010       CONTINUE
 3000     CONTINUE
          DO 3020 I = 1, NY
            J = 1
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J+1)-Q(I,J))/DY
     2    - Q(I,J)* (UNP1(I,J+1)-UNP1(I,J))/DY
            J = NX
              UN(I,J) = UNP1(I,J)
              FORCE(I,J) = FDIV(I,J)
     1    - UNP1(I,J) * (Q(I,J)-Q(I,J-1))/DY
     2    - Q(I,J)* (UNP1(I,J)-UNP1(I,J-1))/DY
 3020     CONTINUE

          GO TO 9999
        ENDIF

      DO 4000 J = 1, NY
        DO 4010 I= 1, NX
          UT(I,J) = UNP1(I,J)
          VT(I,J) = (F/H(I,J)*DHDX(I,J)*UNP1(I,J) + FVOR(I,J) )
     1             / TOPBET(I,J)
 4010   CONTINUE
 4000 CONTINUE

C***********************************************************----------!!

      RETURN
      END
