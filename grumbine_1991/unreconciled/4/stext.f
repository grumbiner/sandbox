      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H, LNH,
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF, STREF,
     2                 GAMMA, ASH, ASV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT
C       TIME STEP.
C     MODIFIED (IN ARGUMENT NAMES) TO BE USED FOR THE TEMP. EXTRAP
C       AS WELL.  3-30-88, BG.
C     FORCING ARRAYS ENLARGED TO 60X60 BY 5-26-88.
C     DEL(US) COMPUTED AS D(US)/DX + D(VS)/DY 3-30-88.
C     EDDY DIFFUSION (LESS TOPOGRAPHIC TERMS) RE-ADDED 3-30-88
C     BC 3-29 TO 5-25-88:
C       Y = 0, YMAX D /DY = 0
C       X = 0  D /DX = GAMMA*(S -S0)
C       X = XMAX S = SREF (BOTH SD, SS)
C     CHANGE BC ON 5-26-88 AT X = 0,XMAX
C       X = 0, D /DX = 0
C       X = XMAX  D / DX = GAMMA*(A -A0)
C     TOPOGRAPHY ADDED 1-18-89
C     FORCING ARRAYS CUT BACK TO 40X40 1-24-89.
      INTEGER NX, NY, TSTEP
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL QS(NX, NY), QD(NX, NY), SS(NX, NY), SD(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)
      REAL DELX, DELY, DELT, DREF, SREF, STREF, GAMMA, ASH, ASV
      INTEGER I, J
      REAL FSS(40, 40), FSD(40, 40)
      REAL DX2, DY2
      REAL PSI
      INTEGER K, L
      PSI(K,L) = 0.0
CD    PSI(K,L) = SIGN(1., WE(K,L)
CD   1  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CD   2  +  UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   3  +  VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2
CD   4  -  UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
CD   5  -  VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2          )
      DX2 = 2.*DELX
      DY2 = 2.*DELY
C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 1000 J = 2, NY-1
        DO 1010 I = 2, NX-1
          FSS(I,J) =
C          ORIGINALLY USED DEL*(US), TRY S*(DELU)+ U*GRAD(S) INSTEAD.
C          RETURN TO THE DEL (US) METHOD. 3-30-88
C          VERSION FOR BAROTROPIC FLOW, WITH NO TOPOGRAPHY 8-15-88.
C              DEL(U) FIGURED 'ANALYTICALLY', CENTERED DIFFERENCE,
C              ISO HALINE/THERMAL MIXED LAYER 8-15-88.
     1     - UT(I,J)*(SS(I+1,J)-SS(I-1,J))/DX2
     2     - VT(I,J)*(SS(I,J+1)-SS(I,J-1))/DY2
     3     - UC(I,J)*(SD(I+1,J)-SD(I-1,J))/DX2
     4     - VC(I,J)*(SD(I,J+1)-SD(I,J-1))/DY2
     5     - SD(I,J)*(  (UC(I+1,J)-UC(I-1,J))/DX2
     6                + (VC(I,J+1)-VC(I,J-1))/DY2   )
     7     + QS(I,J)/H(I,J)
     8     - WE(I,J)*SD(I,J)/H(I,J)
C          ADD DIFFUSIVE TERMS 3-30-88
     9     + ASH*( (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))/(DELX*DELX)
     1            +(SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))/(DELY*DELY) )
C          ADD TOPOGRAPHIC TERMS 1-18-89
     1     - (SD(I,J)/H(I,J))
     2          *2.*( UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     3               +VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
     4     + (SD(I,J)/H(I,J))
     5          *( UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     6            +VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
          FSD(I,J) =
     1     - UT(I,J)*(SD(I+1,J)-SD(I-1,J))/DX2
     2     - VT(I,J)*(SD(I,J+1)-SD(I,J-1))/DY2
     3     - UC(I,J)*(SS(I+1,J)-SS(I-1,J))/DX2
     4     - VC(I,J)*(SS(I,J+1)-SS(I,J-1))/DY2
     5     - SS(I,J)*(UC(I+1,J)-UC(I-1,J))/DX2
     6     - SS(I,J)*(VC(I,J+1)-VC(I,J-1))/DY2
     7     + QD(I,J) / H(I,J)
     8     - 8.*ASV*SD(I,J)/H(I,J)**2
     9     + ASH*( (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))/(DELX*DELX)
     1            +(SD(I,J+1)-2.*SD(I,J)+SD(I,J-1))/(DELY*DELY) )
     2     - WE(I,J)*SS(I,J)/H(I,J)
CU   3     - (2.*PSI(I,J)*SD(I,J)/H(I,J))* ( WE(I,J)
CU   4  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CU   5  + (UC(I,J)-UT(I,J))*(H(I+1,J)-H(I-1,J))/DX2
CU   6  + (VC(I,J)-VT(I,J))*(H(I,J+1)-H(I,J-1))/DY2     )
          FSD(I,J) = FSD(I,J)
C          ADD TOPOGRAPHY SEPARATELY TO AVOID PROBS WITH CONTINUATION
C             LINE LIMIT
     1  + UT(I,J)*(SS(I,J)-2.*SD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     2  + VT(I,J)*(SS(I,J)-2.*SD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
     3  - UC(I,J)*(2.*SS(I,J)-2.*SD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     4  - VC(I,J)*(2.*SS(I,J)-2.*SD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
 1010   CONTINUE
 1000 CONTINUE
C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          SS(I,J) = SS(I,J) + DELT*FSS(I,J)
          SD(I,J) = SD(I,J) + DELT*FSD(I,J)
 3010   CONTINUE
 3000 CONTINUE
C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,NY) = S(X,NY-1)
C                     X = XMAX  , FIXED VALUE S(XMAX,Y) = DREF, SREF
C                     X = 0     , NO FLUX?? S(1,Y) = S(2,Y)
C         X=0 BC CHANGED TO A ROBIN CONDITION: DS/DX = GAMMA(S-STREF)
C           ON 3-29-88.  GAMMA IS SET AS INVERSE OF DISTANCE TRAVELED
C           BY CURRENT IN THE NON-DIMENSIONALIZATION TIME (1/FRO).
C         X=0 BC CHANGED TO NO FLUX 5-26-88
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.
C     BC:
      DO 4000 I = 1, NX-1
        SS(I,1)  = SS(I,2)
        SD(I,1)  = SD(I,2)
        SS(I,NY) = SS(I,NY-1)
        SD(I,NY) = SD(I,NY-1)
 4000 CONTINUE
      DO 4010 J = 1, NY
        SS(NX,J) = (SS(NX-1,J)+DELX*GAMMA*SREF)/(1.+GAMMA*DELX)
        SD(NX,J) = (SD(NX-1,J)+DELX*GAMMA*DREF)/(1.+GAMMA*DELX)
        SS(1,J)  = SS(2,J)
        SD(1,J)  = SD(2,J)
 4010 CONTINUE
      RETURN
      END
