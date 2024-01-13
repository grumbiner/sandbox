      SUBROUTINE CHEXT(UC, VC, UT, VT, WE, CS, CD, QS, QD, H, LNH,
     1                 NX, NY, DELX, DELY, DELT, DREF, SREF, LAMBDA,
     2                 GAMMA, AH, AV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE CHEMICAL TRACER FIELD TO THE NEXT
C       TIME STEP.
C     TAKEN FROM STDIF OF 1-24-89.
C     BC 1-24-89
C       Y = 0, YMAX D /DY = 0
C       X = 0, D /DX = 0
C       X = XMAX  D / DX = GAMMA*(A -A0)
      INTEGER NX, NY, TSTEP
      REAL WE(NX, NY), UC(NX, NY), VC(NX, NY), UT(NX, NY), VT(NX, NY)
      REAL QS(NX, NY), QD(NX, NY), CS(NX, NY), CD(NX, NY)
      REAL H(NX, NY), LNH(NX, NY)
      REAL DELX, DELY, DELT, DREF, SREF, GAMMA, LAMBDA, AH, AV
      INTEGER I, J
      REAL FCS(40, 40), FCD(40, 40)
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
          FCS(I,J) =
C              ISO HALINE/THERMAL MIXED LAYER 8-15-88.
     1     - UT(I,J)*(CS(I+1,J)-CS(I-1,J))/DX2
     2     - VT(I,J)*(CS(I,J+1)-CS(I,J-1))/DY2
     3     - UC(I,J)*(CD(I+1,J)-CD(I-1,J))/DX2
     4     - VC(I,J)*(CD(I,J+1)-CD(I,J-1))/DY2
     5     - CD(I,J)*(  (UC(I+1,J)-UC(I-1,J))/DX2
     6                + (VC(I,J+1)-VC(I,J-1))/DY2   )
     7     + LAMBDA*(QS(I,J)-CS(I,J)-CD(I,J))/H(I,J)
     8     - WE(I,J)*CD(I,J)/H(I,J)
C          ADD DIFFUSIVE TERMS 3-30-88
     9     + AH*( (CS(I+1,J)-2.*CS(I,J)+CS(I-1,J))/(DELX*DELX)
     1           +(CS(I,J+1)-2.*CS(I,J)+CS(I,J-1))/(DELY*DELY) )
C          ADD TOPOGRAPHIC TERMS 1-18-89
     1     - (CD(I,J)/H(I,J))
     2          *2.*( UC(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     3               +VC(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
     4     + (CD(I,J)/H(I,J))
     5          *( UT(I,J)*(H(I+1,J)-H(I-1,J))/DX2
     6            +VT(I,J)*(H(I,J+1)-H(I,J-1))/DY2 )
          FCD(I,J) =
     1     - UT(I,J)*(CD(I+1,J)-CD(I-1,J))/DX2
     2     - VT(I,J)*(CD(I,J+1)-CD(I,J-1))/DY2
     3     - UC(I,J)*(CS(I+1,J)-CS(I-1,J))/DX2
     4     - VC(I,J)*(CS(I,J+1)-CS(I,J-1))/DY2
     5     - CS(I,J)*(UC(I+1,J)-UC(I-1,J))/DX2
     6     - CS(I,J)*(VC(I,J+1)-VC(I,J-1))/DY2
     7     + LAMBDA*(QD(I,J)-CS(I,J)-CD(I,J))/H(I,J)
     8     - 8.*AV*CD(I,J)/H(I,J)**2
     9     + AH*( (CD(I+1,J)-2.*CD(I,J)+CD(I-1,J))/(DELX*DELX)
     1            +(CD(I,J+1)-2.*CD(I,J)+CD(I,J-1))/(DELY*DELY) )
     2     - WE(I,J)*CS(I,J)/H(I,J)
CU   3     - (2.*PSI(I,J)*CD(I,J)/H(I,J))* ( WE(I,J)
CU   4  +  H(K,L)*((UC(I+1,J)-UC(I-1,J))/DX2+(VC(I,J+1)-VC(I,J-1))/DY2)
CU   5  + (UC(I,J)-UT(I,J))*(H(I+1,J)-H(I-1,J))/DX2
CU   6  + (VC(I,J)-VT(I,J))*(H(I,J+1)-H(I,J-1))/DY2     )
          FCD(I,J) = FCD(I,J)
C          ADD TOPOGRAPHY SEPARATELY TO AVOID PROBS WITH CONTINUATION
C             LINE LIMIT
     1  + UT(I,J)*(CS(I,J)-2.*CD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     2  + VT(I,J)*(CS(I,J)-2.*CD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
     3  - UC(I,J)*(2.*CS(I,J)-2.*CD(I,J))*(H(I+1,J)-H(I-1,J))/DX2
     4  - VC(I,J)*(2.*CS(I,J)-2.*CD(I,J))*(H(I,J+1)-H(I,J-1))/DY2
 1010   CONTINUE
 1000 CONTINUE
C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          CS(I,J) = CS(I,J) + DELT*FCS(I,J)
          CD(I,J) = CD(I,J) + DELT*FCD(I,J)
 3010   CONTINUE
 3000 CONTINUE
C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,NY) = S(X,NY-1)
C         X = 0 BC CHANGED TO NO FLUX 5-26-88
C         X = XMAX BC CHANGED TO ROBIN 5-26-88.
C     BC:
      DO 4000 I = 1, NX-1
        CS(I,1)  = CS(I,2)
        CD(I,1)  = CD(I,2)
        CS(I,NY) = CS(I,NY-1)
        CD(I,NY) = CD(I,NY-1)
 4000 CONTINUE
      DO 4010 J = 1, NY
        CS(NX,J) = (CS(NX-1,J)+DELX*GAMMA*SREF)/(1.+GAMMA*DELX)
        CD(NX,J) = (CD(NX-1,J)+DELX*GAMMA*DREF)/(1.+GAMMA*DELX)
        CS(1,J)  = CS(2,J)
        CD(1,J)  = CD(2,J)
 4010 CONTINUE
      RETURN
      END
