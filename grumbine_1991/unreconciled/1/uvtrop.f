C***********************************************************----------!!
      SUBROUTINE UVTROP(UT, VT, WE, H, NX, NY, DX, DY, F, BETA, AM)
C     BAROTROPIC SOLUTION.
      INTEGER NX, NY
      REAL UT(NX, NY), VT(NX, NY), WE(NX, NY), H(NX, NY)
      REAL DX, DY, F, BETA, AM

      INTEGER NNX, NNY
      PARAMETER (NNX = 36)
      PARAMETER (NNY = 36)
      REAL PSI(NNX, NNY)
      REAL NUM, LB, WAVE
      INTEGER I, J

C     COMPUTE THE INTERIOR SOLUTION
      DO 1000 J = 1, NY
        PSI(NX, J) = WE(NX, J)
        DO 1010 I = NX-1, 1, -1
          PSI(I, J) = PSI(I+1,J) + WE(I+1,J) + WE(I,J)
 1010   CONTINUE
 1000 CONTINUE
      NUM = F*DX/2./BETA/H(1,1)
      DO 1020 J = 1, NY
        DO 1030 I= 1, NX
          PSI(I,J) = -PSI(I,J) * NUM
 1030   CONTINUE
 1020 CONTINUE

C     APPLY THE BOUNDARY LAYER CORRECTIONS.
      LB = (AM/BETA)**(1./3.)
      WAVE = SQRT(3.)*DX/2./LB
      DO 2000 J = 1, NY
        NUM = LB*(PSI(NX,J)-PSI(NX-1,J))/DX
        DO 2010 I = 1, NX
          PSI(I,J) = PSI(I,J)*(1.-EXP(-(I-1)*DX/2./LB)*
     1                 (COS(WAVE*(I-1)) + SIN(WAVE*(I-1))/SQRT(3.) ))
     2              - NUM*EXP( (I-NX)*DX/LB)
 2010   CONTINUE
 2000 CONTINUE

C     NOW THAT WE HAVE PSI, COMPUTE UT, VT.
      DO 3000 J = 2, NY-1
        DO 3010 I = 2, NX-1
          UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
          VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
 3010   CONTINUE
 3000 CONTINUE
      DO 3020 J = 2, NY-1
        I = 1
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
CD      VT(I,J) =  (PSI(I+1,J)-PSI(I  ,J))   /DX
        VT(I,J) = 0.0
        I = NX
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J-1))/2./DY
        VT(I,J) =  (PSI(I  ,J)-PSI(I-1,J))   /DX
 3020 CONTINUE
      DO 3030 I = 2, NX-1
        J = 1
        UT(I,J) = -(PSI(I,J+1)-PSI(I,J))/DY
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
        J = NY
        UT(I,J) = -(PSI(I,J)-PSI(I,J-1))/DY
        VT(I,J) =  (PSI(I+1,J)-PSI(I-1,J))/2./DX
 3030 CONTINUE

      RETURN
      END
