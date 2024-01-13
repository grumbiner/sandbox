C***********************************************************----------!!
      SUBROUTINE STEXT(UC, VC, UT, VT, WE, SS, SD, QS, QD, H,
     1                 DELX, DELY, DELT, DREF, SREF,
     2                 ASH, ASV, TSTEP)
C     SUBROUTINE TO EXTRAPOLATE THE SALINITY FIELD TO THE NEXT
C       TIME STEP.

      IMPLICIT none
      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)
      INTEGER TSTEP
      REAL WE(nx, ny), UC(nx, ny), VC(nx, ny), UT(nx, ny), VT(nx, ny)
      REAL QS(nx, ny), QD(nx, ny), SS(nx, ny), SD(nx, ny)
      REAL H(nx, ny)
      REAL DELX, DELY, DELT, DREF, SREF, ASH, ASV

      INTEGER I, J

      REAL FSS(nx, ny), FSD(nx, ny), lapl(nx, ny)
      REAL DXTDX, HREF, DIFU
      REAL DYTDY, DX2, DY2, PVDIF

      DX2   = DELX+DELX
      DY2   = DELY+DELY
      DXTDX = DELX*DELX
      DYTDY = DELY*DELY
      HREF  = H(nx/2, ny/2)
      PVDIF = 8.*ASV/HREF

      DIFU = ASH/DXTDX

C     COMPUTE FORCING FOR THE INTERIOR POINTS:
      DO 100 J = 3, ny-2
        DO 101 I = 3, nx-2
          lapl(I,J) =
     1    (-SS(I+2,J)+16.*SS(I+1,J)-30.*SS(I,J)
     2               +16.*SS(I-1,J)-SS(I-2,J)   )/48.
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.
  101   CONTINUE
        I = 2
        lapl(I,J) = (11.*SS(I-1,J)-20.*SS(I,J)+6.*SS(I+1,J)
     1               +4.*SS(I+2,J)-SS(I+3,J) )/48.
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.
        I = nx-1
        lapl(I,J) = SS(I+1,J)-2.*SS(I,J)+SS(I-1,J)
     3   +(-SS(I,J+2)+16.*SS(I,J+1)-30.*SS(I,J)
     4               +16.*SS(I,J-1)-SS(I,J-2)   )/48.

  100 CONTINUE
      DO 102 I = 2, nx-1
        J = 2
        lapl(I,J) = SS(I+1,J)-2.*SS(I,J)+SS(I-1,J)
     1   +(11.*SS(I,J-1)-20.*SS(I,J)+6.*SS(I,J+1)
     2               +4.*SS(I,J+2)-SS(I,J+3) )/48.
        J = ny-1
        lapl(I,J) = SS(I+1,J)-4.*SS(I,J)+SS(I-1,J)+SS(I,J+1)+SS(I,J-1)
  102 CONTINUE

      DO 1000 J = 2, ny-1
        DO 1010 I = 2, nx-1

          FSS(I,J) =
     1     + DIFU*lapl(I,J)
     1     - (UT(I,J)*(SS(I+1,J)-SS(I-1,J))
     2       +VT(I,J)*(SS(I,J+1)-SS(I,J-1))
     3       +UC(I+1,J)*SD(I+1,J)-UC(I-1,J)*SD(I-1,J)
     4       +VC(I,J+1)*SD(I,J+1)-VC(I,J-1)*SD(I,J-1) )/DX2
     6     + (QS(I,J) - WE(I,J)*SD(I,J)) / HREF
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     3     + ( (UT(I,J)*UT(I,J)*DELT*0.5)
     4           * (SS(I+1,J)-2.*SS(I,J)+SS(I-1,J))
     5       + (VT(I,J)*VT(I,J)*DELT*0.5)
     6           * (SS(I,J+1)-2.*SS(I,J)+SS(I,J-1))  ) / DXTDX

          FSD(I,J) =
     1     - (UT(I,J)*(SD(I+1,J)-SD(I-1,J))
     2       +VT(I,J)*(SD(I,J+1)-SD(I,J-1))
     3       +UC(I,J)*(SS(I+1,J)-SS(I-1,J))
     4       +VC(I,J)*(SS(I,J+1)-SS(I,J-1))     ) / DX2
     5     + (QD(I,J) - SD(I,J)* PVDIF          ) / HREF
C      Adopt upwind vertical differencing for test 8-10-90.BG
     6     - SD(I,J)*ABS(WE(I,J)/H(I,J)+(UC(I+1,J)-UC(I-1,J)+
     7                                   VC(I,J+1)-VC(I,J-1))/DX2 )
C          ADOPT LAX-WENDROFF DIFFERENCING. 8-8-89.
     2     + ((ASH + UT(I,J)*UT(I,J)*DELT*0.5 )
     3           * (SD(I+1,J)-2.*SD(I,J)+SD(I-1,J))
     4       +(ASH + VT(I,J)*VT(I,J)*DELT*0.5 )
     5           * (SD(I,J+1)-2.*SD(I,J)+SD(I,J-1)) ) / DXTDX

 1010   CONTINUE
 1000 CONTINUE

C     EXTRAPOLATE THE INTERIOR VALUES:
      DO 3000 J = 2, ny-1
        DO 3010 I = 2, nx-1
          SS(I,J) = SS(I,J) + DELT*FSS(I,J)
          SD(I,J) = SD(I,J) + DELT*FSD(I,J)
 3010   CONTINUE
 3000 CONTINUE

C     NOW MUST APPLY THE BOUNDARY CONDITIONS.
C     CONDITION FOR THE Y=0 BNDY, NO FLUX: S(X,1) = S(X,2)
C                     Y = YMAX  ,          S(X,ny) = S(X,ny-1)
C         X=0 BC CHANGED TO NO FLUX 5-26-88
C     BC:
      DO 4000 I = 2, nx-1
        SS(I,1)  = SS(I,2)
        SD(I,1)  = SD(I,2)
        SS(I,ny) = SS(I,ny-1)
        SD(I,ny) = SD(I,ny-1)
 4000 CONTINUE
      DO 4010 J = 2, ny-1
        SS(nx,J) = SS(nx-1,J)
        SD(nx,J) = SD(nx-1,J)
        SS(1,J)  = SS(2,J)
        SD(1,J)  = SD(2,J)
 4010 CONTINUE
      SS(1,1)   = SS(2,2)
      SS(1,ny)  = SS(2,ny-1)
      SS(nx,1)  = SS(nx-1,2)
      SS(nx,ny) = SS(nx-1, ny-1)
      SD(1,1)   = SD(2,2)
      SD(1,ny)  = SD(2,ny-1)
      SD(nx,1)  = SD(nx-1,2)
      SD(nx,ny) = SD(nx-1, ny-1)

      RETURN
      END
