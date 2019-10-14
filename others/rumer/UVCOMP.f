      SUBROUTINE UVCOMP
      INCLUDE "glgrid.inc"

      DIMENSION U(NX,NY), V(NX,NY), P(NX,NY), DUT(NX,NY), DVT(NX,NY),
     *          CN(NX,NY), JO(NX,NY), JOFN(NX,NY), TICE(NX,NY),
     *          WX(NX,NY), WY(NX,NY), DIV(NX,NY), TAUAX(NX,NY),
     *          TAUAY(NX,NY)
      COMMON / WATER / WX , WY
      COMMON / DIVER / DIV
      COMMON / MASS / CN
      COMMON / PRESUR / P
      COMMON / THICK / TICE
      COMMON / SPEEDY / U , V
      COMMON / DELTUV / DUT , DVT
      COMMON / JOINT / JO , JOFN
      COMMON / CONSTAN / DL , DT , ETA
      COMMON / PCONSTS / PST, TI, DIVLIM, K
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 ,C3
      DATA OMEMIN, RESQ / 1.0E-04, 0.25 /
C
      FFK2 ( A , B ) = 0.5 * A / B
      FOMEGA ( A , B , C ) = SQRT ( 1.25 * ( A**2 + B**2 ) + C**2
     *                       + 1.5 * A * B )
C
      DO 111 J = 1 , M
      DO 111 I = 1 , N
      IF ( JO(I,J) .NE. 1 ) GO TO 111
      IP1 = I + 1
      JP1 = J + 1
      IM1 = I - 1
      JM1 = J - 1
      UIJ = U(I,J)
      VIJ = V(I,J)
      UIJ2 = UIJ * 2.
      VIJ2 = VIJ * 2.
      PIJMMM = P(I,J) - P(IM1,JM1)
      TICEIJ = TICE(I,J)
      CTIJPM = CN(I,J) * TICEIJ - CN(IM1,JM1) * TICE(IM1,JM1)
      CTIMJ = CN(IM1,J) * TICE(IM1,J)
      CTIJM = CN(I,JM1) * TICE(I,JM1)
      D2VXY = V (IP1,JP1) + V (IM1,JM1) - V (IM1,JP1) - V (IP1,JM1)
      D2UXY = U (IP1,JP1) + U (IM1,JM1) - U (IM1,JP1) - U (IP1,JM1)
      D2UXX = U ( IM1 , J ) - UIJ2 + U ( IP1 , J )
      D2UYY = U ( I , JM1 ) - UIJ2 + U ( I , JP1 )
      DPX = PIJMMM + P(I,JM1) - P(IM1,J)
      DPY = PIJMMM + P(IM1,J) - P(I,JM1)
      DUX = U ( IP1 , J ) - U ( IM1 , J )
      DUY = U ( I , JP1 ) - U ( I , JM1 )
      DTNX = CTIJPM + CTIJM - CTIMJ
      DTNY = CTIJPM + CTIMJ - CTIJM
      DVY = V ( I , JP1 ) - V ( I , JM1 )
      DVX = V ( IP1 , J ) - V ( IM1 , J )
      D2VYY = V ( I , JM1 ) - VIJ2 + V ( I , JP1 )
      D2VXX = V ( IM1 , J ) - VIJ2 + V ( IP1 , J )
      RTRI = 1. / ( RHOICE * TICEIJ )
      ETAX = RTRI * TAUAX(I,J)
      ETAY = RTRI * TAUAY(I,J)
      CNIJ = ( CN (IM1,JM1) + CN (IM1,J) + CN (I,JM1) + CN (I,J) )
      TIJ = ( TICE(IM1,JM1)+TICE(IM1,J)+TICE(I,JM1)+TICEIJ )
      TCNIJ = TIJ * CNIJ * .0625
      SQRT1CN = SQRT (1.-0.25*CNIJ)
      UWX = UIJ - WX(I,J) * SQRT1CN
      VWY = VIJ - WY(I,J) * SQRT1CN
      PIJ = ( P (IM1,JM1) + P (IM1,J) + P (I,JM1) + P(I,J) ) * .25
      GAMAPIJ = GAMMA * PIJ
      THESQRT = C2 * RTRI * SQRT ( UWX**2 + VWY**2 )
      DVXDUY = DVX + DUY
      ABC = 0.
      IF ( TCNIJ .NE. 0. ) ABC = 1. / TCNIJ
C
C     COMPUTE BULK AND SHEAR VISCOSITY
C
      STRANX = EPS * DUX
      STRANY = EPS * DVY
      STRANXY = 0.5 * EPS * DVXDUY
      OMEGA = FOMEGA ( STRANX , STRANY , STRANXY )
      OMEGA = AMAX1 ( OMEMIN , OMEGA )
      FK2 = FFK2 ( PIJ , OMEGA )
      FK2 = AMIN1 ( FK2 , FK2MAX )
      FK2 = AMAX1 ( FK2 , FK2MIN )
      FK1 = FK2 * RESQ
      E = 2. * FK1
      FLAMBDA = FK1 - FK2
      ALPHA = ( E - FLAMBDA ) * RRDLL
      BETA = FLAMBDA * RRDLL4
      DELTA = FK1 * RRDLL4
      ZETA = 0.25 * ALPHA
      DELUYVX = DELTA * DVXDUY
C
      DELVEL = STRANX + STRANY
      FLDEP = FLAMBDA * DELVEL + PIJ
      IF ( DIV(I,J) .GE. DIVLIM ) GO TO 10
      SALPHA = ALPHA
      SBETA = BETA
      SZETA = ZETA
      GO TO 20
 10   SALPHA = ALPHA * 0.1
      SBETA = BETA * 0.1
      SZETA = ZETA * 0.1
 20   CONTINUE
C
      DUT(I,J) = DT * ( SALPHA * D2UXX - SBETA * D2VXY - GAMMA * DPX
     *   + DELTA * ( D2VXY + 4.* D2UYY ) - EPS * ( UIJ * DUX +
     *   VIJ * DUY ) + ABC * ( DTNX * ( SZETA * DUX - SBETA * DVY -
     *   GAMAPIJ ) + DTNY * DELUYVX ) + ETAX
     *   - THESQRT * UWX + C3 * VIJ )
C
      DVT(I,J) = DT * ( SALPHA * D2VYY - SBETA * D2UXY - GAMMA * DPY
     *   + DELTA * ( D2UXY + 4.* D2VXX ) - EPS * ( UIJ * DVX +
     *   VIJ * DVY ) + ABC * ( DTNY * ( SZETA * DVY - SBETA * DUX -
     *   GAMAPIJ ) + DTNX * DELUYVX ) + ETAY
     *   - THESQRT * VWY - C3 * UIJ )
C
111   CONTINUE
      RETURN
      END
