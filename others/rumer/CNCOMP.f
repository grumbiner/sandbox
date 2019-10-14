      SUBROUTINE CNCOMP
      INCLUDE "glgrid.inc"

      DIMENSION CN(NX,NY), U(NX,NY), V(NX,NY), JO(NX,NY), JOFN(NX,NY),
     *          DNT(NX,NY), TICE(NX,NY), DTT(NX,NY), DIV(NX,NY)
      COMMON / MASS / CN
      COMMON / DELTCN / DNT , DTT
      COMMON / DIVER / DIV
      COMMON / THICK / TICE
      COMMON / SPEEDY / U , V
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / CONSTAN / DL , DT , ETA
      DO 555 J = 1 , MM1
      DO 555 I = 1 , NM1
      JN = JOFN(I,J)
      IF ( JN .EQ. 0 ) GO TO 555
      IP1 = I + 1
      JP1 = J + 1
      IM1 = I - 1
      JM1 = J - 1
      UIJIP = U(I,J) + U(I,JP1)
      UPJPP = U(IP1,J) + U(IP1,JP1)
      VIJPJ = V(I,J) + V(IP1,J)
      VIPPP = V(I,JP1) + V(IP1,JP1)
      UIJ = ( UIJIP + UPJPP ) * .25
      VIJ = ( VIJPJ + VIPPP ) * .25
      TICEIJ = TICE (I,J)
      CNIJ = CN (I,J)
      DIFFU = ABS ( 0.5 * UIJ )
      DIFFV = ABS ( 0.5 * VIJ )
      JOX = JN / 100
      JOY = ( JN - JOX * 100 ) / 10
      IF ( JOX - 2 ) 21 , 22 , 22
21    FUX = 0.
      DFUX = 0.
      TUX = TICEIJ
      AVUX = 0.
      DIVUX = 0.
      GO TO 23
22    DFUX = ( CN(IM1,J) + CNIJ ) * UIJIP * 0.25
      FUX = DFUX - DIFFU * ( CNIJ - CN(IM1,J) )
      TUX = 0.5 * ( TICE(IM1,J) + TICEIJ )
      AVUX = DIFFU * ( TICEIJ - TICE(IM1,J) )
      DIVUX = TUX * DFUX
23    IF ( JOX - 2 ) 31 , 31 , 32
31    DFDX = ( CN(IP1,J) + CNIJ ) * UPJPP * 0.25
      FDX = DFDX - DIFFU * ( CN(IP1,J) - CNIJ )
      TDX = 0.5 * ( TICE(IP1,J) + TICEIJ )
      AVDX = DIFFU * ( TICE(IP1,J) - TICEIJ )
      DIVDX = TDX * DFDX
      GO TO 33
32    FDX = 0.
      DFDX = 0.
      TDX = TICEIJ
      AVDX = 0.
      DIVDX = 0.
33    IF ( JOY - 2 ) 41 , 42 , 42
41    FUY = 0.
      DFUY = 0.
      TUY = TICEIJ
      AVUY = 0.
      DIVUY = 0.
      GO TO 43
42    DFUY = ( CN(I,JM1) + CNIJ ) * VIJPJ * 0.25
      FUY = DFUY - DIFFV * ( CNIJ - CN(I,JM1) )
      TUY = 0.5 * ( TICE(I,JM1) + TICEIJ )
      AVUY = DIFFV * ( TICEIJ - TICE(I,JM1) )
      DIVUY = TUY * DFUY
43    IF ( JOY - 2 ) 51 , 51 , 52
51    DFDY = ( CN(I,JP1) + CNIJ ) * VIPPP * 0.25
      FDY = DFDY - DIFFV * ( CN(I,JP1) - CNIJ )
      TDY = 0.5 * ( TICE(I,JP1) + TICEIJ )
      AVDY = DIFFV * ( TICE(I,JP1) - TICEIJ )
      DIVDY = TDY * DFDY
      GO TO 53
52    FDY = 0.
      DFDY = 0.
      TDY = TICEIJ
      AVDY = 0.
      DIVDY = 0.
53    DNT(I,J) = ETA * ( FUX - FDX + FUY - FDY )
      DTT (I,J) = ETA * ( UIJ * ( TUX - TDX ) + VIJ * ( TUY - TDY ) +
     *            AVDX - AVUX + AVDY - AVUY )
      DIV(I,J) = ( DIVDX - DIVUX + DIVDY - DIVUY ) / DL
555   CONTINUE
      RETURN
      END
