      PROGRAM CIEXXX (INPUT,OUTPUT,TAPE9,TAPE8,TAPE5=INPUT,TAPE6=OUTPUT)
C*******************************************************************************
C
C                               PROGRAM CONTOUR
C
C                     AUGUST 4TH , 1978   BY EIJI FUKUMORI
C
C                       DEPARTMENT OF CIVIL ENGINEERING
C
C                   STATE UNIVERSITY OF NEW YORK AT BUFFALO
C
C*******************************************************************************
C
C
C                        MODIFIED FOR ARBITRARY SHAPE
C
C                   AS A GRAPHIC OUTPUT RETRIEVAL SYSTEM
C
C                  FOR GREAT LAKES ICE DYNAMICS SIMULATION
C
C                               BY AKIO WAKE
C
C                               OCTOBER 1980
C
C
C*******************************************************************************
C
C     LIST OF VARIABLES
C
C     WHAT    : CODE FOR PLOTTING ITEM ( N, T, P, V )
C     TIMESEC : DATA RECORDING TIME
C     FMIN    : STARTING VALUE FOR CONTOUR PLOT
C     FMAX    : MAXIMUM VALUE FOR CONTOUR PLOT
C     DH      : INCREMENT FOR CONTOUR LEVEL
C     BOUNDX  : BOUNDARY SHAPE OF THE WATER BODY IN X COORDINATE
C     BOUNDY  : BOUNDARY SHAPE OF THE WATER BODY IN Y COORDINATE
C     N       : PLOTTING CODE FOR ICE CONCENTRATION
C     T       : PLOTTING CODE FOR ICE THICKNESS
C     V       : PLOTTING CODE FOR ICE DRIFT VELOCITY
C     P       : PLOTTING CODE FOR ICE PRESSURE
C     N       : NUMBER OF GRID POINT IN X DIRECTION
C     M       : NUMBER OF GRID POINT IN Y DIRECTION
C
C     LIST OF SUBROUTINES
C
C     SUBROUTINE PLOTVP  : INITIALIZATION OF VELOCITY OR PRESSURE PLOT
C     SUBROUTINE ARHEAD  : PLOTTING ARROW HEAD ON THE VELOCITY VECTORS
C     SUBROUTINE PVPLOT  : PRESSURE PLOTTING AND VELOCITY PLOTTING
C     SUBROUTINE INITIAL : INITIALIZATION OF THE CONTOUR PLOTTING
C     SUBROUTINE MIDASH  : PLOTTING BOUNDARY FRAME OF THE WATER BODY
C                          AND WRITTING THE LEGEND
C     SUBROUTINE FINDER2 : FINDING THE STARTING POSITION OF THE
C                          CONTOUR LINE ON THE BOUNDARY OF WATER BODY
C     SUBROUTINE SEARCH2 : INTERPLOLATION OF THE STARTING POSITION OF
C                          THE CONTOUR LINE ON THE BOUNDARY OF WATER BODY
C     SUBROUTINE TRACE2  : LOCATING THE POSITIONS OF THE CONTOUR LINE
C                          INSIDE THE TWO DIMENSIONAL REGION
C     SUBROUTINE LIST    : PRINTING OUTPUT
C     SUBROUTINE PCURVE  : PLOTTING SMOOTH CONTOUR CURVE
C     SUBROUTINE PLOTT   : PLOTTING SMOOTH CONTOUR LINE
C     SUBROUTINE PLOTCL  : LOCATING THE PROPER POSITION ON THE CONTOUR
C                          LINE TO WRITE THE NUMBER
C     SUBROUTINE ANGER   : CALCULATION OF THE ANGULAR ORIENTATION OF THE
C                          NUMBER TO BE WRITTEN ON THE CONTOUR LINE
C
C******************************************************************************
C
C
      DIMENSION A(78,21), MM(4), U(78,21), V(78,21),
     *          JO(78,21), JOFN(78,21), BOUNDX(229), BOUNDY(229),
     *          BISLX1(7), BISLY1(7), BISLX2(9), BISLY2(9),
     *          BISLX3(9), BISLY3(9), MT(5,4), NCHAR(5),
     *          XD(50,120), YD(50,120), DATE(3)
      COMMON /MEMORY/ XD, YD
      COMMON / MDATE / DATE
      COMMON / ABC / NX, NY, A / DEF / DX, DY / XYZ / EPS / AAA / IR
      COMMON / EFG / DH, CH, CLEN, ICH, ID, MINIP /BBB/ NLETT, MM, HR
      COMMON / FGH / STX , STY , ISY , ANGLE ,  HEIGHT
      COMMON / JOINT / JO , JOFN
      COMMON / SHAPE / BOUNDX, BOUNDY, BISLX1, BISLY1, BISLX2,
     *                 BISLY2, BISLX3, BISLY3
      COMMON / SPEEDY / U , V
      DATA XD, YD / 12000*-999. /
      DATA ICH , ID / 10 , 1 /
      DATA STX, STY, DX, DY / -3.0, -2.0, 1.0, 1.0 /
      DATA ISY, IR, EPS, HEIGHT, CH / 3, 3, 0.67, 4.49, 0.0 /
      DATA BISLX1 , BISLY1 / 8., 9., 9., 8., 8., 0., 0.,
     *                       10., 10., 11., 11., 10., 0., 0. /
      DATA BISLX2 , BISLY2 / 9., 10., 10., 10., 9., 9., 9., 0., 0.,
     *                       7., 7., 8., 9., 9., 8., 7., 0., 0. /
      DATA BISLX3 , BISLY3 /11., 12., 12., 12., 11., 11., 11., 0., 0.,
     *                      10., 10., 11., 12., 12., 11., 10., 0., 0. /
C
      DATA NCHAR / 39, 37, 29, 40, 34 /
      DATA (MT(1,J),J=1,4)/10HAREAL ICE ,10HCONCENTRAT,
     &                     10HION FIELD ,10H AT  T =  /
      DATA (MT(2,J),J=1,4)/10HINTERNAL I,10HCE PRESSUR,
     &                     10HE FIELD  A,10HT  T =    /
      DATA (MT(3,J),J=1,4)/10HICE THICKN,10HESS FIELD ,
     &                     10H AT  T =  ,10H          /
      DATA (MT(4,J),J=1,4)/10HICE MASS F,10HLUX DIVERG,
     &                     10HENCE FIELD,10H  AT  T = /
      DATA (MT(5,J),J=1,4)/10HICE DRIFT ,10HVELOCITY F,
     &                     10HIELD  AT  ,10HT =       /
C
      READ ( 5 , 112 ) ( DATE(I), I = 1 , 3 )
      READ ( 5 , 100 ) WHAT , TIMESEC
      READ ( 5 , 101 ) FMIN , FMAX , DH
      READ ( 5 , 109 ) ( BOUNDX(I), I = 1 , 229 )
      READ ( 5 , 109 ) ( BOUNDY(I), I = 1 , 229 )
      WRITE ( 6 , 111 ) ( BOUNDX(I), I = 1 , 229 )
      WRITE ( 6 , 110 )
      WRITE ( 6 , 111 ) ( BOUNDY(I), I = 1 , 229 )
      ICODE = 9
      IF ( WHAT .EQ. 1HN ) ICODE = 1
      IF ( WHAT .EQ. 1HP ) ICODE = 2
      IF ( WHAT .EQ. 1HT ) ICODE = 3
      IF ( WHAT .EQ. 1HD ) ICODE = 4
      IF ( WHAT .EQ. 1HV ) ICODE = 5
      IF ( ICODE .EQ. 9 ) GO TO 99
      DO 9 I = 1, 4
    9 MM(I) = MT(ICODE,I)
      NLETT = NCHAR(ICODE)
      HR = TIMESEC/3600.
      READ ( 9 , 102 ) N , M , NX , NY
      READ ( 9 , 104 ) IDUM
      ISS = FLOAT(N*M)/15. + 0.94
      ISSM = FLOAT(NX*NY)/15. + 0.94
      READ ( 9 , 102 ) ( (JO(I,J), J = 1, M), I = 1, N )
      READ ( 9 , 102 ) ( (JOFN(I,J), J = 1, NY), I = 1, NX )
      DO 58 ITR = 1 , 200
      READ ( 9 , 101 ) T , WSP , ANGLE
      IF ( EOF(9) ) 97, 54
   54 WRITE ( 6 , 106 ) T , WSP , ANGLE
C
      IF ( T .EQ. TIMESEC ) GO TO 40
      DO 10 I = 1 , 2
      DO 10 J = 1 , ISS
   10 READ(9,104) IDUM
      DO 11 I = 1 , 4
      DO 11 J = 1 , ISSM
   11 READ(9,104) IDUM
      GO TO 58
   40 READ ( 9 , 101 ) ( (U(I,J), J = 1, M), I = 1, N )
      READ ( 9 , 101 ) ( (V(I,J), J = 1 , M ), I = 1, N )
      IF ( ICODE .EQ. 5 ) GO TO 90
      DO 43 JI = 1,4
      IF ( ICODE .NE. 4 ) GO TO 47
      READ ( 9 , 107 ) ( (A(I,J), J = 1, NY), I = 1, NX )
      GO TO 44
   47 READ ( 9 , 101 ) ( (A(I,J), J = 1, NY), I = 1, NX )
   44 IF ( ICODE .EQ. JI ) GO TO 5
   43 CONTINUE
   58 CONTINUE
    5 IF ( ICODE .NE. 2 ) GO TO 90
      READ ( 9 , 101 ) ( (U(I,J), J = 1, NY), I = 1, NX )
   90 REWIND 9
      GO TO 98
   99 WRITE(6,105)
      GO TO 999
   97 WRITE(6,103)
      GO TO 999
   98 GO TO (70,71,70,70,71), ICODE
   71 CALL PLOTVP (ICODE,HEIGHT,NX,NY,WSP)
      GO TO 888
  100 FORMAT ( A1 , F10.0 )
  101 FORMAT ( 15F8.0 )
  102 FORMAT ( 30I4 )
  103 FORMAT ( 10X, * EOF !* )
  104 FORMAT ( A1 )
  105 FORMAT ( 5X , * NO MATCH * )
  106 FORMAT (/,10X,*T=*,F10.2,5X,*WSP=*,F10.2,5X,*ANGLE=*,F10.2,/ )
  107 FORMAT ( 15E8.1 )
  109 FORMAT ( 20F3.0 )
  110 FORMAT ( / )
  111 FORMAT ( 3X , 20F4.0 )
  112 FORMAT ( 3A10 )
   70 CONTINUE
      IF ( DH .LE. 0. ) DH = 0.1
      IF ( IR .EQ. 0 ) IR = 2
      IF ( CH .LT. 0.075 ) CH = 0.075
      IF ( CH .GT. 0.1250 ) CH = 0.09375
      CALL INITAL ( FMIN , FMAX , IC , FMAX9, WSP, ICODE )
      NDH = ( FMAX - FMIN )/DH + 1.000005
      CL = FMIN
      DO 20 I = 1 , NDH
      IMP = ALOG 10 ( ABS ( CL ) )
      IF ( IMP .LT. 0 ) IMP = 0
      IMP = IMP + 1
      CLEN = CH * FLOAT ( 1 + ID + IMP ) * 0.98
      IF ( CL .GE. 0 ) CLEN = CLEN - CH * 0.98
      MINIP = MAX0 ( 6 , IFIX ( CLEN / DY + 1.00005 ) * ICH )
      CALL FINDER2 ( NLI , NLO , CL )
      DO 25 IW = 1, 50
      DO 25 JW = 1, 120
      XD(IW,JW) = -999.
   25 YD(IW,JW) = -999.
      IF ( IR .NE. 2 ) CALL LIST ( NLI , NLO , 99999. )
C     ------------------------------
C     IF ( I .GE. 4 ) DH = 0.2
C     ------------------------------
      CL = CL + DH
   20 CONTINUE
      IF ( IC .NE. 9 ) GO TO 30
      CL = CL - DH
      IF ( IFIX ( ABS ( CL - FMAX ) * 1000. ) .EQ. 0 ) GO TO 30
      CALL FINDER2 ( NLI , NLO , FMAX9 )
      DO 26 IW = 1, 50
      DO 26 JW = 1, 120
      XD(IW,JW) = -999.
   26 YD(IW,JW) = -999.
      IF ( IR .NE. 2 ) CALL LIST ( NLI , NLO , 99999. )
   30 IF ( IR .EQ. 1 ) STOP
888   CALL EFPLOT
999   STOP
      END
      SUBROUTINE PLOTVP (ICODE,HEIGHT,NX,NY,WSP)
      DIMENSION MM(4)
      COMMON /DEF/ DX, DY
      COMMON /BBB/ NLETT, MM, HR
C
      NXX = NX+1
      NYY = NY+1
      YMAX = FLOAT(NY) * DY
      SIZE = HEIGHT / YMAX
      CALL PLOTS
      CALL PLOT ( 3.0, 1.3, -3 )
      CALL MIDASH ( NLETT, MM, HR, SIZE, NXX, NYY, WSP, ICODE )
      CALL PVPLOT (ICODE)
      RETURN
      END
      SUBROUTINE ARHEAD ( X1, Y1, AX1, AY1, AX2, AY2, CC, DDD )
C
C      CALCULATES COORDINATES FOR ARROW HEAD
C
      D = ( 1.0/DDD ) ** 2
      RSQ = D * ( 1.0 - CC ) ** 2 * ( X1 ** 2 + Y1 ** 2 )
      IF ( ABS(X1) .LT. .00001 ) GO TO 10
      IF ( ABS(Y1) .LT. .00001 ) GO TO 10
C
      SA = Y1 / X1
      ALP = -1.0 / SA
      BETA = ( CC - 1.0 ) * ( X1 / SA + Y1 )
C
      A = 1.0 + ALP ** 2
      B = ALP * BETA
      C = BETA ** 2 - RSQ
C
      Q = B ** 2 - A * C
      QRT = SQRT (Q)
      AX1 = ( -B + QRT ) / A
      AX2 = ( -B - QRT ) / A
      AY1 = ALP * AX1 + BETA
      AY2 = ALP * AX2 + BETA
C
      RETURN
   10 CONTINUE
      IF ( ABS(X1) .LT. .00001 .AND.
     &     ABS(Y1) .LT. .00001 ) GO TO 50
      SS = 1.0 - DDD ** 2
      SS = SQRT (SS)
      R = SQRT ( RSQ )
      AX1 = R * SS
      AX2 = - AX1
      AY1 = R * DDD
      AY2 = AY1
      IF ( ABS(Y1) .LT. .00001 ) GO TO 15
      IF ( Y1 .GT. 0.0 ) GO TO 20
      RETURN
   15 CONTINUE
      AX1 = AY1
      AY1 = AX2
      AY2 = - AY1
      AX2 = AX1
      IF ( X1 .GT. 0.0 ) GO TO 30
      RETURN
   30 CONTINUE
      AX1 = - AX1
      AX2 = AX1
      RETURN
   50 CONTINUE
      AX1 = X1
      AX2 = X1
      AY1 = Y1
      AY2 = Y1
      RETURN
   20 CONTINUE
      AY1 = - AY1
      AY2 = AY1
      RETURN
      END
      SUBROUTINE INITAL ( FMIN , FMAX , IC , FMAX9, WSP, ICODE )
      DIMENSION A ( 78 , 21 )
      DIMENSION MM ( 4 )
      COMMON / ABC / NX , NY , A / DEF / DX , DY / XYZ / EPS
      COMMON / EFG / DH , CH , CLEN , ICH , ID , MINIP
      COMMON / FGH / STX , STY , ISY , ANGLE ,  HEIGHT
      COMMON / AAA / IR  / BBB / NLETT , MM, HR
      IC = 0
      DCH = CH
      IF ( NLETT .GT. 70 ) NLETT = 70
      IF ( IR .EQ. 1 ) GO TO 33
      IF ( ISY .EQ. 0 ) ISY = 3.
      YMAX = FLOAT ( NY  ) * DY
      IF ( HEIGHT .GT. 9. ) HEIGHT = 9.
      IF ( HEIGHT .LE. 0. ) HEIGHT = 9.
      SIZE = HEIGHT / Y MAX
      CH = CH / SIZE
      CALL PLOTS
      CALL PLOT ( 3.0 , 1.3 , - 3 )
      CALL MIDASH ( NLETT , MM , HR , SIZE , NX+1 , NY+1, WSP, ICODE )
      CALL FACTOR ( SIZE )
   33 IF ( FMAX - FMIN ) 44 , 44 , 55
   44 IF ( FMIN .GT. FMAX ) IC = 9
      RMAX = A ( 1 , 1 )
      RMIN = A ( 1 , 1 )
      DO 10 I = 1 , NX
      DO 10 J = 1 , NY
      RMAX = AMAX1 ( RMAX , A ( I , J ) )
      RMIN = AMIN1 ( RMIN , A ( I , J ) )
   10 CONTINUE
      FMAX9 = FLOAT ( IFIX ( RMAX / 0.1 ) ) * 0.1
      FMAX = FLOAT ( IFIX ( RMAX / DH ) ) * DH
      FMIN = FLOAT ( IFIX ( RMIN / DH ) ) * DH + DH
   55 CONTINUE
      WRITE ( 6 , 444 ) NLETT , MM
  444 FORMAT ( *1* /// 20X , 5X , *NLETT     MM* / 20X,I10, 5X, 4A10 )
      WRITE ( 6 , 555 ) STX , STY , ISY , ANGLE
  555 FORMAT ( / 20X, 7X, *STX*, 7X, *STY*, 7X, *ISY*, 5X, *ANGLE* /
     *           20X, 2F10.2 , I10 , F10.2 )
      WRITE ( 6 , 666 ) NX , NY , DX , DY
  666 FORMAT ( / 20X , 8X , *NX* , 8X, *NY*, 8X, *DX*, 8X, *DY* /
     *           20X , 2 I 10 , 2 F 10.2 )
      WRITE ( 6 , 777 ) DH , FMIN , FMAX , IR , EPS , HEIGHT , DCH
  777 FORMAT ( / 20X , 8X , *DH*, 6X , *FMIN* , 6X , *FMAX*,
     *            8X , *IR* , 7X , *EPS* , 4X , *HEIGHT*
     *                     8 X , *CH*
     *         / 20 X , 3 F 10.2 , I 10 , 2 F 10.2 , F 10.4 )
      WRITE ( 6 , 888 )  NX , NY
  888 FORMAT ( /// 60X , *INPUT MATRIX (*,I3,* X*,I3,*)*/  )
      DO 22 I = 1 , NX
      WRITE ( 6 , 333 )
      WRITE ( 6 , 222 ) ( A ( I , J ) , J = 1 , NY )
  222 FORMAT ( 22 F 6 . 2 )
  333 FORMAT ( * * )
   22 CONTINUE
      DO 999 I = 1,NX
      DO 999 J = 1,NY
      IF ( A(I,J) .EQ. FMIN ) A(I,J) = A(I,J) - 1.0E-03
      IF ( A(I,J) .EQ. FMAX ) A(I,J) = A(I,J) + 1.0E-03
  999 IF ( A(I,J) .LT. 0. ) A(I,J) = 0.
      RETURN
      E  N  D
      SUBROUTINE SEARCH2 ( I , J , CL , IB , NL )
      DIMENSION A ( 78 , 21 ), IPP(50)
      DIMENSION X ( 120 ) , Y ( 120 )
      DIMENSION XT (120) , YT (120) , XD (50,120) , YD (50,120)
      COMMON / ABC / NX , NY , A  / BCD / X , Y , IP  / AAA / IR
      COMMON /MEMORY/ XD, YD
      DATA IPP / 50*0 /
      GO TO ( 100 , 200 , 300 , 400 , 100 ) , IB
  100 X ( 1 ) = I - 1
      Y ( 1 ) = J - 2 + ( CL - A ( I , J - 1 ) ) /
     *                  ( A ( I , J ) - A ( I , J - 1 ) )
      GO TO 20
  200 Y ( 1 ) = J - 1
      X ( 1 ) = I - 1 + ( CL - A ( I , J ) ) /
     *                  ( A ( I + 1 , J ) - A ( I , J ) )
      GO TO 20
  300 X ( 1 ) = I
      Y ( 1 ) = J - 2 + ( CL - A ( I + 1 , J - 1 ) ) /
     *                  ( A ( I + 1 , J ) - A ( I + 1 , J - 1 ) )
      GO TO 20
  400 X ( 1 ) = I - 1 + ( CL - A ( I , J- 1 ) ) /
     *                  ( A ( I + 1 , J - 1 ) - A ( I , J - 1 ) )
      Y ( 1 ) = J - 2
   20 CONTINUE
      IF ( IB .EQ. 5 ) GO TO 23
      IF ( NL .EQ. 0 ) GO TO 23
      DO 25 K = 1 , NL
      IPE = IPP(K)
      IF ( X ( 1 ).EQ.XD (K,IPE) .AND. Y ( 1 ).EQ.YD (K,IPE) ) RETURN
   25 CONTINUE
   23 CONTINUE
      CALL TRACE2 ( I , J , CL , IB , IP , IND )
      IF ( IND .EQ. 0 .OR. IP .EQ. 1 ) RETURN
      IF ( NL .EQ. 0 ) GO TO 73
      DO 70 K = 1,NL
      IPE = IPP(K)
      DO 70 K2 = 1,IPE
      IF ( X(1).EQ.XD(K,K2) .AND. Y(1).EQ.YD(K,K2) ) RETURN
   70 CONTINUE
      DO 71 K = 1,NL
      IPE = IPP(K)
      IF ( X(IP).EQ.XD(K,IPE) .AND. Y(IP).EQ.YD(K,IPE) ) GO TO 72
      GO TO 71
   72 IPNEW = IP - IPE + 1
      IF ( IPNEW .LT. 2 ) RETURN
      IP = IPNEW
   71 CONTINUE
   73 NL = NL + 1
      IPP(NL) = IP
      IF ( IND .EQ. 2 ) GO TO 10
      DO 40 KW = 1, IP
      XD ( NL, KW ) = X ( KW )
   40 YD ( NL, KW ) = Y ( KW )
      IF ( IR .NE. 2 ) CALL LIST ( 1 , 0 , CL )
      IF ( IR .NE. 1 ) CALL PCURVE ( 1 , 0 , CL )
      RETURN
   10 CONTINUE
      IF ( NL .NE. 1 ) GO TO 30
   15 XT ( NL ) = X ( 1 )
      YT ( NL ) = Y ( 1 )
      IF ( IR .NE. 2 ) CALL LIST ( 0 , 1 , CL )
      IF ( IR .NE. 1 ) CALL PCURVE ( 0 , 1 , CL )
      RETURN
   30 NLD = NL - 1
      DO 60 K = 1 , NLD
      DO 50 L = 1 , IP
      IF ( X ( L ) .NE. XT ( K ) ) GO TO 50
      IF ( Y ( L ) .NE. YT ( K ) ) GO TO 50
      NL = NL - 1
      RETURN
   50 CONTINUE
   60 CONTINUE
      GO TO 15
      END
      SUBROUTINE PCURVE ( NLI , NLO , CL )
      DIMENSION X ( 120 ) , Y ( 120 )
      COMMON / BCD / X , Y , IP / DEF / DX , DY
      IF ( NLI .EQ. 0 ) GO TO 60
      IF ( IP .LT. 2 ) RETURN
      DO 5 I = 1 , IP
      X ( I ) = X ( I ) * DX
      Y ( I ) = Y ( I ) * DY
    5 CONTINUE
      IF ( IP .GT. 3 ) GO TO 10
      IF ( IP .EQ. 3 ) GO TO 6
      CALL PLOT ( X ( 1 ) , Y ( 1 ) , 3 )
      CALL PLOT ( X ( 2 ) , Y ( 2 ) , 2 )
      RETURN
    6 CONTINUE
      XA =  0.5 * X ( 1 ) -     X ( 2 ) + 0.5 * X ( 3 )
      XB = -2.5 * X ( 1 ) + 4.* X ( 2 ) - 1.5 * X ( 3 )
      XC =  3.0 * X ( 1 ) - 3 * X ( 2 ) +       X ( 3 )
      YA =  0.5 * Y ( 1 ) -     Y ( 2 ) + 0.5 * Y ( 3 )
      YB = -2.5 * Y ( 1 ) + 4.* Y ( 2 ) - 1.5 * Y ( 3 )
      YC =  3.0 * Y ( 1 ) - 3 * Y ( 2 ) +       Y ( 3 )
      CALL PLOT ( X ( 1 ) , Y ( 1 ) , 3 )
      DO 7 I = 11 , 30
      T = FLOAT ( I ) / 10.
      XP = XA * T ** 2 + XB * T + XC
      YP = YA * T ** 2 + YB * T + YC
      CALL PLOT ( XP , YP , 2 )
    7 CONTINUE
      RETURN
   10 CONTINUE
      CALL PLOTT ( IP , 0 , CL )
      RETURN
C
   60 CONTINUE
      IF ( NLO .EQ. 0 ) RETURN
      DO 75 I = 1, IP
      X ( I ) = X ( I ) * DX
      Y ( I ) = Y ( I ) * DY
   75 CONTINUE
      IP = IP - 1
      CALL PLOTT ( IP , - 1 , CL )
      RETURN
      E  N  D
      SUBROUTINE PLOTT ( IP , IPEN , CL )
      DIMENSION X ( 120 ) , Y ( 120 )
      DIMENSION IPK ( 10 )
      COMMON / BCD / X , Y , IPDUM / DEF / DX , DY  / XYZ / EPS
      COMMON / EFG / DH , CH , CLEN , ICH , ID , MINIP
      IP1 = IP - 1
      ICK = 1
      IF ( IP .LE. MINIP ) GO TO 100
      IST = IP / ( ICH + 1 )
      IPK ( 1 ) = IST
      DO 10 I = 2 , ICH
   10 IPK ( I ) = IPK ( I - 1 ) + IST
      ICK = 0
  100 CONTINUE
      IS = 2
  200 CONTINUE
      DO 80 I = IS , IP1
      IF ( IP .LE. 6 ) GO TO 55
      IF ( Y ( I + 1 ) .NE. Y ( I - 1 ) ) GO TO 44
      IF ( Y ( I + 1 ) .EQ. Y ( I ) ) GO TO 44
      SX = ABS ( X ( I + 1 ) ) - ABS ( X ( I - 1 ) )
      SY = ABS ( Y ( I + 1 ) ) - ABS ( Y ( I ) )
      RATIO = ABS ( SX / SY )
      IF ( EPS .GE. RATIO ) Y ( I ) = ( Y ( I - 1 ) + Y ( I ) ) / 2.
      GO TO 55
   44 IF ( X ( I - 1 ) .NE. X ( I + 1 ) ) GO TO 55
      IF ( X ( I + 1 ) .EQ. X ( I ) ) GO TO 55
      SY = ABS ( Y ( I + 1 ) ) - ABS ( Y ( I - 1 ) )
      SX = ABS ( X ( I + 1 ) ) - ABS ( X ( I ) )
      RATIO = ABS ( SY / SX )
      IF ( EPS .GE. RATIO ) X ( I ) = ( X ( I - 1 ) + X ( I ) ) / 2.
   55 CONTINUE
      IF ( ICK .NE. 0 ) GO TO 80
      II = I
      DO 30 K = 1 , ICH
      IF ( IPK ( K ) .EQ. I ) GO TO 40
   30 CONTINUE
   80 CONTINUE
      GO TO 60
   40 CALL PLOTCL ( CL , ICK , IS , I , IP1 )
      GO TO 200
   60 CONTINUE
      CALL SMOOT ( X ( 1 ) , Y ( 1 ) , IPEN )
      IF ( IP .LE. MINIP .OR. ICK .EQ. 0 ) GO TO 210
      DO 70 I = 2 , II
   70 CALL SMOOT ( X ( I ) , Y ( I ) , -2 )
      CALL SMOOT ( X ( IS ) , Y ( IS ) , - 3 )
      IS = IS + 1
      DO 85 I = IS , IP1
   85 CALL SMOOT ( X ( I ) , Y ( I ) , - 2 )
   90 CALL SMOOT ( X ( IP ) , Y ( IP ) , - 24 )
      RETURN
  210 DO 20 I = 2 , IP1
      CALL SMOOT ( X ( I ) , Y ( I ) , - 2 )
   20 CONTINUE
      GO TO 90
      END
      SUBROUTINE PLOTCL ( CL , ICK , IS , II , N )
      DIMENSION A ( 78 , 21 )
      DIMENSION X ( 120 ) , Y ( 120 )
      COMMON / ABC / NX , NY , A  / BCD / X , Y , IP / DEF / DX , DY
      COMMON / EFG / DH , CH , CLEN , ICH , ID
      IS = II + 1
      IF ( II .LE. 4 ) RETURN
      I = X ( II ) / DX + 1.000001
      J = Y ( II ) / DY + 1.000001
      IF ( I .GE. NX ) RETURN
      IF ( J .GE. NY ) RETURN
      IF ( I .LE. 1 ) RETURN
      IF ( J .LE. 1 ) RETURN
      NCL = ABS ( A ( I , J ) - A ( I , J + 1 ) ) / DH + 0.5001
      NCL1 = ABS ( A ( I , J ) - A ( I + 1 , J ) ) / DH + 0.50001
      NCL2 = ABS ( A ( I , J ) - A ( I - 1, J ) ) / DH + 0.5001
      NCL3 = ABS ( A ( I , J ) - A ( I ,J - 1 ) ) / DH + 0.5001
      NCL = MAX0 ( NCL , NCL1 , NCL2 , NCL3 )
      IF ( NCL .EQ. 0 ) GO TO 5
      SPACE = DY / ( 1.1 * FLOAT ( NCL ) )
      IF ( SPACE .LE. CH ) RETURN
    5 J = IS
   10 DIST =        SQRT ( ( X ( J ) - X (II ) ) ** 2 +
     *                     ( Y ( J ) - Y (II ) ) ** 2 )
      IF ( DIST .GT. CLEN + 1.2 * CH ) GO TO 20
      J = J + 1
      IF ( J .GE. N ) RETURN
      GO TO 10
   20 CALL ANGER ( IS , II , J , J - 1  , TANG )
      IF ( TANG .GE. 1.0 ) RETURN
      IF ( J .EQ. IS .AND. DIST / 2. .GT. CLEN + 2.5 * CH ) RETURN
      SL1 = ( Y ( J ) - Y ( II ) ) / ( X ( J ) - X ( II ) + 1.E-8 )
      ANGR = ATAN ( SL1 )
      ANGD = ANGR * 57.29578
      IF ( X ( II ) .GT. X ( J ) ) GO TO 24
      DDX = CH * ( COS ( ANGR ) + 0.5 * COS ( ANGR - 1.57029 ) )
      DDY = CH * ( SIN ( ANGR ) + 0.5 * SIN ( ANGR - 1.57029 ) )
      DDX = X ( II ) + DDX
      DDY = Y ( II ) + DDY
      X ( J     ) = ( CLEN + 2. * CH ) * COS ( ANGR ) + X ( II )
      Y ( J     ) = ( CLEN + 2. * CH ) * SIN ( ANGR ) + Y ( II )
      GO TO 26
   24 DDX = X ( II ) - ( CLEN + CH ) * COS ( ANGR )
     *               + 0.5 * CH * COS ( ANGR - 1.57029 )
      DDY = Y ( II ) - ( CLEN + CH ) * SIN ( ANGR )
     *               + 0.5 * CH * SIN ( ANGR - 1.57029 )
      X ( J     ) = X ( II ) - ( CLEN + 2. * CH ) * COS ( ANGR )
      Y ( J     ) = Y ( II ) - ( CLEN + 2. * CH ) * SIN ( ANGR )
   26 CALL NUMBER ( DDX , DDY , CH , CL , ANGD , ID )
      IS = J
      ICK = 1
      RETURN
      E N D
      SUBROUTINE ANGER ( IS , II , J , I ,TANG )
      DIMENSION X ( 120 ) , Y ( 120 )
      COMMON / BCD / X , Y , IP
      PI = 3.14159
      IF ( X ( IS ) .EQ. X ( II ) ) GO TO 5
      SL1 = ( Y ( IS ) - Y ( II ) ) / ( X ( IS ) - X ( II ) )
      ANG1 = ATAN ( ABS ( SL1 ) )
    5 IF ( X ( J ) .EQ. X ( I ) ) GO TO 6
      SL2 = ( Y ( J ) - Y ( I ) ) / ( X ( J ) - X ( I ) )
      ANG2 = ATAN ( ABS ( SL2 ) )
    6 CONTINUE
      IF ( X ( IS ) - X ( II ) ) 10 , 20 , 30
   10 IF ( Y ( IS ) - Y ( II ) ) 40 , 50 , 60
   40 ANG1 = ANG1 + PI
      GO TO 70
   50 ANG1 = PI
      GO TO 70
   60 ANG1 = PI - ANG1
      GO TO 70
   20 IF ( Y ( IS ) - Y ( II ) ) 80 , 90 , 90
   80 ANG1 = 1.5 * PI
      GO TO 70
   90 ANG1 = 0.5 * PI
      GO TO 70
   30 IF ( Y ( IS ) - Y ( II ) ) 100 , 110 , 70
  100 ANG1 = - ANG1 + 2. * PI
      GO TO 70
  110 ANG1 = 0.
      GO TO 70
   70 IF ( X ( J ) - X ( I ) ) 120 , 130 , 140
  120 IF ( Y ( J ) - Y ( I ) ) 145 , 150 , 160
  145 ANG2 =   ANG2 + PI
      GO TO 300
  150 ANG2 = PI
      GO TO 300
  160 ANG2 = PI - ANG2
      GO TO 300
  130 IF ( Y ( J ) - Y ( I ) ) 170 , 180 , 180
  170 ANG2 = 1.5 * PI
      GO TO 300
  180 ANG2 = 0.5 * PI
      GO TO 300
  140 IF ( Y ( J ) - Y ( I ) ) 190 , 200 , 300
  190 ANG2 = - ANG2 + 2. * PI
      GO TO 300
  200 ANG2 = 0.
  300 TANG = ABS ( ANG2 - ANG1 )
      RETURN
      END
      SUBROUTINE LIST ( NLI , NLO , CL )
      DIMENSION X ( 120 ) , Y ( 120 )
      COMMON / BCD / X , Y , IP  / DEF / DX , DY
      DATA DCL / 12345 . 54321 /
      DATA CLL , CLO / 12345. , 543321. /
      IF ( CL .EQ. 99999. ) GO TO 20
      IF ( CL .NE. DCL ) WRITE ( 6 , 100 ) CL
      DCL = CL
  100 FORMAT ( *1* /// 54X , *CONTOUR LEVEL =* , F8.3 // )
      IF ( NLI .EQ. 0 ) GO TO 10
      IF ( CLL .NE. CL ) WRITE ( 6 , 150 )
      CLL = CL
  150 FORMAT ( 5 X , *LINES*  )
      WRITE ( 6 , 220 ) IP , ( ( X ( I ) , Y ( I ) ) , I = 1 , IP )
  220 FORMAT ( / 5X , *IP =* , I5 , 8( 2X , 2F 6.2 ) /
     *         100 ( 14X , 8 ( 2X , 2 F 6.2 ) / )  )
      RETURN
   10 CONTINUE
      IF ( NLO .EQ. 0 ) RETURN
      IF ( CLO .NE. CL ) WRITE ( 6 , 230 )
      CLO = CL
  230 FORMAT ( // 5 X , *LOOPS*  )
      WRITE ( 6 , 220 ) IP , ( ( X ( I ) , Y ( I ) ) , I = 1 , IP )
      RETURN
   20 CONTINUE
      WRITE ( 6 , 300 )    NLI , NLO
  300 FORMAT ( //40X , *NUMBER OF LINES = * , I4 , 7X ,
     *                 *NUMBER OF LOOPS = * , I4  )
      RETURN
      E N D
      SUBROUTINE TRACE2 ( IX, JY, CL, IB, IP, IND )
      DIMENSION A(78,21), JO(78,21), JF(78,21), X(120), Y(120)
      COMMON /ABC/ NX, NY, A /JOINT/ JO, JF /BCD/ X, Y, IPDUM
      IND = 0  $  IP = 1  $  I = IX  $  J = JY
      GO TO ( 222, 333, 444, 111, 222 ), IB
C
C*****   UPWARD   *****
  111 IF ( JF(I,J) .EQ. 0 .AND. JF(I+1,J) .EQ. 0 ) GO TO 42
      IF ( JF(I,J) .EQ. 0 ) GO TO 11
      IF ( JF(I+1,J) .EQ. 0 ) GO TO 12
      IF ( A(I,J-1) .GT. CL ) GO TO 510
      IF ( A(I,J) .GT. CL ) GO TO 400
      IF ( A(I+1,J) .GT. CL ) GO TO 100
      GO TO 200
  510 IF ( A(I+1,J) .GT. CL ) GO TO 200
      IF ( A(I,J) .GT. CL ) GO TO 100
      GO TO 400
   11 IF ( A(I,J-1) .GT. CL ) GO TO 13
      IF ( A(I+1,J) .LT. CL ) GO TO 200
      GO TO 42
   13 IF ( A(I+1,J) .GT. CL ) GO TO 200
      GO TO 42
   12 IF ( A(I,J-1) .GT. CL ) GO TO 14
      IF ( A(I,J) .GT. CL ) GO TO 400
      GO TO 42
   14 IF ( A(I,J) .LT. CL ) GO TO 400
      GO TO 42
C
C*****   RIGHTWARD   *****
  222 IF ( JF(I+1,J) .EQ. 0 .AND. JF(I+1,J-1) .EQ. 0 ) GO TO 43
      IF ( JF(I+1,J) .EQ. 0 ) GO TO 21
      IF ( JF(I+1,J-1) .EQ. 0 ) GO TO 22
      IF ( A(I,J-1) .GT. CL ) GO TO 520
      IF ( A(I+1,J-1) .GT. CL ) GO TO 300
      IF ( A(I+1,J) .GT. CL ) GO TO 200
      GO TO 100
  520 IF ( A(I+1,J) .GT. CL ) GO TO 100
      IF ( A(I+1,J-1) .GT. CL ) GO TO 200
      GO TO 300
   21 IF ( A(I,J-1) .GT. CL ) GO TO 23
      IF ( A(I+1,J-1) .GT. CL ) GO TO 300
      GO TO 43
   23 IF ( A(I+1,J-1) .LT. CL ) GO TO 300
      GO TO 43
   22 IF ( A(I,J-1) .GT. CL ) GO TO 24
      IF ( A(I+1,J) .LT. CL ) GO TO 100
      GO TO 43
   24 IF ( A(I+1,J) .GT. CL ) GO TO 100
      GO TO 43
C
C*****   DOWNWARD   *****
  333 IF ( JF(I,J-1) .EQ. 0 .AND. JF(I+1,J-1) .EQ. 0 ) GO TO 44
      IF ( JF(I,J-1) .EQ. 0 ) GO TO 31
      IF ( JF(I+1,J-1) .EQ. 0 ) GO TO 32
      IF ( A(I+1,J) .GT. CL ) GO TO 530
      IF ( A(I+1,J-1) .GT. CL ) GO TO 200
      IF ( A(I,J-1) .GT. CL ) GO TO 300
      GO TO 400
  530 IF ( A(I,J-1) .GT. CL ) GO TO 400
      IF ( A(I+1,J-1) .GT.CL ) GO TO 300
      GO TO 200
   31 IF ( A(I,J) .GT. CL ) GO TO 33
      IF ( A(I+1,J-1) .LT. CL ) GO TO 200
      GO TO 44
   33 IF ( A(I+1,J-1) .GT. CL ) GO TO 200
      GO TO 44
   32 IF ( A(I,J) .GT. CL ) GO TO 34
      IF ( A(I,J-1) .GT. CL ) GO TO 400
      GO TO 44
   34 IF ( A(I,J-1) .LT. CL ) GO TO 400
      GO TO 44
C
C*****   LEFTWARD   *****
  444 IF ( JF(I,J) .EQ. 0 .AND. JF(I,J-1) .EQ. 0 ) GO TO 41
      IF ( JF(I,J) .EQ. 0 ) GO TO 401
      IF ( JF(I,J-1) .EQ. 0 ) GO TO 402
      IF ( A(I+1,J) .GT. CL ) GO TO 540
      IF ( A(I,J) .GT. CL ) GO TO 100
      IF ( A(I,J-1) .GT.CL ) GO TO 400
      GO TO 300
  540 IF ( A(I,J-1) .GT.CL ) GO TO 300
      IF ( A(I,J) .GT. CL ) GO TO 400
      GO TO 100
  401 IF ( A(I+1,J) .GT. CL ) GO TO 403
      IF ( A(I,J-1) .LT. CL ) GO TO 300
      GO TO 41
  403 IF ( A(I,J-1) .GT. CL ) GO TO 300
      GO TO 41
  402 IF ( A(I+1,J) .GT. CL ) GO TO 404
      IF ( A(I,J) .GT. CL ) GO TO 100
      GO TO 41
  404 IF ( A(I,J) .LT. CL ) GO TO 100
      GO TO 41
C
C
  100 IP = IP + 1
      Y(IP) = J - 1
      X(IP) = I-1 + ( CL - A(I,J) ) / ( A(I+1,J) - A(I,J) )
      IF ( IB.EQ.5 .AND. X(1).EQ.X(IP) .AND. Y(1).EQ.Y(IP) ) GO TO 600
      IF( X(1) .EQ. X(IP) .AND. Y(1) .EQ. Y(IP) ) GO TO 42
      IF ( J .EQ. NY ) GO TO 42
      J = J + 1
      GO TO 111
C
  200 IP = IP + 1
      X(IP) = I
      Y(IP) = J-2 + ( CL - A(I+1,J-1) ) / ( A(I+1,J) - A(I+1,J-1) )
      IF ( IB.EQ.5 .AND. X(1).EQ.X(IP) .AND. Y(1).EQ.Y(IP) ) GO TO 600
      IF ( X(1) .EQ. X(IP) .AND. Y(1) .EQ. Y(IP) ) GO TO 43
      IF ( I .EQ. (NX-1) ) GO TO 43
      I = I + 1
      GO TO 222
C
  300 IP = IP + 1
      Y(IP) = J - 2
      X(IP) = I-1 + ( CL - A(I,J-1) ) / ( A(I+1,J-1) - A(I,J-1) )
      IF ( IB.EQ.5 .AND. X(1).EQ.X(IP) .AND. Y(1).EQ.Y(IP) ) GO TO 600
      IF ( X(1) .EQ. X(IP) .AND. Y(1) .EQ. Y(IP) ) GO TO 44
      IF ( J .EQ. 2 ) GO TO 44
      J = J - 1
      GO TO 333
C
  400 IP = IP + 1
      X(IP) = I - 1
      Y(IP) = J-2 + ( CL - A(I,J-1) ) / ( A(I,J) - A(I,J-1) )
      IF ( IB.EQ.5 .AND. X(1).EQ.X(IP) .AND. Y(1).EQ.Y(IP) ) GO TO 600
      IF ( X(1) .EQ. X(IP) .AND. Y(1) .EQ. Y(IP) ) GO TO 41
      IF ( I .EQ. 1 ) GO TO 41
      I = I - 1
      GO TO 444
  600 IND = 2
      RETURN
C
   41 IF ( IB .GT. 1 .OR. IP .EQ. 1 ) RETURN
      IF ( Y(1) .GT. Y(IP) ) RETURN
      GO TO 700
   42 IF ( IB .GT. 2 .OR. IP .EQ. 1 ) RETURN
      IF ( X(1) .GT. X(IP) ) RETURN
      GO TO 700
   43 IF ( IB .GT. 3 .OR. IP .EQ. 1) RETURN
      IF ( Y(1) .GT. Y(IP) .AND. IB .EQ. 3 ) RETURN
      GO TO 700
   44 IF ( IB .EQ. 5 .OR. IP .EQ. 1 ) RETURN
      IF ( X(1) .GT. X(IP) .AND. IB .EQ. 4 ) RETURN
  700 IND = 1
      RETURN
      E  N  D
      SUBROUTINE MIDASH ( NLETT,MM, HR, SIZE, NX , NY, WSP, ICODE )
      DIMENSION MM ( 4 ), BOUNDX ( 229 ), BOUNDY ( 229 ), BISLX1 ( 7 ),
     *          BISLY1 ( 7 ), BISLX2 ( 9 ), BISLY2 ( 9 ), BISLX3 ( 9 ),
     *          BISLY3 ( 9 ), DATE ( 3 )
      DIMENSION  IA ( 3 , 5 ), XA ( 8 ), YA ( 8 ), XAD ( 8 ), YAD ( 8 )
      COMMON / DEF / DX , DY
      COMMON / MDATE / DATE
      COMMON / FGH / STX , STY , ISY , ANGLE , HEIGHT
      COMMON /HIJ/ DDX, DDY
      COMMON / SHAPE / BOUNDX, BOUNDY, BISLX1, BISLY1, BISLX2,
     *                 BISLY2, BISLX3, BISLY3
      DATA XA / 0., -0.375, -0.375, -1.0, -1.0, -0.375, -0.375, 0. /
      DATA YA / 0., 0.09, 0.062, 0.062, -0.062, -0.062, -0.09, 0. /
      DATA ( IA ( 1 , I ) , I = 1 , 5 ) / 88,49,40, 76,41/
      DATA ( IA ( 2 , I ) , I = 1 , 5 ) / 88, 103, 40,  76, 41 /
      DATA ( IA ( 3 , I ) , I = 1 , 5 ) / 88, 103, 40,  76, 41 /
      F ( X ) = ABS ( X ) / X * ( ABS ( X ) * 1000. + 0.5 )
C
      NX = NX + 6
      NY = NY + 4
C
      DDX = DX * SIZE
      DDY = DY * SIZE
      HE = HEIGHT / 9.
      SKI = 0.1 * HE
      DRO = 0.2 * HE
      CHO = 0.15 * HE
      BRO = SKI + DRO
      RATIO = 0.98
      CH = 0.3 * HE
      CH1 = 0.20 * HE
      CH2 = 0.20 * HE
      PX = - 0.75 * HE
      PY = - STY / DY * DDY
      IF ( PY .LT. 0. ) PY = 0.
      YMAX = SIZE * ( NY - 1 ) * DY
      IF ( PY .GT. YMAX ) PY = YMAX
      CALL PLOT ( 0. , 0. , 3 )
      CALL PLOT ( -3.*DDX , -2.0*DDY , -3 )
      CALL FACTOR ( 1 . 0 )
C     STXX= DX*FLOAT(NX-1)*SIZE/2.-FLOAT(NLETT)/2.*CH*RATIO
      STXX = 18. * DDX
      YS = 22.6 * DDY
      IF ( STXX.LT. 0 ) STXX= 0.
      XMAX = FLOAT ( NX - 1 ) * DX * SIZE
      TCLEN = FLOAT ( NLETT ) * CH * RATIO
      IF ( TCLEN .LE. XMAX ) GO TO 5
      CH = XMAX / ( FLOAT ( NLETT ) * RATIO )
    5 CONTINUE
C     CALL SYMBOL ( STXX , HEIGHT+0.5*HE , CH , MM , 0. , NLETT )
      CALL SYMBOL (STXX, YS, CH, MM, 0., NLETT )
      CALL NUMBER (STXX+FLOAT(NLETT)*CH*RATIO,YS,CH,HR,0,1)
      CALL SYMBOL(STXX+FLOAT(NLETT+5)*CH*RATIO,YS,CH,5HHOURS,0,5)
      CALL SYMBOL(STXX+FLOAT(NLETT+12)*CH*RATIO,22.3*DDY,1.2*CH,40,0,-1)
      CALL SYMBOL(STXX+FLOAT(NLETT+13)*CH*RATIO,YS,CH,DATE,0,30 )
      CALL SYMBOL(STXX+FLOAT(NLETT+36)*CH*RATIO,22.3*DDY,1.2*CH,41,0,-1)
C
      X = 0.
      DO 10 I = 1 , NX
      XX = FLOAT ( I - 1 ) * DX + STX
      XX = XX / 2.0
      IIIX = XX
      IF ( XX .NE. 0. ) GO TO 6
      IC = 1
      GO TO 7
    6 CONTINUE
      K = F ( XX )
      IF ( K .NE. K / 1000 * 1000 ) GO TO 11
      IC = ALOG 10 ( ABS ( XX ) ) + 1.00001
      IF ( XX .LT. 0. ) IC = IC + 1
    7 CONTINUE
      CNAGA = FLOAT ( IC ) * CH1 * 0.98
      XR = X - 0.5 * CNAGA + CH1 * 0.3 * 0.98
      YR = - BRO - CH1
      IF ( (XX-IIIX) .GT. 0. .OR. XX .LT. 0. ) GO TO 1111
      CALL NUMBER ( XR , YR , CH1 , XX , 0. , - 1 )
 1111 CALL PLOT ( X , - SKI , 3 )
      CALL PLOT ( X , - DRO , 2 )
      GO TO 12
   11 CONTINUE
      CALL PLOT ( X , - SKI , 3 )
      CALL PLOT ( X , - CHO , 2 )
   12 CONTINUE
      X = X + DDX
   10 CONTINUE
      X = X - DDX
      Y = 0.
      DO 20 I = 1 , NY
      YY = FLOAT ( I - 1 ) * DY + STY
      IF ( YY .EQ. 0. ) GO TO 24
      K = F ( YY )
      IF ( K .NE. K / 1000 * 1000 ) GO TO 25
   24 CONTINUE
      CALL PLOT ( X + SKI , Y , 3 )
      CALL PLOT ( X + DRO , Y , 2 )
      GO TO 26
   25 CONTINUE
      CALL PLOT ( X + SKI , Y , 3 )
      CALL PLOT ( X + CHO , Y , 2 )
   26 CONTINUE
      Y = Y + DDY
   20 CONTINUE
      X = 0.
      Y = Y - DDY
      DO 30 I = 1 , NX
      XX = FLOAT ( I - 1 ) * DX + STX
      IF ( XX .EQ. 0. ) GO TO 32
      K = F ( XX )
      IF ( K .NE. K / 1000 * 1000 ) GO TO 33
   32 CONTINUE
      CALL PLOT ( X , Y + SKI , 3 )
      CALL PLOT ( X , Y + DRO , 2 )
      GO TO 34
   33 CONTINUE
      CALL PLOT ( X , Y + SKI , 3 )
      CALL PLOT ( X , Y + CHO , 2 )
   34 CONTINUE
      X = X + DDX
   30 CONTINUE
      X = 0.
      Y = 0.
      DO 40 I = 1 , NY
      YY = FLOAT ( I - 1 ) * DY + STY
      YY = YY / 2.0
      IIIY = YY
      IF ( YY .NE. 0. ) GO TO 16
      IC = 1
      GO TO 17
   16 CONTINUE
      K = F ( YY )
      IF ( K .NE. K / 1000 * 1000 ) GO TO 22
      IC = ALOG 10 ( ABS ( YY ) ) + 1.00001
      IF ( YY .LT. 0. ) IC = IC + 1
   17 CONTINUE
      CNAGA = FLOAT ( IC ) * CH1 * 0.98
      XR = - CNAGA - BRO
      YR = Y - 0.5 * CH1
      IF ( (YY-IIIY) .GT. 0. .OR. YY .LT. 0. ) GO TO 2222
      CALL NUMBER ( XR , YR , CH1 , YY , 0. , - 1 )
 2222 CALL PLOT ( X - SKI , Y , 3 )
      CALL PLOT ( X - DRO , Y , 2 )
      GO TO 44
   22 CONTINUE
      CALL PLOT ( X - SKI , Y , 3 )
      CALL PLOT ( X - CHO , Y , 2 )
   44 CONTINUE
      Y = Y + DDY
   40 CONTINUE
      CALL PLOT ( - SKI , -SKI , 3 )
      X = FLOAT ( NX - 1 ) * DDX + SKI
      CALL PLOT ( X , -SKI , 2 )
      Y = FLOAT ( NY - 1 ) * DDY + SKI
      CALL PLOT ( X , Y , 2 )
      CALL PLOT ( -SKI , Y , 2 )
      CALL PLOT ( -SKI , -SKI , 2 )
      XR = ( NX - 1 ) * DDX / 2. - CH2 * RATIO * 5. / 2.
      YR = - 1 . * HE
      DO 50 I = 1 , 5
      CALL SYMBOL ( XR , YR , CH2 , IA ( ISY , I ) , 0. , - 1 )
   50 XR = XR + CH2 * RATIO
      YR = ( NY - 1 ) * DDY * 0.5
      XR = - 1.5 * HE + CH2 * RATIO + .5*DDX
      IF ( ISY .EQ. 1 ) XR = XR - CH2 * RATIO / 2.
      CALL SYMBOL ( XR , YR , CH2 , 89 , 0. , - 1 )
      XR = XR + CH2 * RATIO
      IF ( ISY .EQ. 1 ) CALL SYMBOL ( XR , YR , CH2 , 49 , 0. , - 1 )
      XR = - 1.5 * HE + .5*DDX
      YR = YR - 2.0 * CH2
      DO 60 I = 3 , 5
      CALL SYMBOL ( XR , YR , CH2 , IA ( ISY , I ) , 0. , - 1 )
   60 XR = XR + CH2 * RATIO
      CALL PLOT ( 0. , 0. , 3 )
      CALL PLOT ( 3.*DDX , 2.0*DDY , -3 )
C
C
      BOUNDX (228) = 0.
      BOUNDY (228) = 0.
      BOUNDX (229) = 1./DDX
      BOUNDY (229) = 1./DDY
      BISLX1 (6) = 0.
      BISLY1 (6) = 0.
      BISLX1 (7) = 1./DDX
      BISLY1 (7) = 1./DDY
      BISLX2 (8) = 0.
      BISLY2 (8) = 0.
      BISLX2 (9) = 1./DDX
      BISLY2 (9) = 1./DDY
      BISLX3 (8) = 0.
      BISLY3 (8) = 0.
      BISLX3 (9) = 1./DDX
      BISLY3 (9) = 1./DDY
C
      CALL LINE ( BOUNDX, BOUNDY, 227, 1, 0, 0 )
      CALL LINE ( BISLX1, BISLY1, 5, 1, 0, 0 )
      CALL LINE ( BISLX2, BISLY2, 7, 1, 0, 0 )
      CALL LINE ( BISLX3, BISLY3, 7, 1, 0, 0 )
      OX = 70.5*DDX $ OY = 14.5*DDY
      AL = 4.2*DDX $ AEX = AL*.39875 $ AEY = AL*.91706
      AEXA = AEX+OX $ AEYA = AEY+OY
      CALL PLOT( OX, OY, 3 )
      CALL PLOT(AEXA,AEYA,2)
      CALL ARHEAD(AEX,AEY,AX1,AY1,AX2,AY2,0.66,0.9397)
      AX11=AX1+AEXA $ AY11=AY1+AEYA
      AX22=AX2+AEXA $ AY22=AY2+AEYA
      CALL PLOT(AX11,AY11,2) $ CALL PLOT(AEXA,AEYA,3)
      CALL PLOT(AX22,AY22,2)
      CALL SYMBOL(AEXA,AEYA+.5*DDY,0.12,78,0,-1)
      CALL SYMBOL(AEXA+.09*DDX,AEYA+.5*DDY,0.12,78,0,-1)
      CALL SYMBOL (-2.9*DDX,13.*DDY,0.14,6HTOLEDO,0,6)
      CALL SYMBOL (-1.*DDX,15.5*DDY,0.21,1,0.,-1)
      CALL SYMBOL (26.*DDX,-1.2*DDY,0.14,9HCLEVELAND,0,9)
      CALL SYMBOL (23.*DDX,-1.*DDY,0.21,1,0.,-1)
      CALL SYMBOL (75.0*DDX,11.5*DDY,0.14,7HBUFFALO,0,7)
      CALL SYMBOL (78.*DDX,10.*DDY,0.21,1,0.,-1)
      CALL PLOT ( 0. , 0. , 3 )
      CALL PLOT ( 2.0*DDX, 0., 2 )
      CALL SYMBOL (0.,0.,0.07,13,0.,-1)
      CALL SYMBOL (2.0*DDX,0.,0.07,13,0.,-1)
      CALL SYMBOL (2.5*DDX,-0.3*DDY,0.14,5H10 KM,0.,5)
      IF (ICODE.NE.5.AND.ICODE.NE.2) CALL PLOT(0.5*DDX, 0.5*DDY, -3)
      RETURN
      E N D
      SUBROUTINE PVPLOT ( ICODE )
      DIMENSION U(78,21), V(78,21), A(78,21), JO(78,21),
     *          JOFN(78,21)
      COMMON / JOINT / JO , JOFN
      COMMON / ABC / NX , NY , A
      COMMON / SPEEDY / U , V
      COMMON / HIJ / DX , DY
C
      CALL PLOT ( 0. , 0. , -3 )
      IF ( ICODE .EQ. 2 ) GO TO 10
      PI = 4. * ATAN(1.)
      CC = 0.66
      DDD = COS ( 20.*PI/180. )
      DO 50 I = 2 , NX
      DO 50 J = 2 , NY
      IF ( JO(I,J) .NE. 1 ) GO TO 50
      XNODE = ( I - 1 ) * DX
      YNODE = ( J - 1 ) * DY
      CALL PLOT ( XNODE , YNODE , 3 )
      UU = U ( I , J ) * 1.2
      VV = V ( I , J ) * 1.2
      CALL ARHEAD ( UU, VV, AX1, AY1, AX2, AY2, CC, DDD )
      XU = UU + XNODE
      YV = VV + YNODE
      ARX1 = XU + AX1
      ARX2 = XU + AX2
      ARY1 = YV + AY1
      ARY2 = YV + AY2
      CALL PLOT ( XU , YV , 2 )
      CALL PLOT ( ARX1 , ARY1 , 2 )
      CALL PLOT ( XU , YV , 3 )
      CALL PLOT ( ARX2 , ARY2 , 2 )
   50 CONTINUE
      OX = 68.*DX $ OY = 3.0*DY
      OX1 = OX+0.12 $ CALL PLOT(OX,OY,3)
      CALL PLOT (OX1,OY,2)
      CALL ARHEAD(0.12,0.,A1,B1,A2,B2,CC,DDD)
      CALL PLOT(OX1+A1,OY+B1,2)
      CALL PLOT(OX1,OY,3)
      CALL PLOT(OX1+A2,OY+B2,2)
      CALL SYMBOL(OX1+.05,OY-.055,.11,12H= 10 CM/SEC.,0,12)
      RETURN
   10 DO 60 I = 1 , NX
      DO 60 J = 1 , NY
      IF ( JOFN(I,J) .EQ. 0 ) GO TO 60
      XC = DX * ( FLOAT(I) - 0.5 )
      YC = DY * ( FLOAT(J) - 0.5 )
      SFACT = A ( I , J ) * U ( I , J ) / 50000.
      IF ( SFACT .LT. 0.007 ) SFACT = 0.007
      CALL SYMBOL ( XC , YC , SFACT , 11 , 0. , -1 )
   60 CONTINUE
      RETURN
      END
      SUBROUTINE CHECK2 ( CL )
      DIMENSION A(78,21), JO(78,21), JOFN(78,21)
      COMMON /ABC/ NX, NY, A
      COMMON /JOINT/ JO, JOFN
      DATA EPS /1.0E-08/
      NY1 = NY - 1
      DO 40 I = 1 , NX
      IF ( JOFN(I,1) .NE. 0 .AND. A(I,1) .EQ. CL ) GO TO 10
      GO TO 15
   10 IF ( A(I,2) .GT. CL ) A(I,1) = A(I,1) - EPS
      IF ( A(I,2) .LE. CL ) A(I,1) = A(I,1) + EPS
   15 CONTINUE
      DO 45 J = 2 , NY1
      IF ( JOFN(I,J) .EQ. 0 .OR. JOFN(I,J-1) .EQ. 0 .OR.
     *     JOFN(I,J+1) .EQ. 0 ) GO TO 30
      IF ( A(I,J) .NE. CL ) GO TO 45
      IF ( A(I,J-1) .GT. CL .AND. A(I,J+1) .GE. CL ) GO TO 20
      A(I,J) = A(I,J) + EPS
      GO TO 45
   20 A(I,J) = A(I,J) - EPS
      GO TO 45
   30 IF ( JOFN(I,J-1) .EQ. 0 .AND. JOFN(I,J) .NE. 0 .AND.
     *     JOFN(I,J+1) .NE. 0 .AND. A(I,J) .EQ. CL ) GO TO 31
      IF ( JOFN(I,J+1) .EQ. 0 .AND. JOFN(I,J) .NE. 0 .AND.
     *     JOFN(I,J-1) .NE. 0 .AND. A(I,J) .EQ. CL ) GO TO 32
      GO TO 45
   31 IF ( A(I,J+1) .GT. CL ) A(I,J) = A(I,J) - EPS
      IF ( A(I,J+1) .LE. CL ) A(I,J) = A(I,J) + EPS
      GO TO 45
   32 IF ( A(I,J-1) .GT. CL ) A(I,J) = A(I,J) - EPS
      IF ( A(I,J-1) .LE. CL ) A(I,J) = A(I,J) + EPS
   45 CONTINUE
      IF ( JOFN(I,NY) .NE. 0 .AND. A(I,NY) .EQ. CL ) GO TO 50
      GO TO 40
   50 IF ( A(I,NY1) .GT. CL ) A(I,NY) = A(I,NY) - EPS
      IF ( A(I,NY1) .LE. CL ) A(I,NY) = A(I,NY) + EPS
   40 CONTINUE
      RETURN
      END
      SUBROUTINE FINDER2 ( NLI, NLO, CL )
      DIMENSION A (78,21), JO (78,21), JOFN (78,21)
      DIMENSION X (120), Y (120)
      COMMON /ABC/ NX, NY, A /BCD/ X, Y, IP
      COMMON /JOINT/ JO, JOFN
      CALL CHECK2 (CL)
      NX1 = NX - 1
      NY1 = NY - 1
      NLO = 0
      NLI = 0
      DO 31 I = 1 , NX1
      DO 31 J = 2 , NY
      IF ( I .EQ. 1 ) GO TO 41
      IF ( JOFN(I-1,J) .NE. 0 .AND. JOFN(I-1,J-1) .NE. 0 ) GO TO 31
   41 IF ( JOFN(I,J) .EQ. 0 .OR. JOFN(I,J-1) .EQ. 0 ) GO TO 31
      IF ( A(I,J-1) .GE. CL ) GO TO 11
      IF ( A(I,J) .LT. CL ) GO TO 31
      GO TO 21
   11 IF ( A(I,J) .GE. CL ) GO TO 31
   21 CALL SEARCH2 ( I, J, CL, 1, NLI )
   31 CONTINUE
      DO 32 JJ = 1 , NY1
      J = NY - JJ + 1
      DO 32 I = 1 , NX1
      IF ( J .EQ. NY ) GO TO 42
      IF ( JOFN(I,J+1) .NE. 0 .AND. JOFN(I+1,J+1) .NE. 0 ) GO TO 32
   42 IF ( JOFN(I+1,J) .EQ. 0 .OR. JOFN(I,J) .EQ. 0 ) GO TO 32
      IF ( A(I,J) .GE. CL ) GO TO 12
      IF ( A(I+1,J) .LT. CL ) GO TO 32
      GO TO 22
   12 IF ( A(I+1,J) .GT. CL ) GO TO 32
   22 CALL SEARCH2 ( I, J, CL, 2, NLI )
   32 CONTINUE
      DO 33 II = 1 , NX1
      I = NX - II + 1
      DO 33 J = 2 , NY
      IF ( I .EQ. NX ) GO TO 43
      IF ( JOFN(I+1,J) .NE. 0 .AND. JOFN(I+1,J-1) .NE. 0 ) GO TO 33
   43 IF ( JOFN(I,J) .EQ. 0 .OR. JOFN(I,J-1) .EQ. 0 ) GO TO 33
      IF ( A(I,J-1) .GE. CL ) GO TO 13
      IF ( A(I,J) .LT. CL ) GO TO 33
      GO TO 23
   13 IF ( A(I,J) .GE. CL ) GO TO 33
   23 CALL SEARCH2 (I-1, J, CL, 3, NLI )
   33 CONTINUE
      DO 34 J = 1 , NY1
      DO 34 I = 1 , NX1
      IF ( J .EQ. 1 ) GO TO 44
      IF ( JOFN(I,J-1) .NE. 0 .AND. JOFN(I+1,J-1) .NE. 0 ) GO TO 34
   44 IF ( JOFN(I+1,J) .EQ. 0 .OR. JOFN(I,J) .EQ. 0 ) GO TO 34
      IF ( A(I,J) .GE. CL ) GO TO 14
      IF ( A(I+1,J) .LT. CL ) GO TO 34
      GO TO 24
   14 IF ( A(I+1,J) .GT. CL ) GO TO 34
   24 CALL SEARCH2 ( I, J+1, CL, 4, NLI )
   34 CONTINUE
      DO 35 I = 2 , NX1
      DO 35 J = 2 , NY1
      IF ( JOFN(I,J) .EQ. 0 .OR. JOFN(I,J-1) .EQ. 0 ) GO TO 35
      IF ( JOFN(I-1,J) .EQ. 0 .OR. JOFN(I-1,J-1) .EQ. 0 ) GO TO 35
      IF ( JOFN(I+1,J) .EQ. 0 .AND. JOFN(I+1,J-1) .EQ. 0 ) GO TO 35
      IF ( A(I,J-1) .GE. CL ) GO TO 15
      IF ( A(I,J) .LT. CL ) GO TO 35
      GO TO 25
   15 IF ( A(I,J) .GE. CL ) GO TO 35
   25 CALL SEARCH2 ( I, J, CL, 5, NLO )
   35 CONTINUE
      RETURN
      END
