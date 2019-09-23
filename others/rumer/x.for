C
C
C*****************************************************************************
C
C      GREAT LAKES ICE DYNAMICS SIMULATION MODEL  VERSION-3  (GLIDS-III)
C
C               STATE UNIVERSITY OF NEW YORK AT BUFFALO
C                   DEPARTMENT OF CIVIL ENGINEERING
C
C      DEVELOPED BY :    RALPH R. RUMER JR.
C                        AKIO  WAKE
C                        SHIH-HUANG  CHIEH
C                        EIJI  FUKUMORI
C
C************************** OCTOBER  1980 ************************************
C MODIFIED BY RAYMOND A. ASSEL TO RUN ON VAX 11/780
C                    NOAA/ERL/GLERL
C                    2205 COMMENWEALTH BLVD
C                    ANN ARBOR, MICH 48105C
C************************ MAY 1990 *******************************************
C      LIST OF VARIABLES AND COEFFICIENTS
C
C      M       : NUMBER OF NODE POINTS , IN Y DIRECTION
C      N       : NUMBER OF NODE POINTS , IN X DIRECTION
C      DT      : TIME STEP (SECONDS)
C      DL      : NODE SPACING (METERS)
C      YIDO    : LATITUDE (DEGREES)
C      FK1     : ICE SHEAR VISCOSITY
C      FK2     : ICE BULK VISCOSITY
C      PST     : EMPIRICAL COEFFICIENT FOR ICE PRESSURE CALCULATION
C      TI      : REFERENCE ICE THICKNESS (METERS)
C      DIVLIM  : DIVERGENCE CRITERION
C      K       : EMPIRICAL COEFFICIENT FOR ICE PRESSURE CALCULATION
C      TMAX    : TIME PERIOD OF SIMULATION (SECONDS)
C      PRINTM  : TIME INTERVAL BETWEEN PRINTOUTS (SECONDS)
C      WINDINT : TIME INTERVAL BETWEEN WIND CONDITION CHANGES (SECONDS)
C      DISKINT : TIME INTERVAL BETWEEN DISK RECORDINGS (SECONDS)
C      IDISK   : MAGNETIC DISK RECORDING INDEX
C      ICONT   : RESTART INDEX
C      OVERWRT : MAGNETIC DISK OVERWRITING INDEX
C      PTIME   : RESTART TIME
C      JO      : BOUNDARY CONFIGURATION INDEX FOR VELOCITY CALCULATION
C      JOFN    : BOUNDARY CONFIGURATION INDEX FOR CONCENTRATION CALCULATION
C      U, V    : ICE VELOCITIES (METERS/SECOND)
C      WX, WY  : WATER CURRENTS (METERS/SECOND)
C      P       : INTERNAL ICE PRESSURE (NEWTONS/SQ.METER)
C      DIV     : ICE MASS FLUX DIVERGENCE (KG/SQ.METER-SECOND)
C      CN      : AREAL ICE CONCENTRATION (DIMENSIONLESS)
C      TICE    : ICE THICKNESS (METERS)
C      WIN (1) : WIND SPEED AT TOLEDO (KNOTS)
C      WIN (2) : WIND SPEED AT CLEVELAND (KNOTS)
C      WIN (3) : WIND SPEED AT BUFFALO (KNOTS)
C      WAN (1) : WIND ANGLE AT TOLEDO (DEGREE)
C      WAN (2) : WIND ANGLE AT CLEVELAND (DEGREE)
C      WAN (3) : WIND ANGLE AT BUFFALO (DEGREE)
C      TEMP(1) : AIR TEMPERATURE AT TOLEDO (DEGREE F)
C      TEMP(2) : AIR TEMPERATURE AT CLEVELAND (DEGREE F)
C      TEMP(3) : AIR TEMPERATURE AT BUFFALO (DEGREE F)
C      RHOICE  : ICE DENSITY (KG/M**3)
C      C1      : EQUAL TO ( DENSITY OF AIR ) * ( CA )
C      C2      : EQUAL TO ( DENSITY OF WATER ) * ( CW )
C      C3      : CORIOLIS PARAMETER (SEC**-1)
C      DIFFU   : ARTIFICIAL DAMPING COEFFICIENT IN X DIRECTION
C      DIFFV   : ARTIFICIAL DAMPING COEFFICIENT IN Y DIRECTION
C      DUT     : U VELOCITY CHANGE IN ONE TIME STEP
C      DVT     : V VELOCITY CHANGE IN ONE TIME STEP
C      DNT     : CONCENTRATION CHANGE IN ONE TIME STEP
C      DTT     : THICKNESS CHANGE IN ONE TIME STEP
C      SIGMAX  : X COMPONENT NORMAL STRESS (NEWTONS/SQ.METER)
C      SIGMAY  : Y COMPONENT NORMAL STRESS (NEWTONS/SQ.METER)
C      SIGMAXY : SHEAR STRESS (NEWTONS/SQ.METER)
C
C      LIST OF SUBROUTINES
C
C      SUBROUTINE BOUNDS1 : SETTING NO-SLIP BOUNDARY CONDITIONS FOR EQNS. OF
C                           MOTION
C      SUBROUTINE BOUNDS2 : SETTING SLIP BOUNDARY CONDITIONS FOR EQNS. OF
C                           MOTION
C      SUBROUTINE WINCOMP : CALCULATION OF WIND VELOCITY FIELD OVER LAKE ERIE
C      SUBROUTINE UVCOMP  : ICE VELOCITY CALCULATION
C      SUBROUTINE CNCOMP  : ICE THICKNESS , CONCENTRATION AND MASS FLUX
C                           DIVERGENCE CALCULATION
C      SUBROUTINE WRITEX  : PRINTING AND RECORDING OUTPUT
C      SUBROUTINE RESTART : READ I.C. FROM PREVIOUS RESULT OR UPDATE I.C.
C      SUBROUTINE PRINTER : GRAPHICAL PRINTING
C      SUBROUTINE THERMAL : CALCULATION OF THERMAL EFFECT
C      SUBROUTINE CHECK   : CALCULATION OF TOTAL ICE VOLUME AND ICE AREA
C
C*****************************************************************************
C
      character infile*79,outfile*79
C
      DIMENSION U(78,21), V(78,21), CN(78,21), DUT(78,21), DVT(78,21)
     *       , DNT(78,21), P(78,21), JO(78,21), JOFN(78,21)
     *       , TICE(78,21), DTT(78,21), DIV(78,21)
     *       , WIN(3), WAN(3), TEMP(3)
C
      COMMON / MASS / CN
      COMMON / DIVER / DIV
      COMMON / THICK / TICE
      COMMON / DELTCN / DNT , DTT
      COMMON / PRESUR / P
      COMMON / SPEEDY / U , V
      COMMON / DELTUV / DUT , DVT
      COMMON / JOINT / JO , JOFN
      COMMON / CONSTAN / DL , DT , ETA
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / WIND / WIN, WAN, TEMP
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / WRITES / IDISK, KOUNT, ICONT, OVERWRT, YIDO
      COMMON / WINDEX / PRINTM , WINDINT , DISKINT , IRATIO , IW ,
     *                  IPR , MELT
      COMMON / PCONSTS / PST, TI, DIVLIM, K
C
      DATA RHOICE, KOUNT, T, INDEX / 920., 0, 0., 1 /
      DATA TRANS1, TRANS2, TRANS3 / 600., 1200., 10800. /
      DATA U , V , CN , DUT , DVT , DNT , DTT / 11466 * 0. /
      DATA DIV, P / 3276 * 0. /
      DATA TICE / 1638 * 0. /

      open(unit=4,file='setup.file',readonly,status='old')
      read(4,210)infile
      read(4,210)outfile
  210 format(1x,a79)
      open(unit=5,file=infile,readonly, status='old')
      open(unit=6,file=outfile,status='new')
C
      READ ( 5 , 111 ) M , N
      NM1 = N - 1
      MM1 = M - 1
      READ ( 5 , 102 )  DL , YIDO , FK2MIN , FK2MAX
      READ ( 5 , 101 ) PST, TI, DIVLIM, K
      READ ( 5 , 102 ) TMAX , PRINTM , WINDINT , DISKINT
      READ ( 5 , 103 ) IDISK , ICONT , OVERWRT , MELT , PTIME
      DO 50 I = 1 , N
50    READ ( 5 , 104 ) ( JO(I,J), J = 1 , M )
      DO 51 I = 1 , NM1
51    READ ( 5 , 110 ) ( JOFN(I,J), J = 1 , MM1 )
      DO 52 I = 1 , NM1
52    READ ( 5 , 105 ) ( CN(I,J), J = 1 , MM1 )
      DO 53 I = 1 , NM1
53    READ ( 5 , 105 ) ( TICE(I,J), J = 1 , MM1 )
      READ ( 5 , 112 ) (( WIN(I), WAN(I), TEMP(I)), I = 1, 3 )
      WRITE ( 6, 107 )
      DO 54 I = 1, N
54    WRITE ( 6, 109 ) ( JO(I,J), J = 1 , M )
      WRITE ( 6, 108 )
      DO 55 I = 1, NM1
55    WRITE ( 6, 109 ) ( JOFN(I,J), J = 1 , MM1 )
C
      PI = 4. * ATAN ( 1. )
      C1 = 2.19 E -06 * RHOICE
      C2 = 9.98 E -03 * RHOICE
      C3 = 2. * ( 2. * PI / 86400. ) * SIN ( YIDO * PI / 180. )
C
      IF ( ICONT .EQ. 3HYES ) CALL RESTART ( T, PTIME, INDEX, OVERWRT )
      IF ( INDEX .NE. 1 ) GO TO 1000
C
      GAMMA = 1. / ( RHOICE * 2. * DL )
      EPS = 1. / ( 2. * DL )
      RRDLL = 1. / ( RHOICE * DL * DL )
      RRDLL4 = 0.25 * RRDLL
C
      WRITE ( 6, 106 ) M, N, MELT, DL, C1, C2, FK2MIN, FK2MAX,
     *   YIDO, PST, C3, K, RHOICE, DIVLIM, TMAX, PRINTM, DISKINT,
     *   WINDINT, IDISK, ICONT, OVERWRT, PTIME
      CALL WINCOMP ( T , DTX )
      WT = 0.0
      CALL WRITEX ( T )
C
      DO 9999 ITER = 1 , 5000
      IT = T
      IF ( IT .EQ. 0. .OR. IT/IW*IW .NE. IT ) GO TO 10
      READ ( 5 , 112 ) (( WIN(I), WAN(I), TEMP(I)), I = 1, 3 )
      CALL WINCOMP ( T , DTX )
      WT = 0.0
   10 IF ( MELT .NE. 3HYES ) GO TO 15
      IF ( WT .LT. TRANS1 ) GO TO 11
      IF ( WT .LT. TRANS2 ) GO TO 12
      IF ( WT .LT. TRANS3 ) GO TO 13
   15 DT = DTX
      GO TO 14
   11 DT = DTX / 3.0
      GO TO 14
   12 DT = DTX / 2.0
      GO TO 14
   13 DT = DTX / 1.5
   14 WT = WT + DT
      ETA = DT / DL
      T = T + DT
C
C     COMPUTE INTERNAL ICE PRESSURE
C
      DO 100 I = 1 , NM1
      DO 100 J = 1 , MM1
      IF ( JOFN(I,J) .EQ. 0 ) GO TO 100
      IF ( CN(I,J) .LT. 0.01 ) GO TO 20
      P(I,J) = PST * ( TICE(I,J)/TI ) * CN(I,J) ** K
      IF ( DIV(I,J) .GE. DIVLIM ) P(I,J) = 0.
20    IF ( CN(I,J) .LT. 0.01 ) P(I,J) = 0.
100   CONTINUE
C
      CALL BOUNDS1
C
      CALL UVCOMP
C
      CALL CNCOMP
C
C     ICE GROWTH OR MELT DUE TO THERMAL EFFECT
C
      CALL THERMAL
C
C     UPDATE ICE VELOCITY , CONCENTRATION AND THICKNESS
C
      DO 200 I = 1 , N
      DO 200 J = 1 , M
      U(I,J) = U(I,J) + DUT(I,J)
      V(I,J) = V(I,J) + DVT(I,J)
      CN(I,J) = CN(I,J) + DNT(I,J)
      IF ( CN(I,J) .LT. 0.0 ) CN(I,J) = 0.0
      TICE(I,J) = TICE(I,J) + DTT(I,J)
      IF ( JOFN(I,J) .EQ. 0 .OR. CN(I,J) .LE. 1.0 ) GO TO 200
      TICE(I,J) = TICE(I,J) * CN(I,J)
      CN(I,J) = 1.0
200   CONTINUE
C
      IT = T
      IF ( IT/IPR*IPR .EQ. IT ) CALL WRITEX (T)
      IF ( T .GE. TMAX ) GO TO 1000
9999  CONTINUE
1000  STOP
C
101   FORMAT ( 3F10.0 , I10 )
102   FORMAT ( 8F10.0 )
103   FORMAT ( 4A3 , F10.0 )
104   FORMAT ( 21I3 )
105   FORMAT ( 20F4.0 )
106   FORMAT (1H1,///,20X,'M       =',I15,20X,'N       =',I15,//
     *      ,20X,'MELT    =',12X,A3,20X,'DL      =',F15.2,//
     *      ,20X,'C1      =',E15.2,20X,'C2      =',E15.2,//
     *      ,20X,'FK2MIN  =',E15.1,20X,'FK2MAX  =',E15.1,//
     *      ,20X,'YIDO    =',F15.2,20X,'PST     =',F15.2,//
     *      ,20X,'C3      =',E15.2,20X,'K       =',I15,  //
     *      ,20X,'RHOICE  =',F15.2,20X,'DIVLIM  =',F15.4,//
     *      ,20X,'TMAX    =',F15.2,20X,'PRINTM  =',F15.2,//
     *      ,20X,'DISKINT =',F15.2,20X,'WINDINT =',F15.2,//
     *      ,20X,'IDISK   =',12X,A3,20X,'ICONT   =',12X,A3,//
     *      ,20X,'OVERWRT =',12X,A3,20X,'PTIME   =',F15.2,////)
107   FORMAT ( 1H1 ,  // , 10X , ' JO ( I , J )' / )
108   FORMAT ( // , 10X , ' JOFN ( I , J )' / )
109   FORMAT ( / 10X , 21I4 )
110   FORMAT ( 20I4 )
111   FORMAT ( 2I10 )
112   FORMAT ( 9F5.0 )
C
      END
      SUBROUTINE WRITEX ( TT )
      DIMENSION U(78,21), V(78,21), P(78,21), JO(78,21), JOFN(78,21),
     *       CN(78,21), DIV(78,21), TICE(78,21), WIN(3), WAN(3), TEMP(3)
      COMMON / SPEEDY / U , V
      COMMON / JOINT / JO , JOFN
      COMMON / DIVER / DIV
      COMMON / THICK / TICE
      COMMON / MASS / CN
      COMMON / PRESUR / P
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / WIND / WIN, WAN, TEMP
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
      COMMON / WRITES / IDISK, KOUNT, ICONT, OVERWRT, YIDO
      COMMON / WINDEX / PRINTM, WINDINT, DISKINT,  IRATIO, IW,
     *                  IPR, MELT
      COMMON / CONSTAN / DL, DT, DUM2
      COMMON / PCONSTS /  PST, T0, DIVLIM, K
      IHOUR = TT / 3600
      IMIN = ( TT - IHOUR * 3600 ) / 60
      ISEC = TT - IHOUR * 3600 - IMIN * 60
      WRITE( 6 , 333 ) IHOUR, IMIN, ISEC, ((WIN(I), WAN(I)),I = 1,3), DT
      WRITE ( 6 , 400 )
      DO 100 I = 1 , NM1
100   WRITE ( 6 , 500 ) ( P(I,J), J = 1 , MM1 )
      WRITE ( 6 , 430 )
      DO 140 I = 1 , NM1
140   WRITE ( 6 , 530 ) ( DIV(I,J), J = 1 , MM1 )
C
      CALL PRINTER ( IHOUR, IMIN, ISEC, 1, 4 )
C
      IF ( IDISK .NE. 3HYES ) GO TO 3000
      IF (TT .NE. 0.) GO TO 160
      WRITE ( 9 , 300 ) N, M, NM1, MM1
      WRITE ( 9 , 310 ) DT,DL,YIDO,FK2MIN,FK2MAX,PST,T0,DIVLIM,WINDINT,K
      WRITE ( 9 , 300 ) ( ( JO(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 300 ) ( ( JOFN ( I , J ), J = 1 , MM1 ), I = 1 , NM1 )
160   IF ( ICONT .EQ. 3HYES .AND. OVERWRT .NE. 3HYES .AND.
     *     KOUNT .EQ. 0 ) GO TO  3000
      IF ( KOUNT / IRATIO * IRATIO .NE. KOUNT ) GO TO 3000
      WRITE ( 9 , 520 ) TT, (( WIN(I), WAN(I)), I = 1, 3 )
      WRITE ( 9 , 550 ) ( ( U(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 550 ) ( ( V(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 550 ) ( ( CN(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 570 ) ( ( P(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 550 ) ( ( TICE(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 560 ) ( ( DIV(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
3000  KOUNT = KOUNT + 1
C
      CALL CHECK
C
300   FORMAT ( 30I4 )
310   FORMAT ( 9F10.1 , I2 )
333   FORMAT ( 1H1, / , 15X, I4,' HOURS',I4,' MINUTES',I4,' SECONDS',
     *    6X,'WIND AT TOLEDO    =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'WIND AT CLEVELAND =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'WIND AT BUFFALO   =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'DT =',F6.1,' SEC.',// )
400   FORMAT ( // , 10X , ' P ( I , J ) ( NEWTON/M/M ) ' / )
430   FORMAT ( //,10X,'DIVERGENCE ( I , J ) ( KG/M/M - SEC ) ' / )
500   FORMAT ( 1X , 21F5.0 )
510   FORMAT ( 1X , 21F5.3 )
520   FORMAT ( F8.0, 6F8.2 )
530   FORMAT ( 1X , 21E6.0 )
540   FORMAT ( / 10X, 20F5.2 )
550   FORMAT ( 15F8.3 )
560   FORMAT ( 15E8.1 )
570   FORMAT ( 15F8.0 )
      RETURN
      END
      SUBROUTINE RESTART ( T , PT , INDEX , OVERWRT )
      DIMENSION U(78,21), V(78,21), CN(78,21), TICE(78,21), DIV(78,21),
     *          P(78,21)
      COMMON / SPEEDY / U , V
      COMMON / DIVER / DIV
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / PRESUR / P
      COMMON / GENERAL / N , M , NM1 , MM1
      INDEX = 1
      READ ( 9 , 50 ) IDUM
      READ ( 9 , 50 ) IDUM
      IS = FLOAT( N * M ) / 30. + .97
      ISM = FLOAT( NM1*MM1 ) / 30. + .97
      DO 52 I = 1 , IS
52    READ ( 9 , 50 ) IDUM
      DO 51 I = 1 , ISM
51    READ ( 9 , 50 ) IDUM
      DO 66 K = 1 , 100
c      READ ( 9 , 10 ) T , A , B , C , D , E , F
C      IF ( EOF(9) ) 70, 59
C      made change in above 2 lines as follows
      read(9, 10, end=70) T, A, B, C, D, E, F    
59    WRITE ( 6 , 55 ) T , A , B , C , D , E , F
      READ ( 9 , 10 ) (( U(I,J), J = 1 , M ), I = 1, N )
      READ ( 9 , 10 ) (( V(I,J), J = 1 , M ), I = 1, N )
      READ ( 9 , 10 ) (( CN(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 10 ) (( P(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 10 ) (( TICE(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 15 ) (( DIV(I,J), J = 1 , MM1 ), I = 1, NM1 )
      IF ( T .EQ. PT ) GO TO 65
66    CONTINUE
70    INDEX = 0
      GO TO 64
65    WRITE ( 6 , 20 ) T , A , B , C , D , E , F
      GO TO 67
64    WRITE ( 6 , 30 ) PT , T
67    IF ( (OVERWRT .EQ. 3HYES) .OR. (INDEX .EQ. 0) ) REWIND 9
10    FORMAT ( 15F8.0 )
15    FORMAT ( 15E8.1 )
20    FORMAT ( //, 7X,'RESTART FROM T =',F10.1,'SEC.',5X,'WIND ',
     *         'AT TOLEDO    =',F4.1,' M/SEC',2X,F5.1,' DEG',/,42X,
     *    'WIND AT CLEVELAND =',F4.1,' M/SEC',2X,F5.1,' DEG',/,42X,
     *    'WIND AT UFFALO    =',F4.1,' M/SEC',2X,F5.1,' DEG',// )
30    FORMAT (/,10X,'CANNOT LOCATE T =',F10.1,' SEC.',5X,'LAST READ',
     *        ' FROM FILE 9 WAS T =',F10.1,' SEC.',/)
50    FORMAT ( A1 )
55    FORMAT ( // , 20X , 'T =' , F10.1 , 'SEC.' , 5X ,
     *    'WIND AT TOLEDO    =',F4.1,'M/SEC',2X,F5.1,'DEG.',/,42X,
     *    'WIND AT CLEVELAND =',F4.1,'M/SEC',2X,F5.1,'DEG.',/,42X,
     *    'WIND AT BUFFALO   =',F4.1,'M/SEC',2X,F5.1,'DEG.',/ )
      RETURN
      END
      SUBROUTINE BOUNDS1
      DIMENSION U(78,21), V(78,21), JO(78,21), JOFN(78,21)
     *        , TAUAX(78,21), TAUAY(78,21)
      COMMON / SPEEDY / U , V
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      DO 100 I = 1 , N
      DO 100 J = 1 , M
      JOM = JO ( I , J ) - 1
      IF ( ( JOM .LT. 1 ) .OR. ( JOM .EQ. 9 ) ) GO TO 100
      TAUAXIJ = TAUAX ( I , J )
      TAUAYIJ = TAUAY ( I , J )
      GO TO ( 1 , 2 , 3 , 3 , 1 , 1 , 2 , 2 ) , JOM
1     IF ( U ( I-1 , J ) ) 11 , 10 , 12
2     IF ( U ( I+1 , J ) ) 12 , 9 , 13
9     IF ( TAUAXIJ ) 12 , 13 , 13
10    IF ( TAUAXIJ ) 11 , 11 , 12
11    U(I,J) = U ( I-1 , J )
      GO TO 3
12    U(I,J) = 0.
      GO TO 3
13    U(I,J) = U ( I+1 , J )
3     GO TO ( 100 , 100 , 5 , 6 , 6 , 5 , 5 , 6 ) , JOM
5     IF ( V ( I , J-1 ) ) 21 , 20 , 22
6     IF ( V ( I , J+1 ) ) 22 , 19 , 23
19    IF ( TAUAYIJ ) 22 , 23 , 23
20    IF ( TAUAYIJ ) 21 , 21 , 22
21    V(I,J) = V ( I , J-1 )
      GO TO 100
22    V(I,J) = 0.
      GO TO 100
23    V(I,J) = V ( I , J+1 )
100   CONTINUE
      RETURN
      END
      SUBROUTINE CNCOMP
      DIMENSION CN(78,21), U(78,21), V(78,21), JO(78,21), JOFN(78,21),
     *          DNT(78,21), TICE(78,21), DTT(78,21), DIV(78,21)
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
      SUBROUTINE UVCOMP
      DIMENSION U(78,21), V(78,21), P(78,21), DUT(78,21), DVT(78,21),
     *          CN(78,21), JO(78,21), JOFN(78,21), TICE(78,21),
     *          WX(78,21), WY(78,21), DIV(78,21), TAUAX(78,21),
     *          TAUAY(78,21)
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
      SUBROUTINE PRINTER ( IHOUR, IMIN, ISEC, IB, IE )
      CHARACTER TITLE(4)*30
      DIMENSION  JO(78,21), JOFN(78,21), MM(110), NO(10), TICE(78,21),
     *           U(78,21), V(78,21), CN(78,21), BOUNDP(4),
     *           BOUNDM(4), NO2(2)
      COMMON / MASS / CN
      COMMON / SPEEDY / U , V
      COMMON / THICK / TICE
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / JOINT / JO , JOFN
      DATA NO2 / 1H- , 1H /
      DATA NO / 1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9 /
      DATA TITLE(1)/'ICE CONCENTRATION (PERCENT)'/
      DATA TITLE(2)/'ICE THICKNESS (CM)'/
      DATA TITLE(3)/'ICE VELOCITY U (CM/S)'/
      DATA TITLE(4)/'ICE VELOCITY V (CM/S)'/
C ABOVE FOR STATEMENT REPLACE BELOW FOUR STATEMENTS
C      DATA (TITLE(1,I),I=1,3)/10HICE CONCEN,10HTRATION ( ,10HPER CENT )/
C      DATA (TITLE(2,I),I=1,3)/10HICE THICKN,10HESS   ( CM,10H )        /
C      DATA (TITLE(3,I),I=1,3)/10HICE VELOCI,10HTY U  ( CM,10H/S )      /
C      DATA (TITLE(4,I),I=1,3)/10HICE VELOCI,10HTY V  ( CM,10H/S )      /
      DATA BOUNDP / 100.5 , 3*999.5 / , BOUNDM / 2*0.5 , 2*-998.5 /
C
      DO 999 II = IB , IE
      WRITE (6, 1100) TITLE(II),IHOUR,IMIN,ISEC
C ABOVE RELPLACES BELOW STATEMENT
C      WRITE ( 6 , 1100 ) ( TITLE (II,I), I = 1 , 3 ), IHOUR, IMIN, ISEC
      WRITE ( 6 , 1001 ) ( I , I = 10 , 110 , 10 )
      IF ( II .LE. 2 ) GO TO 9
      NG = N
      MG = M
      GO TO 10
    9 NG = NM1
      MG = MM1
C
   10 DO 110 I = 1 , NG
      KK = 5 * I
      DO 115 K = 1 , 110
C  115 MM (K) = 1H
C REPLACED ABOVE STATEMENT WITH BELOW STATEMENT
  115 MM (K) = NO2(2)
      MM (88) = 1HI
      DO 100 J = 1 , MG
      JGN = JOFN (I,J)
      IF ( II .GT. 2 ) JGN = JO (I,J)
      IF ( JGN .EQ. 0 ) GO TO 100
      K = J * 4
C
      GO TO ( 11 , 12 , 13 , 14 ) II
   11 GIJ = CN (I,J) * 100. + 0.5
      GO TO 5
   12 GIJ = TICE (I,J) * 100. + 0.5
      GO TO 5
   13 GIJ = U (I,J) * 100. + 0.5
      GO TO 5
   14 GIJ = V (I,J) * 100. + 0.5
    5 IF ( GIJ ) 120 , 130 , 140
  120 IF ( GIJ .LT. BOUNDM (II) ) GO TO 125
      ISIGN = 1
      GO TO 150
  140 IF ( GIJ .GT. BOUNDP (II) ) GO TO 145
      ISIGN = 2
      GO TO 150
  130 MM (K) = 1H0
      GO TO 100
  125 MM (K) = 1H-
      MM (K-1) = 1H-
      MM (K-2) = 1H-
      GO TO 100
  145 MM (K) = 1H+
      MM (K-1) = 1H+
      MM (K-2) = 1H+
      GO TO 100
  150 IN = ABS (GIJ)
      IN3 = IN / 100
      IN2 = ( IN - IN3 * 100 ) / 10
      IN1 = IN - IN3 * 100 - IN2 * 10
      MM (K) = NO (IN1+1)
      MM (K-1) = NO (IN2+1)
      MM (K-2) = NO (IN3+1)
      IF ( IN3 .EQ. 0 .AND. IN2 .EQ. 0 ) GO TO 160
      IF ( IN3 .EQ. 0 ) GO TO 161
      MM (K-3) = NO2 (ISIGN)
      GO TO 100
C  160 MM (K-2) = 1H
C REPLACE ABOVE STATEMENT WITH BELOW STATEMENT
  160 MM (K-2) = NO2(2)
      MM (K-1) = NO2 (ISIGN)
      GO TO 100
  161 MM (K-2) = NO2 (ISIGN)
  100 CONTINUE
      IF ( I / 2 * 2 .EQ. I ) GO TO 170
      WRITE ( 6 , 1004 ) MM
      GO TO 110
  170 WRITE ( 6 , 1003 ) KK , MM
  110 CONTINUE
      WRITE ( 6 , 1002 ) ( I , I = 10 , 110 , 10 )
  999 CONTINUE
      RETURN
C
 1001 FORMAT ( 9X, '0 ', 11I8, /, 9X, 'I', 11('---.---I') )
 1002 FORMAT ( 9X,'+',/,9X,'I',11('---.---I'),/,9X,'0 ',11I8,///// )
 1003 FORMAT ( 9X , '+' , / , 5X , I4 , 'I' , 110A1 )
 1004 FORMAT ( 9X , '+' , / , 9X , '+' , 110A1 )
 1100 FORMAT ( 1H1, ///// , 15X , 3A10 , ' AT' , I4 , ' HOURS' , I4 ,
     '         ' MINUTES' , I4 , ' SECONDS' , // )
      END
      SUBROUTINE WINCOMP ( TT , DTX )
      DIMENSION TAUAX(78,21) , TAUAY(78,21) , JO(78,21) , JOFN(78,21) ,
     *          WX(78,21) , WY(78,21) , WIN(3) , WAN(3) , TEMP(3) ,
     *          TTEMP(3) , PHI(3) , PSI(3) , VAX(3) , VAY(3) ,
     *          WXX(3) , WYY(3) , A(78,21) , B(78,21)
      COMMON / WINTEMP / A , B
      COMMON / WATER / WX , WY
      COMMON / WIND / WIN, WAN, TEMP
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / WINDEX / PRINTM, WINDINT, DISKINT, IRATIO, IW,
     *                  IPR, MELT
      DATA WX , WY / 3276 * 0. /
      DATA TAUAX , TAUAY / 3276 * 0. /
C
      FVAX ( A , B ) = A * COS ( B * PI / 180. )
      FVAY ( A , B ) = A * SIN ( B * PI / 180. )
      FTAUA ( A , B ) = C1 * ABS ( A ) * B
      FPHI ( A ) = - 0.012 * A + 1.
      FPSI ( A ) = 1.132 + 2.202 / A
      FOMEGA ( A, B, C, D, E, F ) = A * B + C * D + E * F
C
      IHOUR = TT / 3600
      IMIN = ( TT - IHOUR * 3600 ) / 60
      ISEC = 1
      WRITE ( 6 , 107 ) IHOUR, IMIN, ISEC
      PI = 4. * ATAN ( 1.)
      WINMAX = 0.0
      DO 1 I = 1 , 3
      WIN (I) = WIN (I) * 0.514
      WAN (I) = 246.5 - WAN (I)
      TTEMP (I) = TEMP (I) - 32.0
      WAN45 = WAN(I) - 45.
      IF ( TTEMP (I) .LE. 0. ) TTEMP (I) = 0.
      PHI (I) = FPHI (TTEMP(I))
      IF ( WIN (I) .NE. 0. ) GO TO 2
      PSI (I) = 0.
      GO TO 3
    2 PSI (I) = FPSI (WIN(I))
    3 WPHS = WIN (I) * PHI (I) * PSI (I)
      VAX (I) = FVAX ( WPHS , WAN(I) )
      VAY (I) = FVAY ( WPHS , WAN(I) )
      WXX (I) = FVAX ( WIN(I) , WAN45 )
      WYY (I) = FVAY ( WIN(I) , WAN45 )
      WINMAX = AMAX1 ( WINMAX , ABS(VAX(I)) , ABS(VAY(I)) )
    1 CONTINUE
      DO 100 I = 1 , N
      DO 100 J = 1 , M
      IF ( JO(I,J) .EQ. 0 ) GO TO 100
      TOLX = I + 3
      TOLY = J - 15
      CLEX = I - 24
      CLEY = J + 1
      BUFX = I - 79
      BUFY = J - 10
      RDISTOL = 1.0 / ( TOLX ** 2 + TOLY ** 2 )
      RDISCLE = 1.0 / ( CLEX ** 2 + CLEY ** 2 )
      RDISBUF = 1.0 / ( BUFX ** 2 + BUFY ** 2 )
      SUM = RDISTOL + RDISCLE + RDISBUF
      OMEGAT = RDISTOL / SUM
      OMEGAC = RDISCLE / SUM
      OMEGAB = RDISBUF / SUM
      TAUAX (I,J) =  FOMEGA ( VAX(1), OMEGAT, VAX(2), OMEGAC,
     *               VAX(3), OMEGAB )
      TAUAY (I,J) =  FOMEGA ( VAY(1), OMEGAT, VAY(2), OMEGAC,
     *               VAY(3), OMEGAB )
      WX (I,J) = 0.03 * FOMEGA ( WXX(1), OMEGAT, WXX(2), OMEGAC,
     *           WXX(3), OMEGAB )
      WY (I,J) = 0.03 * FOMEGA ( WYY(1), OMEGAT, WYY(2), OMEGAC,
     *           WYY(3), OMEGAB )
      A (I,J) = FOMEGA ( WIN(1), OMEGAT, WIN(2), OMEGAC, WIN(3),
     *          OMEGAB )
      B (I,J) = FOMEGA ( TEMP(1), OMEGAT, TEMP(2), OMEGAC,
     *          TEMP(3), OMEGAB )
  100 CONTINUE
      DTX = 60.
      IF ( MELT .EQ. 3HYES ) GO TO 4
      IF ( WINMAX .LE. 10.0 ) DTX = DTX + 60.
      IF ( WINMAX .LE.  7.0 ) DTX = DTX + 60.
      IF ( WINMAX .LE.  4.5 ) DTX = DTX + 60.
    4 CONTINUE
      IF ( DTX .GT. PRINTM ) PRINTM = DTX
      IF ( DTX .GT. WINDINT ) WINDINT = DTX
      IRATIO = DISKINT / PRINTM
      IF ( IRATIO .LT. 1 ) IRATIO = 1
      IW = WINDINT
      IPR = PRINTM
      WRITE ( 6 , 101 ) ( ( WIN (I) , WAN (I) ) , I = 1 , 3 )
      WRITE ( 6 , 102 ) ( TEMP (I) , I = 1 , 3 )
      WRITE ( 6 , 106 ) DTX
      WRITE ( 6 , 103 )
C     DO 200 I = 1 , N
C 200 WRITE ( 6 , 105 ) ( TAUAX(I,J) , J = 1 , M )
C     WRITE ( 6 , 104 )
C     DO 300 I = 1 , N
C 300 WRITE ( 6 , 105 ) ( TAUAY(I,J) , J = 1 , M )
      DO 400 I = 1 , N
      DO 400 J = 1 , M
      IF ( JO (I,J) .EQ. 0 ) GO TO 400
      TAUAX (I,J) = FTAUA ( A(I,J) , TAUAX(I,J) )
      TAUAY (I,J) = FTAUA ( A(I,J) , TAUAY(I,J) )
  400 CONTINUE
C
  101 FORMAT ( //,20X,'WIND AT TOLEDO    =',F5.1,1X,'M/SEC',2X,
     *         F6.1,1X,'DEG.',/,20X,'WIND AT CLEVELAND =',F5.1,
     *         1X,'M/SEC',2X,F6.1,1X,'DEG.',/,20X,'WIND AT ',
     *         'BUFFALO   =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.' )
  102 FORMAT (//,20X,'AIR TEMPERATURE AT TOLEDO    =',F5.1,1X,'DEG. F'
     *       ,/,20X,'AIR TEMPERATURE AT CLEVELAND =',F5.1,1X,'DEG. F'
     *       ,/,20X,'AIR TEMPERATURE AT BUFFALO   =',F5.1,1X,'DEG. F' )
  103 FORMAT ( //,10X,'WIND FIELD OVER LAKE ERIE  ( X-DIRECTION',
     *         ' COMPONENT )',/ )
  104 FORMAT ( //,10X,'WIND FIELD OVER LAKE ERIE  ( Y-DIRECTION',
     *         ' COMPONENT )',/ )
  105 FORMAT ( / , 5X , 21F5.1 )
  106 FORMAT ( //, 20X, 'DT = ', F6.0, 'SEC', // )
  107 FORMAT ( //, 20X, 'METEOROLOGICAL CONDITIONS AT ', I5, ' HOURS',
     *         I5, ' MINUTES', I5, ' SECONDS' )
C
      RETURN
      END
      SUBROUTINE THERMAL
      DIMENSION TICE (78,21), CN (78,21), DNT (78,21), DTT(78,21),
     *          JO (78,21), JOFN (78,21), A (78,21), B (78,21)
      COMMON / WINTEMP / A , B
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / DELTCN / DNT , DTT
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / CONSTAN / DL , DT , ETA
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
C
      DATA TALPHA, TBETA, TGAMA, TZETA / 3.0E-06 , 6.0E-05 ,
     *     6.0E-06 , 1.0E-03 /
C
      DO 100 I = 1 , NM1
      DO 100 J = 1 , MM1
      IF ( JOFN (I,J) .EQ. 0 ) GO TO 100
      CNIJ = CN (I,J)
      TICEIJ = TICE (I,J)
      WINDIJ = A (I,J)
      TEMPIJ = B (I,J)
      DELTEMP = 32.0 - TEMPIJ
      IF ( TEMPIJ .GT. 32.0 ) GO TO 10
      EM =  DELTEMP * ( TALPHA * CNIJ / TICEIJ + TBETA * WINDIJ *
     *       ( 1.0 - CNIJ ) )
      EA = TBETA * DELTEMP * WINDIJ * (1.0 - CNIJ) / ( RHOICE * TICEIJ )
      IF ( CNIJ .LE. 0. ) CNIJ = 0.001
      GO TO 20
   10 IF ( CNIJ .LT. 0.10 .OR. TICEIJ .LT. 0.03 ) GO TO 100
      EM = TGAMA * WINDIJ * DELTEMP * CNIJ
      EA = TZETA * EM / TICEIJ
   20 DTT (I,J) = DTT (I,J) + DT * ( EM / ( CNIJ * RHOICE ) -
     *             EA * TICEIJ / CNIJ )
      DNT (I,J) = DNT (I,J) + DT * EA
  100 CONTINUE
      RETURN
      END
      SUBROUTINE CHECK
      DIMENSION CN (78,21) , TICE (78,21)
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / GENERAL / N , M , NM1 ,MM1
C
      VOLUME = 0.
      AREA = 0.
      DO 100 I = 1 , NM1
      DO 100 J = 1 , MM1
      AREA = AREA + CN (I,J)
      VOLUME = VOLUME + CN (I,J) * TICE (I,J)
  100 CONTINUE
      AREA = AREA * 25000000.
      VOLUME = VOLUME * 25000000.
      WRITE ( 6 , 200 ) AREA , VOLUME
  200 FORMAT ( //,20X,'TOTAL ICE AREA   = ',F15.0,' SQUARE METERS',
     *          //,20X,'TOTAL ICE VOLUME = ',F15.0,' CUBIC METERS',/// )
      RETURN
      END
      SUBROUTINE BOUNDS2
      DIMENSION U(78,21), V(78,21), JO(78,21), JOFN(78,21)
     *        , TAUAX(78,21), TAUAY(78,21)
      COMMON / SPEEDY / U , V
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      DO 100 I = 1 , N
      DO 100 J = 1 , M
      JOM = JO ( I , J ) - 1
      IF ( JOM .LT. 1 ) GO TO 100
      TAUAXIJ = TAUAX ( I , J )
      TAUAYIJ = TAUAY ( I , J )
      GO TO ( 1, 2, 14, 15, 11, 11, 13, 13, 30, 31, 32, 33 ) , JOM
1     IF ( U ( I-1 , J ) ) 11 , 10 , 12
2     IF ( U ( I+1 , J ) ) 12 , 9 , 13
9     IF ( TAUAXIJ ) 12 , 13 , 13
10    IF ( TAUAXIJ ) 11 , 11 , 12
11    U(I,J) = U ( I-1 , J )
      GO TO 3
12    U(I,J) = 0.
      GO TO 3
13    U(I,J) = U ( I+1 , J )
      GO TO 3
14    U(I,J) = U ( I , J-1 )
      GO TO 3
15    U(I,J) = U ( I , J+1 )
3     GO TO ( 24 , 25 , 5 , 6 , 23 , 21 , 21 , 23 ) , JOM
5     IF ( V ( I , J-1 ) ) 21 , 20 , 22
6     IF ( V ( I , J+1 ) ) 22 , 19 , 23
19    IF ( TAUAYIJ ) 22 , 23 , 23
20    IF ( TAUAYIJ ) 21 , 21 , 22
21    V(I,J) = V ( I , J-1 )
      GO TO 100
22    V(I,J) = 0.
      GO TO 100
23    V(I,J) = V ( I , J+1 )
      GO TO 100
24    V(I,J) = V ( I-1 , J )
      GO TO 100
25    V(I,J) = V ( I+1 , J )
      GO TO 100
30    IF ( U ( I+1 , J+1 ) ) 41 , 41 , 42
31    IF ( U ( I+1 , J-1 ) ) 51 , 51 , 52
32    IF ( U ( I-1 , J-1 ) ) 61 , 61 , 62
33    IF ( U ( I-1 , J+1 ) ) 71 , 71 , 72
41    U(I,J) = 0.
      GO TO 80
42    U(I,J) = U ( I+1 , J+1 )
      GO TO 80
51    U(I,J) = 0.
      GO TO 81
52    U(I,J) = U ( I+1 , J-1 )
      GO TO 81
61    U(I,J) = U ( I-1 , J-1 )
      GO TO 82
62    U(I,J) = 0.
      GO TO 82
71    U(I,J) = U ( I-1 , J+1 )
      GO TO 83
72    U(I,J) = 0.
      GO TO 83
80    IF ( V ( I+1 , J+1 ) ) 91 , 91 , 92
81    IF ( V ( I+1 , J-1 ) ) 101 , 101 , 102
82    IF ( V ( I-1 , J-1 ) ) 111 , 111 , 112
83    IF ( V ( I-1 , J+1 ) ) 121 , 121 , 122
91    V(I,J) = 0.
      GO TO 100
92    V(I,J) = V ( I+1 , J+1 )
      GO TO 100
101   V(I,J) = V ( I+1 , J-1 )
      GO TO 100
102   V(I,J) = 0.
      GO TO 100
111   V(I,J) = V ( I-1 , J-1 )
      GO TO 100
112   V(I,J) = 0.
      GO TO 100
121   V(I,J) = 0.
      GO TO 100
122   V(I,J) = V ( I-1 , J+1 )
100   CONTINUE
      RETURN
      END
