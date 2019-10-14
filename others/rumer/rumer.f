      PROGRAM rumer
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
      INCLUDE "glgrid.inc"

      DIMENSION U(NX,NY), V(NX,NY), CN(NX,NY), DUT(NX,NY), DVT(NX,NY)
     *       , DNT(NX,NY), P(NX,NY), JO(NX,NY), JOFN(NX,NY)
     *       , TICE(NX,NY), DTT(NX,NY), DIV(NX,NY)
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

CBG      open(unit=4,file='setup.file',readonly,status='old')
      open(unit=4,file='setup.file',status='old')
      read(4,210)infile
      read(4,210)outfile
  210 format(1x,a79)
CBG      open(unit=5,file=infile,readonly, status='old')
      open(unit=5,file=infile,status='old')
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
