       PROGRAM ICLFM4
C      PROGRAM ICLFM4(OUTPUT,WVLFMBND,WVLFMSIN,WVLFMS00,WVLFMS06,
C    &              WVLFMS12,WVFMVV06,WVFMVV12,UNIT6=OUTPUT,
C    &              UNIT20=WVLFMBND,UNIT21=WVLFMSIN,
C    &              UNIT30=WVLFMS00,UNIT31=WVLFMS06,UNIT32=WVLFMS12,
C    &              UNIT51=WVFMVV06,UNIT52=WVFMVV12)
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  ICLFM4      FOURTH-ORDER LFM FCST MODEL
C   AUTHOR: DEAVEN             ORG: NMC22         DATE: 86-08-28
C
C MODIFIED BY DEAVEN ON 5/7/87 FOR FORTRAN 77 (FTNNEW)
C     PROGRAM CARD CHANGED FOR FORTRAN 77 FILE PRECONNECTS
C     NO OTHER SOURCE CODE CHANGES
C
C ABSTRACT: EXECUTES THE FORTH ORDER  VERSION OF THE
C   LFM FORECAST MODEL WITH 7 LAYERS ON THE 190.5 KM POLAR STERO
C   GRID.  FORECASTS FOR 12 HOURS.
C
C PROGRAM HISTORY LOG:
C   72-??-??  ORIGINAL AUTHOR(S): ????
C   92-06-15  R.E.JONES   CONVERT CRAY SOURCE TO RUN ON 32 BIT 
C                         WORKSTATIONS
C   93-05-14  R.E.JONES   CORRECTION IN TEND5 FOR OVERFLOW PROBLEM
C
C USAGE:
C   INPUT FILES:
C     UNIT20   -   BOUNDARY VALUES FROM SPECTRAL MODEL
C     UNIT21   -   INITIAL  VALUES FROM LFMINI
C     UNIT30   -   INITIAL  VALUE OF A 12 HOUR FORECAST SEGMENT
C
C   OUTPUT FILES:
C     UNIT53   -    6,18,30,42 HR HISTORY FILE
C     UNIT54   -   12,24,36,48 HR HISTORY FILE
C     UNIT51   -    6,18,30,42 HR VERTICAL VELOCITIES
C     UNIT52   -   12,24,36,48 HR VERTICAL VELOCITIES
C     OUTPUT   -   PRINT OUTPUT
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     ABSH2O, ARYTOT, BLDBND, DSXDY,  TENXY,
C                 DSXY,   DSYDX,  ERREXT, FCST,   FTIME,
C                 INTBND, LCL,    NFB01,  PRECAL, RDBNDY(ZRBNDY),
C                 RDTAPE, SATM,   SOLDEC, STARTF, WRTAPE,
C                 TENDA,  TENDB,  STATS,  WATER,  SETBC, TSMO,
C                 TEND1,  TEND2,  TEND3,  TEND4,
C                 TEND5,  TEND6,  TENPT,  TENUV,
C                 TTOARY, UNPSEP, VVEL,   WKLOX
C
C     LIBRARY:
C       SPECIAL  - 
C
C   EXIT STATES:
C     COND = 0   SUCCESSFUL RUN
C          = 999 FAILURE
C          = 1607  DISASTER STOP FROM ERREXT.  SEE FT06 MESSAGES.
C
C
C   REMARKS: SEE DENNIS DEAVEN W/NMC23 DEVELOPMENT DIVISION FOR SCIENCE
C            MODIFICATION BY STACKPOLE W/NMC42 870602
C               TIME STEP SHORTENED FROM 9 STEPS PER HOUR TO 10 PER HOUR
C               IN AN ATTEMPT TO CURE S.W. CORNER JET BLOWUP
C               (GERRITY SAYS TO BLAME EL NINO - EVERYBODY ELSE DOES)
C               CHANGED CARDS ARE BRACKETED BY ***JDS*** COMMENTS
C
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
      IMPLICIT    REAL (A-H,O-Z)
      REAL        BNDATE(9)
      REAL        HOUR1
      REAL        ICE, MF
      LOGICAL     RECV
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23), BB( 12, 33 ,23 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /VTEMP/  SIGDOT( 53, 45, 5), VT( 53, 45, 25 )
C
      DATA RECV/.FALSE./
C
C     IRECT=RECOV.INT., IFTAPE=1 AT INITIAL PE READ
C
      DATA IRECT/6/,IFTAPE/1/
C
c     DOUBLE PRECISION INIT
c     DATA INIT/'LFMFXMP '/
c     CHARACTER*41 MYESLF/'LATERAL BOUNDARIES FROM PREVIOUS AVN RUN:'/
c     CHARACTER*41 MNOLFM/'LATERAL BOUNDARIES SET TO CONSTANT VALUE:'/
C
C        BEGIN THE WHOLE WORKS HERE
C-----------------------------------------------------------------------
C      CALL W3LOG('$S86240.72','ICLFM4  ')
       CALL W3TAGB('ICLFM4  ',0093,0160,0072,'NMC22 ')
C-----------------------------------------------------------------------
C        INITIALZE TEMPORARY VECTOR  SPACE WITH NINES
C
C-------------------------------------------------------
C
C          
      DO K = 1,25
      DO J = 1,45
      DO I = 1,53
        VT(I,J,K) = 9.0E0
      END DO
      END DO
      END DO
C
      PRINT 5
 5    FORMAT (//,45X,'NMC LFM FORECAST MODEL  FORECAST SECTION')
C     PRINT 99
C  99 FORMAT (/55X,'(VERSION DATE 07 MAY 87)'//)
C
C    IHOUR = CURRENT FCST HOUR SET IN RDTAPE FROM CONTENT OF LUNS00 FILE
C      SET UNIT NO. FOR I/O FILES.
C
      LUNBND = 20
      LUNSIN = 21
      LUNS00 = 30
      LUNS06 = 53
      LUNS12 = 54
      LUNV06 = 51
      LUNV12 = 52
C
    6 CONTINUE
      PRINT 4,LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12
    4 FORMAT(/,10X,'LOGICAL UNIT NOS. FOR ... ',/,5X,' BOUNDARY,',
     A     ' INPUT, 00HR, 06HR, 12HR SIGMA FIELDS ....',
     B     ' 06HR, 12HR VERT. VEL. FIELDS',/,
     C     8X,I4, 4(5X,I2), 19X,I2, 5X,I2,//)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C                                 ***JDS***   START
C - - - - - NOTE NPHOUR = 3600/DT = 3600/360 = 10 TIME STEPS PER HOUR
C             NPHOUR MUST BE INTEGER AS MUST BE DT
C             SUCH THAT THEIR PRODUCT = 3600 SECONDS PER HOUR
C             TYPICAL CHOICES ARE 9 (400 SEC PER TIME STEP)
C                                10 (360 SEC PER TIME STEP)
C                                12 (300 SEC PER TIME STEP)
C                                  ***JDS***  END
C - - - - -      IORDER = 2  SECOND ORDER DIFFERENCES
C - - - - -             = 4  FOURTH ORDER DIFFERENCES
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C           IORDER =  4
C                                  ***JDS***  START
            NPHOUR =  10
C                                  ***JDS***  END
            SATRH = 0.90E0
C
      CALL STARTF
      CALL PRECAL
      CALL RDTAPE
C
C     REESTABLISH BOUNDARY (FIELD) VALUES FOR FORECAST
C     UP TO IHOUR (IHOUR SET IN RDTAPE).
C
      CALL INTBND
C ADDED ZRBNDY CALL TO TRY AND FORCE EXECUTION
C
c     CALL ZRBNDY
      IRC = IHOUR + IRECT
c     IF (IRC.NE.9999) GO TO 10
C
C        TOP OF MANY PASS LOOP ( ONLY TWO TIMES HERE)
C        EACH PASS FORECASTS FOR IRECT HOURS WORTH
C        LOOP TERMINATES WHEN 12 HOURS HAVE BEEN DONE
C
 15   IHR1 = IHOUR
      IF (IFTAPE.NE.1) GO TO 181
      IFTAPE = 0
      REWIND LUNBND
      READ (LUNBND,END=13) LABBND
C     IF (IHOUR.EQ.0) CALL W3LOG('$L',MYESLF)
         GO TO 12
   13      PRINT 14
C     IF (IHOUR.EQ.0) CALL W3LOG('$L',MNOLFM)
   14      FORMAT('0    EOF ENCOUNTERED READING LABEL ON BOUNDARY FILE')
C                          (CORRECTIVE ACTION TAKEN BELOW)
           REWIND LUNBND
   12 IF (IHR1.EQ.0) GO TO 70
C
C      SPACE LIVE BNDRY TAPE
C
      ILS = 0
   21 READ (LUNBND,END=17) BNDATE
   16 ILS = ILS + 1
      IHR = BNDATE(1)
      IF (IHR.EQ.IHOUR) GO TO 22
      GO TO 21
   17 PRINT 151,  ILS, IHR1
      REWIND LUNBND
  151 FORMAT('0 ENCOUNTERED EOF POSITIONING LUNBND, SKIPPED',I5,
     1' RECORDS TRYING TO GET TO HOUR',I5)
      IHR = 999
      GO TO 22
   18 PRINT 152,  ILS
  152 FORMAT('0 PARITY ERROR SKIPPING RECORD ', I5, 'ON LUNBND')
      GO TO 16
C
C        BNDRY TAPE POSITIONED SO NEXT READ WILL GET PROPER TEND VALUES
C
   22 RECV   = .TRUE.
   70 IZTIME = HOUR1(2)
      IDAYMO = HOUR1(5)
      IMO    = HOUR1(4)
      IYEAR  = HOUR1(3)
 181  IF (IRC.NE.IHR1) GO TO 406
      IRC    = IRC + IRECT
 406  CONTINUE
      IOUT   = IRC
      IF (RECV) GO TO 23
C
C      READ LIVE BNDRY TAPE  ONLY EVERY 6 HOURS
C
      IF (MOD(IHOUR,6).NE.0) GO TO 10
C
      READ (LUNBND,END=35) BNDATE
      IHR = BNDATE(1)
   23 CONTINUE
      IF (IHR.EQ.IHOUR) GO TO 31
 35   CALL ZRBNDY
      REWIND LUNBND
      GO TO 10
 31   CALL RDBNDY
      RECV = .FALSE.
   10 CONTINUE
C     ZERO VERTICAL VELOCITY STORAGE SHARED
      N8IJ = 8 * 2385
      DO 88900 IQ2W6E = 1,N8IJ
         IVVEL(IQ2W6E,1,1) = 0.0E0
88900 CONTINUE
C
C        FTIME WILL FORECAST FROM CURRENT TIME (IHOUR,IHR1)
C            UP TO IOUT HOUR  (IOUT AND IRC EQUAL)
C
      CALL FTIME
      IF (MNSTEP.EQ.0) GOTO 720
C
C        GET OUT OF LOOP AFTER 12 HOURS OF FORECASTING
C
         IF(MOD(IHOUR,12).NE.0)  GOTO 15
C
C        BOTTOM OF FORECAST LOOP
C
C-----------------------------------------------------------------------
C     CALL W3LOG('$E')
C-----------------------------------------------------------------------
      PRINT 715
 715  FORMAT('1 END OF NMC PRIMITIVE EQUATION FORECAST')
      CALL W3TAGE('ICLFM4  ')
      STOP 0
C
C        DISASTER AREA
C
720   CONTINUE
C     CALL W3EXIT(999,'7 LAYER LFM MODEL FAILURE IN MAIN :',INIT)
      PRINT *,'7 LAYER LFM MODEL FAILURE IN MAIN :',INIT
      STOP 999
      END
      SUBROUTINE ARYTOT(JROW,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: ARYTOT         CREATES SIGMA STRIPS FOR OUTPUT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: CONVERTS MODEL COMMON BLOCK (DEABLK) FORMAT TO SIGMA
C   STRIPS (ONE ROW AT A TIME) AS REQUIRED BY THE POST PROCESSOR.
C   THE SIGMA STRIPS CONTAIN MODEL OUTPUT AT TAU AND TAU-1 TIME
C   LEVELS PLUS ALL OF THE FIXED FIELDS.
C
C USAGE:  CALL ARYTOT (JROW,A)
C
C
C  - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JROW        ROW INDEX                                 ARGUMENT
C     LI,LJ--     DOMAIN SIZES AND TIME INDICIES            /INDEX/
C     US,VS--     MODEL FORECAST VARIABLES                  /DEABLK/
C                 AND FIXED FIELDS
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A           SIGMA STRIP BLOCK FOR POST PROCESSOR      ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        A( 3816 )
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      COMMON /DEABLK/ US( 53, 45, 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      SAVE
C--------------------------------------------------------------------
C  SET UP POINTERS FOR THE BLOCK OF SIGMA STRIPS
C--------------------------------------------------------------------
      N0  =  1
      N1  =  53 * 36 + 1
      ND  =  53 * 9
      N2  =  N1 + ND
      N3  =  N2 + ND
      N4  =  N3 + ND
      N5  =  53 * 28
      N6  =  53 * 8
      N7  =  53
      N8  =  53 * 12
      N9  =  ND +  1
      N10 =  53 * 18 + 1
      N11 =  53 * 27 + 1
      N12 =  53 *  7 + 1
      N13 =  53 *  8 + 1
      N14 =  53 * 43 + 1
      N15 =  53 * 16 + 1
      N16 =  53 * 17 + 1
      N17 =  53 * 26 + 1
      N18 =  53 * 30 + 1
      N19 =  53 * 33 + 1
      N20 =  53 * 69 + 1
      N21 =  53 * 25 + 1
      N22 =  53 * 29 + 1
      N23 =  53 * 44 + 1
      N24 =  53 * 65 + 1
      N25 =  53 * 52 + 1
C-------------------------------------------------------------------
C  MOVE DATA FROM MODEL FORECAST FIELDS TO A
C-------------------------------------------------------------------
      DO 10 K = 1, 7
      MOLD  = K + LOLD *  7
      MMID  = K + LMID *  7
      NPLUS = (K-1) *  53
      DO 88890 IQ2W6E=1,N7
         A(N0+NPLUS+IQ2W6E-1)=US(IQ2W6E,JROW,MMID)
         A(N1+NPLUS+IQ2W6E-1)=US(IQ2W6E,JROW,MOLD)
         A(N9+NPLUS+IQ2W6E-1)=VS(IQ2W6E,JROW,MMID)
         A(N2+NPLUS+IQ2W6E-1)=VS(IQ2W6E,JROW,MOLD)
         A(N10+NPLUS+IQ2W6E-1)=TS(IQ2W6E,JROW,MMID)
         A(N3+NPLUS+IQ2W6E-1)=TS(IQ2W6E,JROW,MOLD)
88890 CONTINUE
      IF (K .GT. 3) GO TO 10
      MOLD = K + LOLD * 3
      MMID = K + LMID * 3
      DO 88950 IQ2W6E=1,N7
         A(N19+NPLUS+IQ2W6E-1)=WS(IQ2W6E,JROW,MMID)
         A(N20+NPLUS+IQ2W6E-1)=WS(IQ2W6E,JROW,MOLD)
         A(N18+NPLUS+IQ2W6E-1)=SATW(IQ2W6E,JROW,K)
88950 CONTINUE
      IF (K .GT. 2) GO TO 10
      MOLD = K + LOLD * 2
      MMID = K + LMID * 2
      DO 88980 IQ2W6E=1,N7
         A(N11+NPLUS+IQ2W6E-1)=PSIG(IQ2W6E,JROW,MMID)
         A(N4+NPLUS+IQ2W6E-1)=PSIG(IQ2W6E,JROW,MOLD)
88980 CONTINUE
10    CONTINUE
      DO 89000 IQ2W6E=1,N7
         A(N12+IQ2W6E-1) = ST(IQ2W6E,JROW)
         A(N13+IQ2W6E-1) = ICE(IQ2W6E,JROW)
         A(N14+IQ2W6E-1) = COSZEN(IQ2W6E,JROW)
         A(N15+IQ2W6E-1) = XM(IQ2W6E,JROW)
         A(N16+IQ2W6E-1) = F(IQ2W6E,JROW)
         A(N17+IQ2W6E-1) = CD(IQ2W6E,JROW)
         A(N21+IQ2W6E-1) = PREC(IQ2W6E,JROW)
         A(N22+IQ2W6E-1) = ZSTAR(IQ2W6E,JROW)
         A(N23+IQ2W6E-1) = ALON(IQ2W6E,JROW)
         A(N24+IQ2W6E-1) = ALAT(IQ2W6E,JROW)
         A(N25+IQ2W6E-1) = MF(IQ2W6E,JROW)
89000 CONTINUE
      RETURN
      END
      SUBROUTINE RDBNDY
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: RDBNDY         READ BOUNDARY CONDITIONS FOR VLFM
C   AUTHOR: STACKPOLE/DEAVEN ORG: W/NMC23    DATE: 30 MAR 83
C
C ABSTRACT: READS BOUNDARY CONDITIONS INFORMATION FROM FILE
C   CREATED FROM LARGE-SCALE MODEL RUN 12 HOURS AGO.
C   ADJUSTS FOR INCONSISTENT TIME STEPS.
C   ALSO CAN SET BOUNDARY VALUES TO CONSTANTS (ZERO TENDENCY)
C   IF TROUBLE WITH INPUT FILE, (VIA ENTRY ZRBNDY).
C
C
C USAGE:  CALL RDBNDY    (OR CALL ZRBNDY)
C
C
C  - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNBND      UNIT NUMBER OF BOUND COND.FILE            /FORTAP/
C     AA,BB       BOUNDARY VALUE TENDENCIES                 LUNBND
C     DT          TIME STEP (SEC)                           /CNST/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     AA,BB       BOUNDARY TEND. ADJ. FOR MODEL TIMESTEP    /PBNDRY/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        CC( 53 ,23, 12), DD( 12, 23, 33 )
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53, 45, 25 )
      SAVE
C---------------------------------------------------------------------
C  INPUT IS FULL PRECISION STORED IN CC AND DD
      DO 27 J = 1,6
        J1 = 2  * J - 1
        J2 = J1 + 1
        READ (LUNBND,END=35) (((CC(II,KK,JJ),II=1,53),
     1                              KK=1,23),JJ=J1,J2)
   27 CONTINUE
         READ (LUNBND,END=35) (((DD(II,KK,JJ),II=1,12),KK=1,23),
     1                                     JJ=1,       33 )
C     BOUNDARY TAPE IS WRITTEN ASSUMING 6 MIN TIMESTEPS.
C     DT CORRECTS FOR DIFFERENT ACTUAL TIMESTEP IN MODEL.
C---------------------------------------------------------------------
      XX = DT / 360.0E0
      DO 1034 J = 1,33
      DO 1034 K = 1,23
      DO 1034 I = 1,12
        DD(I,K,J) = DD(I,K,J) * XX
 1034 CONTINUE
      DO 2034 J = 1,12
      DO 2034 K = 1,23
      DO 2034 I = 1, 53
        CC(I,K,J) = CC(I,K,J) * XX
 2034 CONTINUE
      GO TO 3000
C
      ENTRY ZRBNDY
C
C   TAPE POSITIONED WRONG. TENDENCIES SET TO ZERO.
C
   35 CONTINUE
      REWIND LUNBND
      PRINT 900
  900 FORMAT(//' BNDRY TAPE POSITIONED WRONG. TENDENCIES SET TO ZERO.')
      DO 32 J = 1,12
      DO 32 K = 1,23
      DO 32 I = 1,53
        CC(I,K,J) = 0.0
   32 CONTINUE
      DO 33 J = 1,33
      DO 33 K = 1,23
      DO 33 I = 1,12
        DD(I,K,J)=0.0
   33 CONTINUE
3000  CONTINUE
C
C  MOVE CC AND DD TO AA AND BB CONVERTING HALF TO FULL PRECISION
C
      DO 40 K = 1,23
      DO 40 J = 1,12
      DO 40 I = 1,53
        AA(I,J,K) = CC(I,K,J)
40    CONTINUE
      DO 50 K = 1,23
      DO 50 J = 1,33
      DO 50 I = 1,12
        BB(I,J,K) = DD(I,K,J)
50    CONTINUE
      RETURN
      END
      SUBROUTINE INTBND
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: INTBND         INITIALIZE LATERAL BOUNDARY VALUES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: CREATES LATERAL BOUNDARY VALUES FOR THE SIX OUTERMOST
C   ROWS OF THE LFM FORECAST DOMAIN.
C
C USAGE:  CALL INTBND
C
C  - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNSIN      LOGICAL UNIT NUMBER FOR INITIAL DATA      /FORTAP/
C     BUFF        INITIAL SIGMA STRIPS                      LUNSIN
C     LUNBND      LOGICAL UNIT NUMBER FOR BOUNDARY DATA     /FORTAP/
C     AA-BB       LATERAL BOUNDARY VALUE TENDENCIES         /PBNDRY/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      LATERAL BOUNDARY VALUES                   /PBNDPE/
C     BLARGE             DITTO                              /PBNDPE/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     BLDBND,RDBNDY,ZRBNDY                                  LFMFCST
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        BUFF( 3816 ), XLAB(8)
      REAL        DATE(9)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/ IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12,23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /VTEMP / SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C     FETCH INITIAL CONDITIONS FOR FORECAST AND CONSTRUCT
C     BOUNDARY VALUES VIA BLDBND.
C
      NREC = 0
      IN   = LUNSIN
      REWIND IN
         READ (IN) LABSIN
         NREC = NREC + 1
      READ (IN) XLAB
         NREC = NREC + 1
      DO 10 L = 1,45
        READ (IN) BUFF
        NREC = NREC + 1
        CALL BLDBND(L,BUFF)
10    CONTINUE
C
C     FOR NONZERO HOUR RECOVERY, CONSTRUCT BOUNDARY VALUES
C     FOR RECOUVERY HOUR VIA RDBNDY AND ADDITON TO INITIAL
C     VALUES IN ALARGE, BLARGE.
C
      IF (IHOUR.LE.0) GO TO 98
      ICY = IHOUR/6
      REWIND LUNBND
      READ (LUNBND,END=45) LABBND
      DO 1 I = 1,ICY
      READ (LUNBND,END=35) DATE
      CALL RDBNDY
      GO TO 34
35    CALL ZRBNDY
      GO TO 98
34    CONTINUE
      ITER = NPHOUR * 6
      IF (I.EQ.1) ITER = NPHOUR*6 - 1
      NA = 12 * 23 *  53
      NB = 12 * 23 *  33
      FIT = ITER
      DO 88890 IQ2W6E=1,NA
         ALARGE(IQ2W6E,1,1)=ALARGE(IQ2W6E,1,1)+FIT*AA(IQ2W6E,1,1)
88890 CONTINUE
      DO 88900 IQ2W6E=1,NB
         BLARGE(IQ2W6E,1,1)=BLARGE(IQ2W6E,1,1)+FIT*BB(IQ2W6E,1,1)
88900 CONTINUE
1     CONTINUE
98    REWIND IN
      REWIND LUNBND
      RETURN
45    CONTINUE
      CALL ZRBNDY
      GO TO 98
      END
      SUBROUTINE BLDBND(L,BUFF)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: BLDBND         CONSTRUCT BOUNDARY VALUES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: MOVES INITIAL VALUES FROM BUFF (SIGMA STRIPS) TO
C   ALARGE AND BLARGE. ALARGE CONTAINS THE LOWER SIX ROWS OF THE
C   LFM GRID,AND THE TOP SIX ROWS AS FOLLOWS: ALARGE(I,J,K) I= 1 TO
C    53 , J= 1 TO 12, WHERE 1-6 ARE THE LOWER SIX J ROWS AND 7 - 12
C   CONTAIN THE TOP SIX J ROWS, K = 1 TO 23, US(1-7), VS(8-14),
C   TS(15-21), AND PSIG(22-23).
C   BLARGE CONTAINS THE REMAINING SIDE ROWS AS FOLLOWS: BLARGE(I,J,K)
C   I= 1 TO 12, WHERE 1-6 ARE THE LEFT SIDE OF THE GRID AND 7-12
C   CONTAIN THE RIGHT SIDE OF THE GRID,J = 1 TO JMAX-12, CORRESPONDING
C   TO JROWS 7 THROUGH JMAX-6, K = 1 TO 23 SAME AS USE IN ALARGE.
C
C   *****************************************************************
C   *                                                               *
C   *               ALARGE I= 1 , 53                                *
C   *                      J= 7 , 12                                *
C   *                                                               *
C   *****************************************************************
C   *            *                                      *           *
C   *  BLARGE    *                                      *  BLARGE   *
C   *            *            INTERIOR NO BOUNDARY      *           *
C   *  I= 1, 6   *            NUDGE                     *  I= 7,12  *
C   *  J= 7,40   *                                      *  J= 7,40  *
C   *            *                                      *           *
C   *            *                                      *           *
C   *            *                                      *           *
C   *****************************************************************
C   *                                                               *
C   *               ALARGE I= 1 , 53                                *
C   *                      J= 1 , 6                                 *
C   *                                                               *
C   *****************************************************************
C
C USAGE:  CALL BLDBND(L,BUFF)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     L           JROW COUNTER                              ARGUMENT
C     BUFF        SIGMA STRIP BUFFER FROM INPUT DISK FILE   ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      BOUNDARY VALUES AS DESCRIBED ABOVE        /PBNDPE/
C     BLARGE                 DITTO                          /PBNDPE/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        BUFF( 3816 )
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
C  SET UP POINTERS FOR THE SIGMA STRIPS STORED IN BUFF
C
      N1 = 53 * 36 + 1
      ND = 53 * 9
      N2 = N1 + ND
      N3 = N2 + ND
      N4 = N3 + ND
C
C MOVE VALUES FROM BUFF INTO ALARGE AND BLARGE
C
      IF(L.LT.7.OR.L.GT.  45 -6 )GO TO 10
      J = L - 6
      DO 1 K = 1,7
      KIND =  53 * (K-1)
      IBU  =  N1 + KIND
      DO 88890 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K)=BUFF(IBU+IQ2W6E-1)
88890 CONTINUE
      IBU=IBU+  53 -6
      DO 88900 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K)=BUFF(IBU+IQ2W6E-1)
88900 CONTINUE
      IBV= N2 +KIND
      DO 88910 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+7)=BUFF(IBV+IQ2W6E-1)
88910 CONTINUE
      IBV = IBV + 53 - 6
      DO 88920 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+7)=BUFF(IBV+IQ2W6E-1)
88920 CONTINUE
      IBT = N3 + KIND
      DO 88930 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+14)=BUFF(IBT+IQ2W6E-1)
88930 CONTINUE
      IBT = IBT + 53 - 6
      DO 88940 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+14)=BUFF(IBT+IQ2W6E-1)
88940 CONTINUE
      IF (K.GT.2) GO TO 1
      IBP = N4 + KIND
      DO 88950 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+21)=BUFF(IBP+IQ2W6E-1)
88950 CONTINUE
      IBP = IBP + 53 - 6
      DO 88960 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+21)=BUFF(IBP+IQ2W6E-1)
88960 CONTINUE
1     CONTINUE
      GO TO 20
10    J = L
      N53 =  53
      DO 15 K = 1,7
      NPLUS = (K-1) *  53
      IF (L.GT.45-6) J = L- 45 + 12
      DO 88970 IQ2W6E=1,N53
         ALARGE(IQ2W6E,J,K)=BUFF(N1+NPLUS+IQ2W6E-1)
         ALARGE(IQ2W6E,J,K+7)=BUFF(N2+NPLUS+IQ2W6E-1)
         ALARGE(IQ2W6E,J,K+14)=BUFF(N3+NPLUS+IQ2W6E-1)
88970 CONTINUE
      IF ( K.GT.2 ) GO TO 15
      DO 89000 IQ2W6E=1,N53
         ALARGE(IQ2W6E,J,K+21)=BUFF(N4+NPLUS+IQ2W6E-1)
89000 CONTINUE
15    CONTINUE
20    RETURN
      END
       SUBROUTINE DEL2XY(R,A)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DEL2XY         COMPUTES DEL SQUARED OF A
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: COMPUTES DEL SQUARED (SECOND ORDER) OF ARRAY A AND
C   STORES THE RESULTS IN R.  VALUES ARE COMPUTED WITH OUT REGARD
C   TO SCALING BY DX OR DT.
C
C USAGE:  CALL DEL2XY(R,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ...    INDEX POINTERS FOR SIZE OF GRID           /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     R           RESULT OF DEL SQUARED (A)                 ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        R(*),A(*)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
       ISTART = LI + 2
       IEND   = ( 45 - 2) * 53 + 52
       DO 1 I = ISTART,IEND
         R(I) = A(I-1) + A(I+1) + A(I+53) + A(I-53) - 4.E0 * A(I)
1      CONTINUE
       RETURN
       END
      SUBROUTINE DSXDY(SXDY,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSXDY          COMPUTE D(A)/DX BAR Y
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: COMPUTES D(A)/DX BAR Y OF A (FORTH ORDER) AND STORES
C   THE RESULTS IN SXDY. A AND SXDY ARE SIZE(LIXLJ)
C
C USAGE:  CALL DSXDY(SXDY,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     INDEX POINTERS FOR SIZE OF GRID           /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     DSXDY       RESULT OF D(A)/DX BAR Y                   ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      REAL HOUR1
      REAL SXDY(*),A(*)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
C
      DATA C243/0.6328125E0/, C27/ 7.03125E-2/, C9/ 2.34375E-2/,
     *  C1/2.6041667E-3/
C
C------ALL INTERIOR POINTS (FOURTH ORDER) FOLLOWS
      ISTART = LI + 2
      IEND   = LI*(LJ-2) - 2
      DO 10 I = ISTART,IEND
        SXDY(I) = C243*(A(I+LI+1)+A(I+LI)-A(I)-A(I+1))  +
     *            C27 *(A(I+2)+A(I-1)-A(I+LI+2)-A(I+LI-1))        +
     *            C9  *(A(I-LI+1)+A(I-LI)-A(I+2*LI+1)-A(I+2*LI))  +
     *            C1  *(A(I+2*LI+2)+A(I+2*LI-1)-A(I-LI+2)-A(I-LI-1))
10    CONTINUE
C______NOW THE EDGES OF THE GRID SECOND ORDER
C------LEFT EDGE FIRST
      IEND = IEND + 3
      DO 20 I = 1,IEND,  53
        SXDY(I) = .5E0*(A(I+LI+1)-A(I+1)+A(I+LI)-A(I))
20    CONTINUE
C------RIGHT EDGE OF GRID FOLLOWS
      IEND = IEND + LI -2
      DO 30 I = LI1,IEND,  53
        SXDY(I) = .5E0*(A(I+LI+1)-A(I+1)+A(I+LI)-A(I))
30    CONTINUE
C------BOTTOM ROW OF GRID FOLLOWS
      DO 40 I = 1,LI
        SXDY(I) = .5E0*(A(I+LI+1)-A(I+1)+A(I+LI)-A(I))
40    CONTINUE
C------AND FINALLY THE TOP ROW OF THE GRID
      ISTART = LI * (LJ-2) + 1
      IEND   = ISTART + LI - 2
      DO 50 I = ISTART,IEND
        SXDY(I) = .5E0*(A(I+LI+1)-A(I+1)+A(I+LI)-A(I))
50    CONTINUE
      RETURN
      END
      SUBROUTINE DSXY(SXY,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSXY           FORTH ORDER BAR
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 18 AUG 83
C
C ABSTRACT: COMPUTES BAR XY (FORTH ORDER) OF ANY ARRAY A AND STORES
C   THE RESULTS IN SXY.
C
C USAGE:  CALL DSXY(SXY,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF GRID A AND SXY       /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SXY         RESULT OF A BAR XY                        ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        SXY(*), A(*)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
      DATA C81/3.1640625E-1/,  C9/-3.515625E-2/,
     A     C1 /3.90625E-3/
C
C----------  FIRST DO ALL INTERIOR POINTS FORTH ORDER
         ISTART = LI + 2
         IEND  = LI * (LJ-2) - 2
         DO 10 I = ISTART,IEND
         SXY(I) = C81*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1)) +
     A            C9 *(A(I+LI-1)+A(I+LI+2)+A(I-1)+A(I+2)+A(I+2*LI) +
     B                 A(I+2*LI+1)+A(I-LI)+A(I-LI+1)) +
     C            C1 *(A(I+2*LI-1)+A(I+2*LI+2)+A(I-LI-1)+A(I-LI+2))
10       CONTINUE
C
C------ NOW THE EDGES OF THE GRID
C
C-----  LEFT EDGE FIRST
C
         IEND = IEND + 3
         DO 20 I = 1,IEND, 53
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
20       CONTINUE
C------ RIGHT EDGE HERE
         IEND = IEND + LI - 2
         DO 30 I = LI1,IEND, 53
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
30       CONTINUE
C----- BOTTOM ROW OF THE GRID
         DO 40 I = 1,LI1
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
40       CONTINUE
C------ TOP ROW OF THE GRID
         ISTART = LI * (LJ-2) + 1
         IEND   = ISTART +  LI - 2
         DO 50 I = ISTART,IEND
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
50       CONTINUE
         RETURN
       END
       SUBROUTINE TENXY(SXY,A)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENXY          FORTH ORDER BAR XY OF BAR XY INPUT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: COMPUTES BAR XY (FORTH ORDER) OF ANY ARRAY A, WHERE
C   A CONTAINS VALUES AT THE CENTER OF THE GRID BOXES. THIS ROUTINE
C   IS USED TO FORM THE FINAL XY BAR ON THE TENDENCIES.
C
C USAGE:  CALL TENXY(SXY,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF GRID A AND SXY       /INDEX/
C     A           ANY (LIXLJ) ARRAY WITH VALUES AT THE      ARGUMENT
C                 CENTER OF GRID BOXES.
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SXY         RESULT OF A BAR XY                        ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        SXY(*),A(*)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      SAVE
C
      DATA C81/3.1640625E-1/,  C9/-3.515625E-2/,
     A     C1 /3.90625E-3/
C
C----------  FIRST DO ALL INTERIOR POINTS FORTH ORDER
         LIP    = LI + 1
         ISTART = LI + 2
         IEND   = LI * (LJ-3) - 3
         DO 10 I = ISTART,IEND
         SXY(I+LIP) = C81*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1)) +
     A            C9 *(A(I+LI-1)+A(I+LI+2)+A(I-1)+A(I+2)+A(I+2*LI) +
     B                 A(I+2*LI+1)+A(I-LI)+A(I-LI+1)) +
     C            C1 *(A(I+2*LI-1)+A(I+2*LI+2)+A(I-LI-1)+A(I-LI+2))
10       CONTINUE
C
C------ NOW THE EDGES OF THE GRID
C
C-----  LEFT EDGE FIRST
C
         IEND = IEND + 3
         DO 20 I = 1,IEND, 53
           SXY(I+LIP) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
20       CONTINUE
C------ RIGHT EDGE HERE
         ISTART = LI - 2
         IEND   = IEND + LI - 2
         DO 30 I = ISTART,IEND,  53
           SXY(I+LIP) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
30       CONTINUE
C----- BOTTOM ROW OF THE GRID
         IEND = LI - 2
         DO 40 I = 1,IEND
           SXY(I+LIP) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
40       CONTINUE
C------ TOP ROW OF THE GRID
         ISTART = LI*(LJ-3) + 1
         IEND   = ISTART +  LI - 3
         DO 50 I = ISTART,IEND
           SXY(I+LIP) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
50       CONTINUE
       RETURN
       END
       SUBROUTINE DSXY2N(SXY,A)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSXY2N         COMPUTE A BAR XY SECOND ORDER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 06 JUL 83
C
C ABSTRACT: COMPUTES A BAR XY (SECOND ORDER) AND STORES THE RESULT
C   IN SXY. NO SCALING FOR DX
C
C USAGE:  CALL DSXY2N(SXY,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF GRID                 /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SXY         RESULTS (LIXLJ) FROM A BAR XY             ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        SXY(*),A(*)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      SAVE
C
      IEND = LI * (LJ-1) - 1
      DO 1 I = 1,IEND
        SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
1     CONTINUE
      RETURN
      END
      SUBROUTINE DSYDX(SYDX,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSYDX          COMPUTES D(A)/DX BAR Y
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: COMPUTES D(A)/DX BAR Y (FORTH ORDER) AND STORES THE
C   RESULTS IN SYDX. NO SCALING FOR DX.
C
C USAGE:  CALL DSYDX(SYDX,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF THE GRID             /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SYDX        RESULT OF D(A)/DX BAR Y                   ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        SYDX(*),A(*)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
      DATA C243/0.6328125E0/, C27/-7.03125E-2/, C9/-2.34375E-2/,
     *     C1  /2.6041667E-3/
C
C------ALL INTERIOR POINTS (FOURTH ORDER) FOLLOWS
      ISTART = LI + 2
      IEND = LI*(LJ-2) - 2
      DO 10 I = ISTART,IEND
        SYDX(I) = C243*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))  +
     *            C27 *(A(I-LI+1)-A(I-LI)+A(I+2*LI+1)-A(I+2*LI))  +
     *            C9  *(A(I+2)-A(I-1)+A(I+LI+2)-A(I+LI-1))  +
     *            C1  *(A(I-LI+2)-A(I-LI-1)+A(I+2*LI+2)-A(I+2*LI-1))
10    CONTINUE
C______NOW THE EDGES OF THE GRID SECOND ORDER
C------LEFT EDGE FIRST
      IEND = IEND + 3
      DO 20 I = 1,IEND,  53
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
20    CONTINUE
C------RIGHT EDGE OF GRID FOLLOWS
      IEND = IEND + LI -2
      DO 30 I = LI1,IEND,  53
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
30    CONTINUE
C------BOTTOM ROW OF GRID FOLLOWS
      DO 40 I = 1,LI
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
40    CONTINUE
C------AND FINALLY THE TOP ROW OF THE GRID
      ISTART = LI*(LJ-2) + 1
      IEND   = ISTART + LI - 2
      DO 50 I = ISTART,IEND
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
50    CONTINUE
      RETURN
      END
      SUBROUTINE FCST
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FCST           FORECAST FOR ONE TIME  STEP
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: FORECAST LFM FOR ONE TIME STEP.
C
C
C USAGE:  CALL FCST
C
C
C  - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      ARRAY OF HORIZONTAL BOUNDARY              /PBNDPE/
C     BLARGE      VALUE QUANTITIES (IN AND OUT)
C
C     AA,BB       HORIZ. BOUNDARY VALUE INCREMENTS          /PBNDRY/
C     ALP         TIME SMOOTHER FACTOR                      /TTME/
C     CSLDL,SSLDL COS, SIN SOLAR DECLINATION
C     CSLHR,SSLHR COS, SIN SOLAR HOUR ANGLE
C     IHOUR       CURRENT HOUR OF FORECAST
C     IODD        ODD EVEN TIME STEP FLAG
C     ITSW        TIMESTEP TYPE FLAG
C                 1=FORWARD(INITIAL), 4=CENTERED W/ SMOOTHING
C     MNSTEP      COUNT OF TIME STEP WITHIN HOUR
C                 (=-1 FOR FORWARD, =0 FOR DISASTER)
C     NPHOUR      NO. OF TIMESTEPS PER HOUR
C     BTHICH      BOUND LAYER PRESSURE THICK / 2.           /CNST/
C     BTHICK      BOUND LAYER PRESS THICK
C     RDELX       1/(GRID LENGTH) KM**-1
C     IHR1        CURRENT HOUR OF FORECAST (=IHOUR)         /FDATE/
C     IOUT        HOUR TO STOP THIS FORECAST SEGMENT
C     HOUR1       CURRENT FORECAST HOUR (=IHOUR)            /SVHOUR/
C     LUNS06,S12  UNIT NUMBERS FOR OUTPUT FILES             /FORTAP/
C     LUNV06,V12  FOR SIGMA AND OMEGA OUTPUT
C     LABS00(3)   ON.85 DATE WORD
C     US,VS,TS,   ALL THE FORECAST AND CONSTANT FIELDS      /DEABLK/
C     PSIG,WS,ETC UPDATED AND ADJUSTED
C     LOLD,LMID,LNEW  ROW COUNTERS FOR TIME STEPS           /INDEX/
C     K7OLD...    K COUNTERS FOR 7 LAYER VERTICAL INDEX     /INDEX/
C     K3OLD...        DITTO      3        DITTO
C     K2OLD...        DITTO      2        DITTO
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS,TS    FORECAST VARIABLES                        /DEABLK/
C     WS, ETC.    CONVECTIVELY ADJUSTED AND BOUNDARYIZED
C     COSZEN      COS(SOLAR ZENITH ANGLE)
C     LABS06/12   ON. 85 LABLES FOR                    UNIT LUNS06/S12
C     LABV06/12   SIGMA AND VVEL OUTPUT FILES               LUNV06/V12
C     HOUR1       DATE TIME FOR  SIGMA                      LUNS06/S12
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C      NFBO1,   SATM,  TENPT,  TENUV,                        LFM
C      DSXY, WATER, STATS, SETBC, TSMO, WRTAPE               LFM
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      LOGICAL     IFWRTE
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      CHARACTER*4 WASH,INGT,ON,LFMS,LFMV,ZERO,LABS2(8)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53, 45, 8 )
      SAVE
C
C     PIECES OF ON85 LABELS.
C
C
      DATA  WASH  /'WASH'/, INGT/'INGT'/, ON/'ON  '/,
     A      LFMS  /'LFMS'/, LFMV/'LFMV'/,
     B      ZERO  /'0000'/
      DATA  LABS2 /'06B2','12B2','18B2','24B2',
     A             '30B2','36B2','42B2','48B2'/
C
C         SET UP IWO,IPO
C         IWO IS FOR VERTICAL VELOCITY, IPO FOR SIGMA STRIPS
C           ROWS ARE OUTPUT EVERY SIX HOURS ON THE HOUR
C
C     WHEN IFWRTE = .FALSE. DONT WRITE VVEL OR SIGMA STRIPS
C
       IFWRTE = .FALSE.
       IF (MOD(IHOUR+1,6).NE.0) GO TO 931
       IF (MNSTEP.NE.NPHOUR) GO TO 931
       IFWRTE = .TRUE.
C
C     I6, I12 ARE INDICES IN LABS2 TO CHOOSE CORRECT FILE LABEL
C     FOR UNITS LUNS06, LUNS12, LUNV06 AND LUNV12.
C
         I6  = (IHOUR+1)/6
         I12 = I6
         IF (MOD(IHOUR+1,12).NE.0) GO TO 902
           IWO       = LUNV12
           IPO       = LUNS12
           LABS12(1) = LFMS
           LABS12(2) = LABS2(I12)
           LABS12(3) = LABS00(3)
           LABS12(4) = ZERO
           LABS12(5) = ZERO
           LABS12(6) = WASH
           LABS12(7) = INGT
           LABS12(8) = ON
           LABV12(1) = LFMV
           LABV12(2) = LABS2(I12)
           LABV12(3) = LABS00(3)
           LABV12(4) = ZERO
           LABV12(5) = ZERO
           LABV12(6) = WASH
           LABV12(7) = INGT
           LABV12(8) = ON
C
C     NOW WRITE LABELS.
C
           WRITE (LUNS12) LABS12
           WRITE (LUNV12) LABV12
           GO TO 933
902      CONTINUE
           IWO       = LUNV06
           IPO       = LUNS06
           LABS06(1) = LFMS
           LABS06(2) = LABS2(I6)
           LABS06(3) = LABS00(3)
           LABS06(4) = ZERO
           LABS06(5) = ZERO
           LABS06(6) = WASH
           LABS06(7) = INGT
           LABS06(8) = ON
           LABV06(1) = LFMV
           LABV06(2) = LABS2(I6)
           LABV06(3) = LABS00(3)
           LABV06(4) = ZERO
           LABV06(5) = ZERO
           LABV06(6) = WASH
           LABV06(7) = INGT
           LABV06(8) = ON
C
C     NOW WRITE LABELS.
C
           WRITE (LUNS06) LABS06
           WRITE (LUNV06) LABV06
  933    CONTINUE
931    CONTINUE
       IVEL   = 0
       RMSSFC = 0.0E0
       RMSTRP = 0.0E0
C
C  SET IVEL = 1, TWO HOURS PRIOR TO POST TIME FOR EVEN TIME STEPS
C  IVEL IS THE FLAG FOR COMPUTING VERT VELS, 1 = YES, 0 = NO
C
       IF ((IHOUR+2-IOUT) .LT. 0) GO TO 31
       IF (IODD .EQ. 2) IVEL = 1
       IF (IHR1 .EQ.0 .OR. MOD(IOUT,6) .NE.0) IVEL = 0
31     CONTINUE
C
C   ZERO PRECIP FIELD EVER 12 HOURS, PRECIP COLLECTED EACH 12 HOURS
C
       IF (.NOT.(MNSTEP.LT.2.AND.MOD(IHOUR,12).EQ.0)) GO TO 30
C
       DO 29 J = 1,  45
       DO 29 I = 1,  53
29     PREC(I,J) = 0.0E0
C
30     CONTINUE
C
C  NO PRECIP COLLECTED DURING FIRST 4 HOURS, (ENGINEERING FOR CONV ADJ)
C
       IF (IHOUR .GT. 3) GO TO 8
       DO 9 J = 1,45
       DO 9 I = 1,53
         PREC(I,J) = 0.0E0
9      CONTINUE
8      CONTINUE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C     ADVANCE PE BOUNDARIES IF NOT FIRST TIME STEP
C
      IF (MNSTEP.LT.0) GO TO 700
      DO 701 K = 1,23
      DO 701 J = 1,12
      DO 701 I = 1,53
        ALARGE(I,J,K) = ALARGE(I,J,K) + AA(I,J,K)
  701 CONTINUE
      DO 702 I = 1,23
      DO 702 J = 1,33
      DO 702 K = 1,12
        BLARGE(K,J,I) = BLARGE(K,J,I) + BB(K,J,I)
  702 CONTINUE
C    CHECK FOR THIN POINTS
      DO 738 K = 1,12
      DO 738 I = 1,  53
      IF (ALARGE(I,K,23).GE.15.E0) GO TO 750
      CHP = 15.E0- ALARGE(I,K,23)
      ALARGE(I,K,22) = ALARGE(I,K,22) - CHP
      ALARGE(I,K,23) = 15.E0
750   CONTINUE
      IF ( I.GT.33 ) GO TO 738
      IF (BLARGE(K,I,23).GE.15.E0) GO TO 760
      CHP = 15.E0- BLARGE(K,I,23)
      BLARGE(K,I,22) = BLARGE(K,I,22) - CHP
      BLARGE(K,I,23) = 15.E0
760   CONTINUE
738   CONTINUE
700   CONTINUE
      DO 1500 I = 1,53
      DO 1500 L = 1,12,11
      DO 1500 K = 1,14
      ALARGE(I,L,K)=SIGN(MIN(ABS(ALARGE(I,L,K)),120.E0),ALARGE(I,L,K))
      IF ( I.GT.33 ) GO TO 1500
      BLARGE(L,I,K)=SIGN(MIN(ABS(BLARGE(L,I,K)),120.E0),BLARGE(L,I,K))
1500  CONTINUE
      DO 1510 I = 1,53,52
      DO 1510 L = 2,11
      DO 1510 K = 1,14
        ALARGE(I,L,K) = SIGN(MIN(ABS(ALARGE(I,L,K)),120.E0),
     A     ALARGE(I,L,K))
1510  CONTINUE
C
C / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C
C     LOLD,LMID,LNEW ARE TIME COUNTERS WHICH CYCLICALLY TAKE
C     ON THE VALUES 0,1,2.
C     K7OLD,K7MID,K7NEW REFER TO VALUES OF THIRD INDEX.
C     K3OLD,K3MID,K3NEW            DITTO
C     K2OLD,K2MID,K2NEW            DITTO
C
      K = 1
      K7OLD = K + LOLD * 7
      K7MID = K + LMID * 7
      K7NEW = K + LNEW * 7
      K3OLD = K + LOLD * 3
      K3MID = K + LMID * 3
      K3NEW = K + LNEW * 3
      K2OLD = K + LOLD * 2
      K2MID = K + LMID * 2
      K2NEW = K + LNEW * 2
C
C     COMPUTE LATITUDE AND LONGITUDE FOR INITIAL TIME AND ALL RECOVERYS
C     AT CENTER OF GRID SQUARES, SINCE (I,JCOUNT) (IN TEND1)
C     REFERS TO THE LOWER LEFT CORNER OF A GRID SQUARE ADJUST II + JJ
C
      LIHLF =  53 / 2
      IF (MNSTEP.LT.0) GO TO 14
      IF (MNSTEP.EQ.0) GO TO 13
      IF (MNSTEP.EQ.1) GO TO 17
      IF (MNSTEP.GE.2) GO TO 19
   13 CONTINUE
      PRINT 600
  600 FORMAT(//10X,'0MNSTEP = 0 IN FCST AT FORMAT 600'//)
      CALL ERREXT
 14   CONTINUE
C
C  MOVE TAU-1 LEVEL INTO TAU LEVEL AT INITIAL STEP ONLY
C
      N3IJ = 3 * 2385
      N7IJ = 7 * 2385
      N2IJ = 2 * 2385
      DO 88890 IQ2W6E=1,N3IJ
         WS(IQ2W6E,1,K3MID)=WS(IQ2W6E,1,K3OLD)
88890 CONTINUE
      DO 88900 IQ2W6E=1,N7IJ
         TS(IQ2W6E,1,K7MID)=TS(IQ2W6E,1,K7OLD)
         US(IQ2W6E,1,K7MID)=US(IQ2W6E,1,K7OLD)
         VS(IQ2W6E,1,K7MID)=VS(IQ2W6E,1,K7OLD)
88900 CONTINUE
      DO 88930 IQ2W6E=1,N2IJ
         PSIG(IQ2W6E,1,K2MID)=PSIG(IQ2W6E,1,K2OLD)
88930 CONTINUE
C
C  CALCULATE LATITUDE AND LONGITUDE FIRST STEP ONLY
C
      DO 15 JCOUNT = 1,        44
      XJ = JCOUNT
      XJ = XJ + .5E0
      DO 15 I=1,LIHLF
      XI = I
      XI = XI + .5E0
15    CALL NFB01(XI,XJ,ALAT(I,JCOUNT),ALAT(I+LIHLF,JCOUNT),
     1     ALON(I,JCOUNT),ALON(I+LIHLF,JCOUNT))
C
C COMPUTE MAP FACTOR SQUARED
C
      DO 28 JCOUNT = 1,45
      DO 28 I=1,  53
        MF(I,JCOUNT) = REAL(1.86603E0/(1.E0+F(I,JCOUNT)))**2
28    CONTINUE
C
C------ FORM XYBAR ON ST,ICE, AND CD AND STORE BACK IN ORIGINAL----
C
      IF(IHOUR .NE. 0) GO TO 17
      NIJ = 2385
      CALL DSXY2N(US(1,1,K7NEW),ST)
      DO 88940 IQ2W6E=1,NIJ
         ST(IQ2W6E,1)=US(IQ2W6E,1,K7NEW)
88940 CONTINUE
      CALL DSXY2N(US(1,1,K7NEW),ICE)
      DO 88950 IQ2W6E=1,NIJ
         ICE(IQ2W6E,1)=US(IQ2W6E,1,K7NEW)
88950 CONTINUE
      CALL DSXY2N(US(1,1,K7NEW),CD)
      DO 88960 IQ2W6E=1,NIJ
         CD(IQ2W6E,1)=US(IQ2W6E,1,K7NEW)
88960 CONTINUE
   17    CONTINUE
C
C     COMPUTE COSINE OF SOLAR ZENITH ANGLE AT THE CENTER OF EACH GRID SQ
C        COSINE ZENITH ANGLE ONCE EACH HOUR
C
      DO 18 I=1,LIHLF
      DO 18 JCOUNT = 1,        44
      COSZEN(I,JCOUNT)=SSLDC*ALAT(I,JCOUNT)+CSLDC*ALAT(I+LIHLF,JCOUNT) *
     1(CSLHR*ALON(I+LIHLF,JCOUNT)+SSLHR*ALON(I,JCOUNT))
      IF(COSZEN(I,JCOUNT).LE.0.17365E0)COSZEN(I,JCOUNT) = 0.0E0
      COSZEN(I+LIHLF,JCOUNT) = SSLDC*ALAT(LIHLF+1-I,JCOUNT)+CSLDC*ALAT
     1(  53 -I,JCOUNT)*(CSLHR*ALON(  53 -I,JCOUNT)-SSLHR*
     A     ALON(LIHLF+1-I,JCOUNT))
      IF(COSZEN(I+LIHLF,JCOUNT).LE.0.17365E0)COSZEN(I+LIHLF,JCOUNT)
     A  = 0.0E0
 18   CONTINUE
      LJ22 =  45  - 2
      DO 7011 JCOUNT = 1,45
        COSZEN(  53 ,JCOUNT) = 0.0E0
        ALON(  53 ,JCOUNT) = 0.0E0
        ALAT(  53 ,JCOUNT) = 0.0E0
7011  CONTINUE
C
C  ZERO RADIATION HEATING/COOLING START OF EACH HOUR
C
      DO 75 K = 1,3
      DO 75 J = 1,45
      DO 75 I = 1,53
75    H2(I,J,K) = 0.0E0
 19   CONTINUE
C
C        SKIP TO HERE FOR OTHER  THAN FIRST STEP IN HOUR
C ADVANCE MASS VARIABLES AND MOISTURE, TS,WS, AND PSIG FORWARD
C  INCLUDING MOIST PHYSICS (SUB WATER) GRID AND SUB-GRID SCALE
C
      DO 99999 K = 1, 7
      K7OLD = K + LOLD * 7
      K7MID = K + LMID * 7
      K7NEW = K + LNEW * 7
      CALL TENPT
      IF ( K.NE.7 ) GO TO 99999
      CALL WATER
99999 CONTINUE
C
C  COMPUTE INTEGRATION STATISTICS EACH TIME STEP
C
      CALL STATS
C
C  CALCULATE VALUES OF SATURATED PREC WATER (SCALED BY SATRH)
C
      CALL SATM
C
C  ADVANCE MOMENTUM (US AND VS) FORWARD IN TIME
C
      DO 99998 K = 1, 7
      K7OLD = K + LOLD*7
      K7MID = K + LMID*7
      K7NEW = K + LNEW*7
      CALL TENUV
99998 CONTINUE
C
C  SET LATERAL BOUNDARY CONDITIONS ON US AND VS
C
      CALL SETBC
C
C  TIME SMOOTH US,VS,TS,WS, AND PSIG ALL BUT FIRST TIME STEP
C
      IF ( ITSW .EQ. 4 ) CALL TSMO
C
C  WRITE FORECAST (SIGMA STRIPS) TO DISK EVERY 6 (POST INTERVAL) HRS
C
      IF (IFWRTE) CALL WRTAPE
C
       RETURN
       END
       SUBROUTINE STATS
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: STATS          COMPUTES INTEGRATION STATISTICS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: COMPUTES INTEGRATION STATISTICS FOR MONITORING THE
C   LFM FORECAST.  SIGDOT AND PSIG RMS VALUES ARE COMPUTED EVERY
C   TIME STEP, THE REMAINING QUANTITIES ARE COMPUTED ON THE HOUR
C
C USAGE:  CALL STATS
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ROCP        R/CP                                      /CNST/
C     PSIG        DP/DSIGMA (MB)                            /DEABLK/
C     ZSTAR       TERRAIN HEIGHT  (METERS)                  /DEABLK/
C     PHI         GEOPOTENTIAL (M**2/SEC**2) LAYER VALUES   /GEOPOT/
C     PI          EXNER FUNCTION (P/1000)**R/CP  DITTO      /GEOPOT/
C                 HAS PIBAR(BROWN-PHILLIPS INCLUDED)
C     TS          POTENTIAL TEMPERATURE (K)                 /DEABLK/
C     US,VS       WIND COMPONENTS  (M/SEC)                  /DEABLK/
C     SIGDOT      D(SIGMA)/DT INTERFACE VALUES              /VTEMP/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SUMZ        GEOPOTENTIAL SUMMED OVER GRID DOMAIN      /DIAG/
C     SUMP        PRESSURE                  DITTO           /DIAG/
C     SUMTS       POT. TEMP.                DITTO           /DIAG/
C     SUMF        MAP SCALE                 DITTO           /DIAG/
C     SUMK        KIN. ENERGY               DITTO           /DIAG/
C     SUMP2       POTENTIAL ENERGY          DITTO           /DIAG/
C     SUMS        STATIC STABILITY          DITTO           /DIAG/
C     SUMVOR      SQUARED VORTICITY         DITTO           /DIAG/
C     SUMDIV       DITTO  DIVERGENCE        DITTO           /DIAG/
C     RMSSFC      RMS SURFACE PRESSURE CHANGE / TIME STEP   /DIAG/
C     RMSTRP      RMS TROP               DITTO              /DIAG/
C     SIGB1       RMS D(SIGMA)/DT TOP OF BOUNDARY LAYER     /DIAG/
C     SIGT1-T2    RMS    DITTO    TROPOSPHERE               /DIAG/
C     SIGS1-S2    RMS    DITTO    STRATOSPHERE              /DIAG/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        PHIL(8),DPP(8)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ), PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12,23), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5), VT( 53 , 45 ,25)
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53, 45, 7 ), PI( 53, 45, 7 )
      SAVE
C
C     DO SUMS FOR ENERGY STATISTIC COMPUTATIONS
      IF (MNSTEP.NE.NPHOUR) GO TO 33
C
      THING = 1.E0/(1.E0+ROCP)
      A = PSIG(I,J,K2MID+1) * 0.1E0
      B = PSIG(I,J,K2MID  ) * 0.1E0
      C = 1.E0/ MF(I,J)
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=PSIG(IQ2W6E,1,K2MID+1)*0.1E0
         VT(IQ2W6E,1,21)=PSIG(IQ2W6E,1,K2MID)*0.1E0
         VT(IQ2W6E,1,22)=1.E0/MF(IQ2W6E,1)
         VT(IQ2W6E,1,12)=5.E0
         VT(IQ2W6E,1,11)=VT(IQ2W6E,1,12)+VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,10)=VT(IQ2W6E,1,11)+VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,9)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,8)=VT(IQ2W6E,1,9)+VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,7)=VT(IQ2W6E,1,8)+VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,6)=VT(IQ2W6E,1,7)+VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,5)=VT(IQ2W6E,1,6)+BTHICK*0.1E0
         VT(IQ2W6E,1,13)=BTHICK*0.1E0
         VT(IQ2W6E,1,14)=VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,15)=VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,16)=VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,17)=VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,18)=VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,19)=VT(IQ2W6E,1,20)
88890 CONTINUE
      DO 5 J = 2,44
      DO 5 I = 2,52
      PHIL(1) = ZSTAR(I,J)*9.8E0
      DO 3 K = 2, 8
      PHIL(K) = PHI(I,J,K-1 )*2.E0- PHIL(K-1)
    3 CONTINUE
      DO 6 K = 1, 6
      DPP(K) = (PI(I,J,K )*VT(I,J,K+4) - PI(I,J,K+1 )*VT(I,J,K+5))
     A      * THING
    6 CONTINUE
      DPP(7) = PI(I,J,7 )*VT(I,J,11) * THING
      DO 2 K = 1,  7
      L7 = K + LMID * 7
      SUMZ(K) = SUMZ(K) + PHIL(K)*VT(I,J,22)
      SUMP(K) = SUMP(K) + VT(I,J,K+4) * VT(I,J,22)
      SUMTS(K) = SUMTS(K)+TS(I,J,L7)*VT(I,J,22)
      SUMF(K) = SUMF(K) + VT(I,J,22)
      SUMK(K) = (US(I,J,L7)*US(I,J,L7) + VS(I,J,L7)*VS(I,J,L7))
     A  *VT(I,J,K+12) + SUMK(K)
      SUMP1(K) = VT(I,J,K+4)*PHIL(K)*VT(I,J,22) +SUMP1(K)
      SUMP2(K) = TS(I,J,L7)*DPP(K)*VT(I,J,22) + SUMP2(K)
      DIFF = PI(I,J,1 )-PI(I,J,K )
      TS2 = TS(I,J,L7)
      TDIFF=TS2-TS(I,J,L7-1)
      SUMS(K) = DIFF*VT(I,J,K+4)*TDIFF*VT(I,J,22) + SUMS(K)
    2 CONTINUE
    5 CONTINUE
      DO 4 K = 1,  7
      L7 = K + LMID * 7
      LGT =  53 * 44
C-----------------------------------------------------------------------
C   THE CONSTANT 2.E0 IN THE TWO STATEMENTS BELOW IS ACTUALLY
C   0.5 X 4.0  (.5 FOR THE DERIVATIVES) AND 4.0 TO REMOVE THE
C   BARXY WHEN TENXY IS CALLED.  THIS IS TO INTRODUCE AN ERROR
C   THAT WAS IN THE ORIGINAL VERSION OF THE MODEL CAUSING THE
C   MEAN SQUARED DIVERGENCE AND VORTICITY STATISTICS TO CONFORM
C   TO THE ORIGINAL VERSION
C-----------------------------------------------------------------------
      DO 89070 IQ2W6E=1,LGT
         VT(IQ2W6E,1,1)=2.E0*(US(IQ2W6E+1,1,L7)-US(IQ2W6E,1,L7)+U
     *   S(IQ2W6E+1,2,L7)-US(IQ2W6E,2,L7)+VS(IQ2W6E+1,2,L7)-VS(IQ2W6E+
     *   1,1,L7)+VS(IQ2W6E,2,L7)-VS(IQ2W6E,1,L7))*RDELX
         VT(IQ2W6E,1,2)=2.E0*(VS(IQ2W6E+1,1,L7)-VS(IQ2W6E,1,L7)+V
     *   S(IQ2W6E+1,2,L7)-VS(IQ2W6E,2,L7)+US(IQ2W6E+1,2,L7)-US(IQ2W6E+
     *   1,1,L7)+US(IQ2W6E,2,L7)-US(IQ2W6E,1,L7))*RDELX
89070 CONTINUE
44    CONTINUE
      CALL TENXY(VT(1,1,3),VT(1,1,1))
      CALL TENXY(VT(1,1,4),VT(1,1,2))
      DO 45 J = 2,       44
      DO 45 I = 2,       52
      SUMVOR(K) = SUMVOR(K)+VT(I,J,K+12)*MF(I,J)*VT(I,J,4)*VT(I,J,4)
     1 /VT(I,J,5)
      SUMDIV(K) = SUMDIV(K)+VT(I,J,K+12)*MF(I,J)*VT(I,J,3)*VT(I,J,3)
     1 /VT(I,J,5)
      SUMFD(K) = SUMFD(K)+ VT(I,J,22)
 45   CONTINUE
    4 CONTINUE
   33 CONTINUE
C     ACCUMULATE RMS STATISTICS OF SFC/TROPOPAUSE PRESSURE CHANGE
C
      DO 50 J = 2, 44
      DO 50 I = 2, 52
        PSTRP  = 3.E0*(PSIG(I,J,K2NEW+1) - PSIG(I,J,K2MID+1))
        PSFC   = PSTRP+3.E0*(PSIG(I,J,K2NEW)-PSIG(I,J,K2MID))
        RMSSFC = RMSSFC + PSFC*PSFC
        RMSTRP = RMSTRP + PSTRP*PSTRP
50    CONTINUE
C     RMS SIGMA DOT OVER THE GRID
C      FOR RUNNING DTAGNOSTICS OF THE MOTION
      DO 40 J = 1,44
      DO 40 I = 1,52
        SIGB1 = SIGB1 + SIGDOT(I,J,1) * SIGDOT(I,J,1)
        SIGT1 = SIGT1 + SIGDOT(I,J,2) * SIGDOT(I,J,2)
        SIGT2 = SIGT2 + SIGDOT(I,J,3) * SIGDOT(I,J,3)
        SIGS1 = SIGS1 + SIGDOT(I,J,4) * SIGDOT(I,J,4)
        SIGS2 = SIGS2 + SIGDOT(I,J,5) * SIGDOT(I,J,5)
 40   CONTINUE
      RETURN
      END
      SUBROUTINE SATM
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SATM           COMPUTES MOISTURE SATURATION VALUES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: CALCULATES SATURATION PRECIPITAL WATER VALUES WHICH
C   ARE SCALED BY SATRH.
C
C USAGE:  CALL SATM
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LNEW..      TIME INDEX                                /INDEX/
C     TS          POT. TEMPERATURE (K)                      /DEABLK/
C     PI          EXNER FUNCTION (P/1000)**R/CP BAR BP      /GEOPOT/
C     PSIG        DP/D(SIGMA)  (MB)                         /DEABLK/
C     SATRH       SATURATION CRITERION (PERCENT X .01)      /CNST/
C     WS          PRECIPITAL WATER                          /DEABLK/
C     AX          TABLE OF SATURATION VALUES                /FASTER/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SATW        SATURATION PRECIPITAL WATER (CM)          /DEABLK/
C     WS          PRECIPITAL WATER  (CM)                    /DEABLK/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     MAX,MIN                                               FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12,23), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33, 23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53, 45, 7 ), PI( 53, 45, 7 )
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C--------------------------------------------------------------------
C  USE VT SCRATCH AS FOLLOWS:
C       VT(1)  -   T(1)
C       VT(2)  -   T(2)
C       VT(3)  -   T(3)
C       VT(4)  -   P(1)
C       VT(5)  -   P(2)
C       VT(6)  -   P(3)
C--------------------------------------------------------------------
C
      DATA CA/3.765578E0/,CB/2.270245E0/
      DATA BRH/0.3E0/
C
      NJ3 = 3 * 2385
      NJ2 = 2 * 2385
C  CONVERT POT. TEMP. TO TEMPERATURE AND FIND MID LAYER PRESSURES
C
      DO 88890 IQ2W6E=1,NJ3
         VT(IQ2W6E,1,1)=TS(IQ2W6E,1,1+LNEW*7)*PI(IQ2W6E,1,1)-273.16E0
88890 CONTINUE
      DO 88900 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,4)=50.E0+3.E0*PSIG(IQ2W6E,1,K2NEW+1)+3.E0 * 
     *   PSIG(IQ2W6E,1,K2NEW)+BTHICH
         VT(IQ2W6E,1,5)=VT(IQ2W6E,1,4)-BTHICH-.5E0*PSIG(IQ2W6E,1,K2NEW
     *   )
         VT(IQ2W6E,1,6)=VT(IQ2W6E,1,5)-PSIG(IQ2W6E,1,K2NEW)
88900 CONTINUE
C
C     TETENS FORMULA FOR SAT. VAP. PRESSURE
C     NUMBERS FROM MURRAY. (JAM, VOL. 6, NO. 1)
C     FOLLOWED BY STANDARD FORMULA   FOR SPECIFIC HUMIDITY
C     ARGUMENT OF EXPONENTIAL HAS PLUS 10 ADDED TO FORCE IT
C     TO BE POSITIVE
C     MURRAYS NUMBERS TIMES       E**-10 ( = 0.000045 ) TO COMPENSATE
C     T(K) = 6.1078*EXP (17.269*T(K) /(T(K)  + 237.3))
C     T(K) = .622 * T(K)/(P(K) - .375*T(K))
C
C  USE TABLE OF SAT. VALUES WHICH ARE STORED IN AX
C
      DO 9012 K = 1,3
      DO 9012 J = 1, 45
      DO 9012 I = 1, 53
       KT = VT(I,J,K) + 71.E0
       KT = MAX(KT,1)
       KT = MIN(KT,120)
9012  VT(I,J,K+6) = AX(KT)
      DO 88930 IQ2W6E=1,NJ3
         VT(IQ2W6E,1,1)=CA*VT(IQ2W6E,1,7)/(VT(IQ2W6E,1,4)-CB*VT(IQ2W6E
     *   ,1,7))
88930 CONTINUE
C
C  51.020408 EQUALS 50/0.98 FOR BOUNDARY LAYER THICKNESS SCALING
C
      DO 88940 IQ2W6E=1,NIJ
         SATW(IQ2W6E,1,1)=SATRH*VT(IQ2W6E,1,1)*51.020408E0
         VT(IQ2W6E,1,7)=PSIG(IQ2W6E,1,K2NEW)/0.98E0
         VT(IQ2W6E,1,8)=VT(IQ2W6E,1,7)
88940 CONTINUE
      DO 88970 IQ2W6E=1,NJ2
         SATW(IQ2W6E,1,2)=SATRH*VT(IQ2W6E,1,2)*VT(IQ2W6E,1,7)
88970 CONTINUE
      DO 299 J = 2,44
      DO 299 I = 2,52
C     DONT ALLOW REL HUM BELOW BRH PERCENT IN BOUNDARY LAYER
C      BUT ONLY OVER OPEN WATER
      IF (XM(I,J).EQ.0.E0) GO TO 299
      WS(I,J,K3NEW) = MAX(WS(I,J,K3NEW) , BRH * SATW(I,J,1))
      WS(I,J,K3MID) = MAX(WS(I,J,K3MID) , BRH * SATW(I,J,1))
  299 CONTINUE
      RETURN
      END
      SUBROUTINE WRTAPE
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: WRTAPE         WRITES SIGMA STRIPS AND VERT VELS TO DISK
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: CHANGES FORECAST QUANTITIES TO SIGMA STRIP FORMAT AND
C   WRITES THE STRIPS TO DISK FOR RESTART OR USE BY POST PROCESSOR
C
C USAGE:  CALL WRTAPE
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNV06-12   LOGICAL UNIT NUMBERS FOR VERT VEL OUTPUT  /FORTAP/
C     LUNS06-12            DITTO           SIGMA STRIP      /FORTAP/
C     HOUR1       RECORD LABEL                              /SVHOUR/
C     IVVEL       VERTICAL VELOCITIES (2 HR AVERAGE)        /VERTV/
C     LOLD..      TIME INDICIES                             /INDEX/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     HOUR1       LABEL FOR SIGMA STRIPS                    LUNS06-12
C     WO          VERTICAL VELOCITIES (ONE SLAB)            LUNV06-12
C     BUFF        SIGMA STRIP (ONE SLAB)                    LUNS06-12

C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     MOD                                                   FORTLIB
C     ARYTOT                                                LFM
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        WO( 53 ,7), BUFF( 3816 )
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE(12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 , 23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C  IWO AND IPO POINT TO PROPER LOGICAL UNIT NUMBERS FOR OUTPUT
C
       IWO = LUNV12
       IPO = LUNS12
       IF (MOD(IHOUR+1,12) .EQ. 0 ) GO TO 10
         IWO = LUNV06
         IPO = LUNS06
10     CONTINUE
C
C  INCREMENT AND STORE FORECAST HOUR IN LABEL RECORD
C
       HOUR1(1) = IHOUR + 1
       WRITE ( IPO  ) HOUR1
       N7I = 7 *  53
C  WRITE VERT VELS TO DISK (FIRST AND LAST SLAB ARE ZERO)
C
      DO 88890 IQ2W6E = 1,N7I
         WO(IQ2W6E,1) = 0.0E0
88890 CONTINUE
       WRITE ( IWO ) WO
       DO 11 J = 2,44
       DO 12 K = 1,7
       DO 12 I = 2,52
       WO(I,K) = IVVEL(I,J,K) * 1.E-3
12     CONTINUE
       WRITE (IWO) WO
11     CONTINUE
      DO 88900 IQ2W6E = 1,N7I
         WO(IQ2W6E,1) = 0.0E0
88900 CONTINUE
       WRITE (IWO) WO
C
C SIGMA STRIPS CONTAIN VALUES AT TAU AND TAU-1 PLUS ALL FIXED FIELDS
C USE ARYTOT TO BUILD THE STRIPS AND THEN WRITE THEM TO IPO
C
       LTT  = LOLD
       LOLD = LMID
       LMID = LNEW
       DO 20 J = 1, 45
         CALL ARYTOT(J,BUFF)
         WRITE (IPO) BUFF
20     CONTINUE
       LMID = LOLD
       LOLD = LTT
       RETURN
       END
      SUBROUTINE RDTAPE
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: RDTAPE         READS INITIAL OR RESTART SIGMA STRIPS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: READS SIGMA STRIPS INTO BUFF.  USING SUBROUTINE TTOARY
C   THE STRIPS ARE CONVERTED AND STORED IN COMMON BLOCK /DEABLK/.
C
C USAGE:  CALL RDTAPE
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNS00      LOGICAL UNIT #, SIGMA STRIPS              /FORTAP/
C     LABS00      SIGMA STRIP LABEL                         /FORTAP/
C     HOUR1       SIGMA STRIP LABEL RECORD (FORECAST HR)    /FORTAP/
C     BUFF        SIGMA STRIP INPUT BUFFER                  LUNS00
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LOLD...     TIME LEVEL POINTERS                       /INDEX/
C     IHOUR       FORECAST HOUR OF INPUT SIGMA STRIPS       /TTME/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TTOARY                                                LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        BUFF( 3816 )
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      COMMON /VTEMP/ SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C   THE TAPE HAS 1 ROW OF TAU, TAU-1 IN EACH RECORD.
C   THAT IS LI * (9 LEVELS) * (4 VARIABLES) * (4 BYTES PER WORD) *
C   (2 TIME LEVELS)
C     LOLD, LMID, LNEW - TIME LEVELS FOR BOUNDARY FILES INITIALIZED.
C
      LOLD = 0
      LMID = 1
      LNEW = 2
      READ (LUNS00) LABS00
C
      READ (LUNS00) HOUR1
C
      IHOUR = HOUR1(1)
      DO 10 LOOP = 1, 45
        READ (LUNS00) BUFF
        CALL TTOARY(LOOP,BUFF)
   10 CONTINUE
      REWIND LUNS00
      RETURN
      END
      SUBROUTINE FTIME
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FTIME          FORECAST LFM OUT TO IOUT HOURS
C   AUTHOR: DEAVEN           ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: FORECAST LFM FROM IHOUR OUT TO IOUT HOURS.
C   PRINT RUNNING ENERGY AND SIGMADOT STATISTICS ON THE HOUR.
C
C
C USAGE:  CALL FTIME
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     HOUR1(7)    SAT. REL HUMIDITY (PERCENT)               /SVHOUR/
C     IHOUR       STARTING HOUR OF FORECAST                 /TTME/
C     SSLHR,CSLHR SIN/COS SOLAR HOUR ANGLE
C     SHR,CHR     SIN/COS OF ONE HOUR
C     IODD        ODD/EVEN TIME STEP FLAG                   /FDATE/
C     IOUT        HOUR TO TERMINATE FORECAST
C     LOLD        TIME LEVEL COUNTERS                       /INDEX/
C     LMID        (SET IN RDTAPE AND
C     LNEW        USED IN FCST AND ARYTOT)
C     RMSSFC      SUM  OF SURFACE PRESSURE TEND             /DIAG/
C     RMSTRP      SUM OF TROP PRESSURE TEND
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SATRH       SAT REL HUM (PERCENT)                     UNIT 6
C     ITSW        TIME STEP TYPE SWITCH                     /TTME/
C     MNSTEP      COUNTER OF TIME STEPS IN A HOUR
C     SIGB1,SIGT1 SIGMADOT SUMMATIONS                       /DIAG/
C     SIGT2,SIGS1 ZEROED THEN PRINTED LATER
C     SIGS2       AFTER OTHER CODES DO SUMS
C     SUMZ,P,TS   ENERGY SUMMATIONS                         /DIAG/
C     P1,P2,S,F   LIKEWISE ZEROED, THEN
C     VOR, DIV    PRINTED LATER
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     FCST, SOLDEC, STARTF                                  MINE
C     SQRT,MOD,ENCODE                                       FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      LOGICAL     ISOP
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
C     CHARACTER*4 MSG(11)
C     DATA MSG/'NWSO','P,@1','0LFM',' FOR','ECAS','T RE','ACHE','D HO',
C    1'UR  ',0,'   :'/
C     CHARACTER*8 MESAGE(6)
C     DATA MESAGE/'        ',' RMS SFC',' PRESSUR',
C    & 'E CHANGE',' AT XX H','OURS::::'/
C
C        START HERE (STARTF CALLED HERE AND IN MAIN)
C
      ISOP  = .FALSE.
      SATRH = .01E0 * HOUR1(7)
      SATRH = .96
         CALL STARTF
      PRINT 50, SATRH
   50 FORMAT('0  SATRH = ',F5.2)
C
C
C        GET NUMBER OF GRID PTS USED FOR RMS SDOT AND SFC PRES CHANGE
C
      NSDOT  =  2288
      NSFCP  = (  53 -2) * (  45 -2)
C   START FORWARD
      ITSW   = 1
      IF (IHOUR.LE.0) GO TO 2
C     NOT DOING  INITIAL (TAU = 0) TIME STEP  GET DATA FOR CENTERED STEP
      MNSTEP = 1
      ITSW   = 4
 2    CONTINUE
C
C        CALC SOLAR DECLINATION ANGLE FOR MIDDAY
C
      IGTIME = IZTIME
      XDAYMO = IDAYMO
      XDAYMO = XDAYMO + (IHOUR+IGTIME)/24+.5E0/24.E0
      IMONTH = IMO
      LL     = IGTIME + IHOUR
      HOUR   = MOD(LL,24)
      CALL SOLDEC(XDAYMO,IMONTH,HOUR,IYEAR)
C
C     PRINT DATE
C
      PRINT 802, IGTIME,IMONTH,IDAYMO,IYEAR
  802 FORMAT('0INITIAL HOUR MONTH DAY YEAR OF FORECAST  ', I2, 'Z  ',
     1I2, '/', I2, '/', I2///)
      PRINT 84,IHOUR
 84   FORMAT(//,' I HAVE STARTED FORECAST HOUR',I3)
C
C//////////////////////////////////////////////////////////////////
C        TOP    OF MAIN LOOP IN WHICH TO FORECAST FROM IHOUR TO IOUT
C//////////////////////////////////////////////////////////////////
C
   19    CONTINUE
C
C     INITIALIZE RMS SIGMA-DOT LOCATIONS
C
      SIGB1 = 0.0E0
      SIGT1 = 0.0E0
      SIGT2 = 0.0E0
      SIGS1 = 0.0E0
      SIGS2 = 0.0E0
C
C   INITIALIZE ENERGY COMPUTATION LOCATIONS
C
      IF (MNSTEP.NE.NPHOUR) GO TO 303
           DO 9 K=1,  8
             SUMZ(K)   = 0.0E0
             SUMP(K)   = 0.0E0
             SUMTS(K)  = 0.0E0
             SUMK(K)   = 0.0E0
             SUMP1(K)  = 0.0E0
             SUMP2(K)  = 0.0E0
             SUMS(K)   = 0.0E0
             SUMF(K)   = 0.0E0
             SUMFD(K)  = 0.0E0
             SUMVOR(K) = 0.0E0
             SUMDIV(K) = 0.0E0
    9      CONTINUE
 303   CONTINUE
C
C     DO ONE TIME STEP
C
      CALL FCST
C
       LTMP = LOLD
       LOLD = LMID
       LMID = LNEW
       LNEW = LTMP
       ITSW = 4
       MNS  = MNSTEP
       IF (MNSTEP.EQ.0) GO TO 78001
C
C     IF MNSTEP = 0 DISASTER
C
      IF (MNSTEP.LT.0) MNSTEP = 1
C
C     IODD = 1 FOR ODD TIME STEP
C            2 FOR EVEN TIME STEP
C
C            MEANING OF ITSW
C            1  -  FORWARD TIME STEP
C            4  -  CENTERED PLUS TIME AND SPACE SMOOTHING OF TAU FIELDS
C
      IODD   = MOD(IODD,2) + 1
      MNSTEP = MOD(MNSTEP,NPHOUR) + 1
      IF (MNSTEP.GT.1) GO TO 304
      IHOUR  = IHOUR + 1
      IHR1   = IHOUR
      HOUR1(1) = IHOUR
      IF (MOD(IHOUR,24) .NE.0.0E0) GO TO 301
C     ADVANCE SUN IN ORBIT DAILY
      LL     = IGTIME + IHOUR
      HOUR = MOD(LL,24)
      CALL SOLDEC(XDAYMO,IMONTH,HOUR,IYEAR)
      GO TO 304
 301  CONTINUE
C  ADVANCE SUN ONE HOUR
      B1    = SSLHR
      B2    = CSLHR
      SSLHR = B1 * CHR + B2 * SHR
      CSLHR = B2 * CHR - B1 * SHR
 304  CONTINUE
C
C
      SIGB1  = SQRT(SIGB1  / NSDOT)
      SIGT1  = SQRT(SIGT1  / NSDOT)
      SIGT2  = SQRT(SIGT2  / NSDOT)
      SIGS1  = SQRT(SIGS1  / NSDOT)
      SIGS2  = SQRT(SIGS2  / NSDOT)
      RMSSFC = SQRT(RMSSFC / NSFCP)
      RMSTRP = SQRT(RMSTRP / NSFCP)
      IF (IABS(MNS).EQ.1) PRINT 801
      PRINT 900,IHOUR,MNS,SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
  801 FORMAT(/,1X,'HOUR MNSTEP ',5X,'SIGB1',10X,'SIGT1',10X,'SIGT2',10X
     1  ,'SIGS1',10X,'SIGS2',10X,'RMSSFC',10X,'RMSTRP',/)
  900 FORMAT(I4,I6,7E15.5)
C
C     PUT FORECAST HOUR ON DAYFILE EVERY 6 HOURS
C
      IF (MOD(IHOUR,6).NE.0 .OR. MNSTEP.NE.1) GO TO 53
      PRINT 54, IHOUR
 54   FORMAT(//,' I HAVE REACHED FORECAST HOUR',I3)
 53   IF (MNSTEP.NE.1 .OR. SUMF(1).EQ.0.E0) GO TO 52
      IF (MOD(IHOUR,3).NE.0) GO TO 55
C     ENCODE(4,108,MSG(10)) IHOUR
C108  FORMAT(I4)
 55   CONTINUE
      IF (MOD(IHOUR,12).NE.0) GO TO 704
C     ENCODE (8,705,MESAGE)    RMSSFC
C     ENCODE (6,706,MESAGE(5)) IHOUR
C705  FORMAT(F8.4)
C706  FORMAT(' AT ',I2)
C     CALL W3LOG('$L',MESAGE)
704   CONTINUE
C
C   COMPUTE ENERGY STATISTICS EVERY HOUR
C
      DO 11 K = 1, 7
        SUMZ(K)   = SUMZ(K)/(SUMF(K)*9.8E0)
        SUMK(K)   = SUMK(K)/(SUMF(K)*19.6E0)
        SUMK( 8)  = SUMK( 8) + SUMK(K)
        SUMP1(K)  = SUMP1(K)/(9.8E0*SUMF(K))
        SUMP(K)   = SUMP(K)/SUMF(K)
        SUMTS(K)  = SUMTS(K)/SUMF(K)
        SUMP2(K)  = (SUMP2(K)/SUMF(K))*(1004.E0/9.8E0)
        SUMDIV(K) = SUMDIV(K)/SUMFD(K)
        SUMVOR(K) = SUMVOR(K)/SUMFD(K)
   11 CONTINUE
      DO 12 K = 1, 7
        SUMP2(K)   = SUMP1(K)-SUMP1(K+1) + SUMP2(K)
        SUMP2( 8)  = SUMP2( 8) + SUMP2(K)
        SUMVOR( 8) = SUMVOR( 8) + SUMVOR(K)
        SUMDIV( 8) = SUMDIV( 8) + SUMDIV(K)
   12 CONTINUE
      SCNST = 1004./(9.8E0*(1.E0+2.E0/7.E0))
      DO 13 K = 2, 7
        SUMS(K)  = (SUMS(K) / SUMF(K)) * SCNST
        SUMS( 8) = SUMS( 8) + SUMS(K)
 13   CONTINUE
      PRINT 102 , IHOUR
      PRINT 100 , SUMK
      PRINT 101 , (SUMP2(I),I = 1,8)
      PRINT 105 , (SUMS(K), K = 2,8)
      PRINT 103 , SUMVOR
      PRINT 104 , SUMDIV
      PRINT 106 , (SUMP(K), K = 1,7)
      PRINT 107 , (SUMTS(K),K = 1,7)
      PRINT 109 , (SUMZ(K), K = 1,7)
C
C     END OF ENERGY AND SIGMA DOT PRINTOUTS
C
 52   CONTINUE
C
      IF (IHOUR.LT.IOUT) GOTO 19
C
C / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C               BOTTOM OF BIG LOOP  TO FORECAST TO IOUT HOUR
C / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C
         RETURN
C
 102  FORMAT ('0  HOUR = ',I3,' ENERGY STATISTICS'/31X,'1',13X,'2',
     113X,'3',13X,'4',13X,'5',13X,'6',13X,'7',9X,'TOTAL')
  100 FORMAT(' KINETIC ENERGY   ', 8E14.4)
  101 FORMAT(' POTENTIAL ENERGY ', 8E14.6)
  105 FORMAT(' STATIC STABILITY ', 14X, 2E14.4,4E14.5,E14.7)
  103 FORMAT(' VOR SQUARED      ', 8E14.4)
  104 FORMAT(' DIV SQUARED      ', 8E14.4)
  106 FORMAT('  AVG PRESSURE    ', 7E14.4)
  107 FORMAT('  AVG TEMPERATURE ', 7E14.4)
  109 FORMAT('  AVG HEIGHT      ', 4E14.4, 3E14.5)
C
C        DISASTER AREA - COME HERE IF MNSTEP = 0
C
78001 CONTINUE
         PRINT 78002
78002    FORMAT(///'  DISASTER IN FTIME - MNSTEP = 0 UPON RETURN ',
     1            'FROM FCST ... GIVE UP...'///)
      RETURN
      END
      SUBROUTINE SOLDEC (DD,MM,TT,IY)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SOLDEC         COMPUTES SOLAR DECLINATION ANGLE
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: COMPUTES SOLAR DECLINATION ANGLE.
C   MODIFIED SO    BOTH EQUATION OF TIME AND DECLINATION ARE COMPUTED
C   IN DEGREES AND THE OUTPUT IS IN TRIG FUNCTIONS INSTEAD OF ANGLES
C  E  IS EQUATION OF TIME I.E. DIFFERENCE BETWEEN MEAN TIME AND SOLAR
C       TIME
C  SLHR IS THE SHA OF THE SUN FOR 100E LONGITUDE FOR SYMMETRY REASONS
C     FORMULA AND SOME NOTATION FROM FINGER, WOOLFE + ANDERSON
C     MON. WEA. REV. OCT. 1965, PP 622 - 623
C         WITH CORRECTIONS
C
C USAGE:  CALL SOLDEC(DD,MM,TT,IY)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     DD          DAY OF MONTH                              ARGUMENT
C     MM          MONTH OF YEAR                               DITTO
C     TT          GREENWICH TIME  (HOURS)                     DITTO
C     IY          YEAR   FIXED POINT                          DITTO
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SSLDC       SIN OF SOLAR DECLINATION ANGLE            /TTME/
C     CSLDC       COS        DITTO                          /TTME/
C     SSLHR       SIN OF LOCAL HOUR ANGLE                   /TTME/
C     CSLHR       COS        DITTO                          /TTME/
C     SHR         SIN OF ONE (RADIAN HOUR) FOR INCREMENT    /TTME/
C     CHR         COS        DITTO                          /TTME/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     SIN,COS,MOD,ASIN                                      FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
C
C     TABLE OF DAY OF YEAR OF LAST DAY OF EACH MONTH WITH PHASE SHIFTED
C     ***************    NOT - REPEAT - NOT LEAP YEAR      *************
      DIMENSION DAYEAR(12)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE (12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12,23 ), BB( 12, 33, 23 )
      SAVE
C
      DATA DAYEAR/0.E0,31.E0,59.E0,90.E0,120.E0,151.E0,
     1   181.E0,212.E0,243.E0,273.E0,304.E0,334.E0/
C
C     0.98565 = 360/365.242
C     0.39785 = SIN(23, 26, 37.8)
C     0.0174533 = RADIANS/DEGREE
C
C     LEAP YEAR
      X = 0.E0
      IF (MOD(IY,4).EQ.0 .AND. MM.GT.2) X = 1.E0
      PI    = 2.E0 * ASIN(1.0)
      PI180 = PI / 180.E0
      D = (DD + DAYEAR(MM) -1.E0+ X) / 365.242E0
      D = D*2.E0*PI
      SIG=PI180*(279.9348E0+1.914827E0*SIN(D)-.07952E0*COS(D)+
     1 .019938E0*SIN(2.E0*D) - .00162E0*COS(2.E0*D)) + D
      SSLDC=0.39783E0*SIN(SIG)
      CSLDC=SQRT(1.E0-SSLDC*SSLDC)
      T = (DD + DAYEAR(MM) -80.E0+ X) / 365.242E0
      T = T*2.E0*PI
      E=-0.03E0*SIN(T)-0.12E0*COS(T)+0.165E0*SIN(2.*T)-.0008E0*COS(2.*T)
C .........   SOLHR EQN MODIFIED FOR LFM GRID CENTER LINE AT 105 DEG W
      SOLHR = PI180 * (15.E0*(E + TT +12.E0+5.E0))
      SSLHR = SIN(SOLHR)
      CSLHR = COS(SOLHR)
C   SIN AND COS OF ONE HOUR FOR INCREMENTING PURPOSE
      SHR = SIN(PI/12.E0)
      CHR = COS(PI/12.E0)
      RETURN
      END
      SUBROUTINE STARTF
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: STARTF         INITIALZES CONSTANTS AND ARRAYS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: INITIALZES MANY CONSTANTS AND LATERAL BOUNDARY COEF.
C   ARRAY (CONTROLS LINEAR DIFFUSION COEFFICIENT)....
C
C USAGE:  CALL STARTF
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     CP          SPECIFIC HEAT AT CONSTANT PRESSURE        /CNST/
C     R           UNIVERSAL GAS CONSTANT (J/(KG X K))       /CNST/
C     ROCP        R/CP  (KAPPA)                             /CNST/
C     RPK         1 / (1 + R/CP) FOR BROWN-PHILLIPS PI      /CNST/
C     LI,LJ,LK    GRID DOMIAN (USUALLY 53,45,7)             /INDEX/
C     LI1,LJ1     LI-1 AND LJ-1                             /INDEX/
C     LI2         /I/2
C     NWDS        BYTES PER STRIP  LI*(LK+2)*4              /SVHOUR/
C     NIJ         LI * LJ  LENGTH OF HORIZONTAL VECTORS     /INDEX/
C     DT          LENGTH OF TIME STEP (SECONDS)             /CNST/
C     DX          GRID INCREMENT (METERS)
C     VVCNST      SCALING CONSTANT FOR VERTICAL VELS
C     DS0         SIGMA INCREMENT B.L. ( EQUALS 1.0)
C     DS1             DITTO       TROPOSPHERE (EQUALS 1/3)
C     DS2             DITTO       STRATOSPHERE    DITTO
C     XBND        DIFFUSION COEF. ARRAY FOR BOUNDARY REGION
C     XDIF        DIFFUSION COEF. ARRAY FOR INTERIOR
C     BTHICK      BOUNDARY LAYER PRESS THICKNESS ( 50 MB)
C     MNSTEP      TIME STEP CONTOL
C     ALP         ALPHA FOR TIME SMOOTHER ( EQUALS 0.075)
C     A1BRN,A2BRN PRESSURE GRADIENT AVERAGE COEFFICIENTS
C
C     MANY MORE...... ALL FUNCTIONS OF ABOVE CONSTANTS
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12,33 ,23 )
      COMMON /BCOEF/  XDIF( 53 , 45 ), XBND( 53 , 45 )
      SAVE
C
      CP     = 1004.67E0
      R      = 287.04E0
      ROCP   = R/CP
      RPK    = 1.E0 / (1.E0  + ROCP)
      LI     = 53
      LJ     = 45
      LK     = 7
      LI1    = 53  - 1
      LJ1    = 45  - 1
      LI2    = 53 / 2
      NWDS   = 53 * (LK+2) * 4
      NIJ    = 2385
      DT     = 3600.E0/NPHOUR
      DX     = 190.5  * 1000.0E0
      VVCNST = 1.E6 * DT / 3600.0E0
      DS0    = 1.0E0
      DS1    = 1.0E0 / 3.0E0
      DS2    = 1.E0 / 3.E0
      RDELXS = 1.E0 /(DX * DX)
      VLM = 2.2E5 * 2.E0 * DT * RDELXS
      DO 88890 IQ2W6E = 1,NIJ
         XBND(IQ2W6E,1) = 0.E0
88890 CONTINUE
      DO 88900 IQ2W6E=1,5*LI
         XBND(IQ2W6E,1)=0.04E0
         XBND(IQ2W6E,LJ-4)=0.04E0
88900 CONTINUE
      JEND = LJ-5
      DO 334 J = 6,JEND
      DO 88920 IQ2W6E=1,5
         XBND(IQ2W6E,J)=0.04E0
         XBND(LI+IQ2W6E-5,J)=0.04E0
88920 CONTINUE
334   CONTINUE
      DO 88940 IQ2W6E=1,NIJ
         XDIF(IQ2W6E,1)=XBND(IQ2W6E,1)
88940 CONTINUE
C**** WHERE (XBND(1,1  ;  NIJ).EQ.0.E0)XDIF(1,1  ;  NIJ) = VLM
C**** DTDS0 = DT/DS0
         IQ2W6E=IQ2W6E
      DO 88950 IQ2W6E=1,NIJ
         IF (.NOT.(XBND(IQ2W6E,1).EQ.0.E0)) GO TO 88950
         XDIF(IQ2W6E,1)=VLM
88950 CONTINUE
      IQ2W6E = IQ2W6E
      DTDS0  = DT  / DS0
      DTDS1  = DT  / DS1
      DTDS2  = DT  / DS2
      DS1DX  = DS1 / DX
      DS2DX  = DS2 / DX
      BTHICK = 50.E0
      BTHK3  = BTHICK / 3.E0
      IODD   = 1
      MNSTEP = -1
      RDELX  = 1.E0 / DX
      DTDX   = DT/DX
      RSAT60 = -.6E0/SATRH
      RDT    = 1.E0/DT
      RBT    = -1.E0/BTHICK
      BTHICH =.5E0*BTHICK
      BTHK3H =.5E0*BTHK3
      BTK398 = BTHK3/.98E0
      BTHIK1 = BTHICK * .001E0
      ALP    = 0.075E0
      A1BRN  = 0.25E0*(ALP*ALP+1.0E0)*(ALP+1.0E0)
      A2BRN  = 1.0E0-2.E0*A1BRN
      RETURN
      END
       FUNCTION ABSH2O(U)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: ABSH2O         COMPUTES ENERGY ABSORBED BY H2O
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CALCULATES THE ENERGY ABSORBED BY WATER VAPOR. APPROXIMATES
C   THE ABSORPTION CURVE WITH SIX STRAIGHT LINES (MANABE).
C
C USAGE:  XX = ABSH2O(U)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     U           E-3 / COS(ZENITH) * P WEIGHTED WS         ARGUMENT
C                 (SEE TEND4 FOR COMPUTATION OF U)
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ABSH2O      ENERGY ABSORBED BY U                      ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     LOG10                                                 FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
      A = 0.E0
      IF (U) 55, 55, 5
    5 X = LOG10(U)
      IF (X) 15, 10, 10
   10 A =.15E0* X + .2E0
      GO TO 55
   15 IF (X +1.E0) 25, 20, 20
   20 A =.1E0* X + .2E0
      GO TO 55
   25 IF (X +2.E0) 35, 30, 30
   30 A =.05E0* X + .15E0
      GO TO 55
   35 IF (X +3.E0) 45, 40, 40
   40 A =.04E0* X + .13E0
      GO TO 55
   45 IF (X +5.E0) 55, 50, 50
   50 A =.005E0* X + .025E0
   55 ABSH2O = A
      RETURN
      END
      SUBROUTINE  LCL(THETA,SATMIX,PLCL,TLCL,NS)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: LCL            FINDS LIFTED CONDENSATION LEVEL (P&T)
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES (BY ITERAION) THE PRESSURE AND TEMPERATURE OF
C   THE LIFTED CONDENSATION LEVEL GIVEN THE POTENTIAL TEMPERATURE
C   AND MIXING RATIO OF A PARCEL.
C
C USAGE:  CALL LCL(THETA,SATMIX,PLCL,TLCL,NS)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     THETA       POTENTIAL TEMPERATURE OF THE PARCEL (K)   ARGUMENT
C     SATMIX      MIXING RATIO OF THE PARCEL (G/G)          ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PLCL        LIFTED CONDENSATION PRESS. (MB)           ARGUMENT
C     TLCL             DITTO          TEMP.  (K)            ARGUMENT
C     NS          ITERATION COUNT (LIMITED TO 20)           ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     LOG10,REAL,ABS                                        FORTLIBC
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
C  FINDS BY ITERATION THE (P,T)COORDINATES OF THE LIFTING CONDENSATION
C  LEVEL,GIVEN THE POTENTIAL TEMPERATURE AND MIXING RATIO.
C  PLCL IN MB, TLCL IN DEGREES CELSIUS.
      NS = 0
C  PITL1 IS FIRST ESTIMATE OF PIT, USING CIT = 0.5
      PITL1 = (REAL(289.95E0/THETA)**3.5)*1000.0E0
C     START OF ITERATION LOOP
 10   CONTINUE
      NS = NS+1
      IF (NS.GT.20) GO TO 15
      CIT=LOG10((PITL1*SATMIX)/(6.11E0*(0.622E0+SATMIX)))
C    2047.5 = 7.5 * 273.0
      PIT  =(REAL((CIT*(-35.7E0)+2047.5E0)/(THETA*(7.5E0-CIT)))
     A     **3.5)*1000.0E0
      IF (ABS(PIT-PITL1)-1.) 15,11,11
   11 PITL1=PIT
      GO TO 10
C     END OF ITERATION LOOP
   15 PLCL=PIT
      TLCL=(CIT*237.3E0)/(7.5E0-CIT)
      RETURN
      END
      SUBROUTINE NFB01(XI,XJ,SLAT,CLAT,SLON,CLON)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: NFB01          FINDS LAT. AND LONG. GIVEN GRID POINT I,J
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES SINE AND COSINE OF LATITUDE AND LONGITUDE GIVEN
C   THE GRID POINT INDEX (FLOATING POINT) ON THE LFM HALF BEDIENT GRID.
C
C USAGE:  CALL NFB01(XI,XJ,SLAT,CLAT,SLON,CLON)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     XI,XJ       VALUE OF GRID POINT INDEX ON LFM GRID     ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SLAT,CLAT   SIN AND COSINE OF LATITUDE                ARGUMENT
C     SLON,CLON       DITTO         LONGITUDE               ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     SIN,COS,ATAN,SQRT                                     FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      GI    =  62.40784
      XIP   =  27
      XJP   =  49
C     LATITUDE AND LONGITUDE AT GRID POINT XI, XJ
      TERMI = XI - XIP
      TERMJ = XJ - XJP
      GI2   = GI * GI
      R2    = TERMI * TERMI + TERMJ * TERMJ
      IF (TERMJ.EQ.0.0) GO TO 2
      TAN   = TERMI / TERMJ
      TLONG = ATAN(TAN)
      IF (TERMJ.GE.0.0) GO TO 7
        TLONG = TLONG + 3.1415927E0
 7    IF (TLONG.GE.0.0) GO TO 10
        TLONG = TLONG + 6.2831853E0
        GO TO 10
 2    CONTINUE
      IF (TERMI.GE.0.0) GO TO 4
        TLONG = 4.7123890E0
        GO TO 10
 4    CONTINUE
        TLONG = 1.5707963E0
 10   CONTINUE
        SLAT  = (GI2-R2) / (GI2+R2)
        CLAT  = SQRT(1.E0-SLAT*SLAT)
        SLON  = SIN(TLONG)
        CLON  = COS(TLONG)
      RETURN
      END
      SUBROUTINE PRECAL
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK    (SHORT VERSION)
C     SUBROUTINE PRECAL          MAKE EXP TABLE
C     DENNIS DEAVEN     W/NMC23   21 JUNE 83
C-----------------------------------------------------------------------
C  SETS  UP TABLE OF EXPONENTIALS FOR CALCULATION OF MIXING RATIO
C-----------------------------------------------------------------------
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      SAVE
C
      DO 200 K = 1,120
        XKT   = K -70.5E0
        AX(K) = EXP(17.269E0*XKT/(XKT+237.3E0))
  200 CONTINUE
      RETURN
      END
      SUBROUTINE UNPSEP(THE,P,PIIN,TGES,TR,NS)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: UNPSEP         FINDS TEMPERATURE OF MOIST ADIABAT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CALCULATES (BY ITERATION) THE TEMPERATURE OF MOIST
C   ADIABAT GIVEN: PSEUDO-EQUIVALENT POTENTIAL TEMPERATURE AND
C   PRESSURE OF THE PARCEL.  ASSUMES SATURATION AT CALCULATED P&T.
C
C USAGE:  CALL UNPSEP(THE,P,PIIN,TGES,TR,NS)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     THE         PSEUDO-EQUIVALENT POT. TEMPERATURE (K)    ARGUMENT
C     P           PRESSURE OF PARCEL (MB)                   ARGUMENT
C     PIIN        EXNER FUNCTION (P/1000)**R/CP OF PARCEL   ARGUMENT
C     TGES        FIRST GUESS TEMPERATURE (C)               ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     TR          TEMPERATURE OF MOIST ADIABAT (C)          ARGUMENT
C     NS          ITERATION COUNT (LIMITED TO 100)          ARGUMENT
C                 WHEN EQUAL TO 200 INDICATES NONCONVERGENCE
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     ABS,SIGN                                              FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      SAVE
C
      SV(T) = 6.1078E0 * EXP(17.2694E0*T/(T+237.3E0))
      EEP(T,SVA,P) = EXP((596.73E0-0.601E0*T)*
     1   ((0.62197E0*SVA)/(P-SVA))/
     2   (0.24E0*(T+273.16E0)))
C
      DATA CON/0.5E0/
C
      NS   = 0
      T    = TGES
      DTT  = 10.0E0
      A    = 0.0E0
  800 CONTINUE
        SVA   = SV(T)
        NS    = NS+1
        IF (NS.GT.100) GO TO 870
        AFORM = A
        A     = (T+273.16E0)*PIIN*EEP(T,SVA,P)-THE
        IF (ABS(A).LT.CON) GO TO 870
        DTT   = 0.5E0*DTT
        IF ((A*AFORM).LT.0.0E0) DTT = -DTT
        TP    = T + DTT
        SVA   = SV(TP)
        AP    = (TP+273.16E0)*PIIN*EEP(TP,SVA,P)-THE
        IF (ABS(AP).LT.CON) GO TO 869
C   USE NEWTON-RAPHSON METHOD TO FIND NEXT ESTIMATE.
C   DTT IS DISTANCE FROM T TO NEXT ESTIMATE.
C   TEST MAGNITUDE OF DENOMINATOR BEFORE COMPUTING DTT.
      XDEN = A-AP
      IF (XDEN.NE.0.0E0) GO TO 400
      NS   = 200
      GO TO 870
  400 CONTINUE
      DTT  = A * DTT / XDEN
      IF (ABS(DTT).LT.0.01E0) DTT = SIGN(0.01E0,DTT)
      T    = T + DTT
      T    = MIN(T,50.0E0)
      GO TO 800
  869 CONTINUE
        T  = TP
  870 CONTINUE
        TR = T
      RETURN
      END
      SUBROUTINE ERREXT
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK    (SHORT VERSION)
C     SUBROUTINE ERREXT          SET ERROR EXIT CODE = 1607
C     DENNIS DEAVEN     W/NMC23   21 JUNE 83
C$$$
C
      SAVE
C
      KSTOP = 9
      IF (KSTOP .NE. 9) RETURN
      PRINT 10
      STOP 1607
   10 FORMAT(//,3X,'PROGRAM STOP IN SUBROUTINE ERREXT.:')
      END
       SUBROUTINE TENPT
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENPT          CONTROLS CALLS TO MASS AND MOISTURE FCST
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CONTROLS CALLS TO MASS (PSIG & TS) AND MOISTURE (WS)
C   FORECAST SUBROUTINES.  ALSO SETS LEVEL POINTERS.
C
C USAGE:  CALL TENPT
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     K           VALUE OF LAYER INDEX (1,2,3...7)          /INDEX/
C     IVEL        VERT VEL FLAG 1 = YES, 0 = NO             /TTME/
C     MNSTEP      TIME STEP IN HOUR (1,2,3...NPHOUR)        /TTME/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     KL          WORKING LAYER (MOD(2)+1) =   1 OR 2       /INDEX/
C     KH              DITTO     PLUS ONE                    /INDEX/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TEND1,TEND2,TEND3,TEND4,TEND5,TEND6,VVEL              LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12,23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ), PI( 53 , 45, 7 )
      SAVE
C
        IF (K.GT.1) GO TO 10
        KL = 1
        KH = 2
        CALL TEND1(PHI,PI)
        CALL TEND3
        GO TO 3
10      KTT = KL
        KL  = KH
        KH  = KTT
3       CALL TEND2
        IF (IVEL .EQ.1 ) CALL VVEL
4       IF (MNSTEP.GT.1 .OR. K .GT.1) GO TO 5
        CALL TEND4
5       CALL TEND5
        CALL TEND6
        RETURN
        END
       SUBROUTINE TEND1(PHI,PI)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND1          COMPUTES GEOPOTENTIAL AND PI MID-LAYER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES GEOPOTENTIAL AND PI (P/1000)**R/CP AT THE
C   MIDPOINT OF EACH LAYER FOR THE TAU TIME LEVEL.  VALUES ARE
C   CALCULATED USING THE BROWN-PHILLIPS VERSION OF THE MID-LAYER
C   AVERAGE.  DOCUMENTED IN NMC OFFICE NOTES 92 AND 104.
C
C USAGE:  CALL TEND1(PHI,PI)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     BTHICK      DP/D(SIGMA) IN BOUNDARY LAYER (50 MB)     /CNST/
C     PSIG        DP/D(SIGMA) TROP & STRAT  (MB)            /DEABLK/
C     K2MID..     LAYER INDEX                               /INDEX/
C     RPK         1 /( 1 + R/CP) FOR PIBAR BP               /CNST/
C     TS          POT. TEMPERATURE ARRAY  (K)               /DEABLK/
C     ZSTAR       TERRAIN HEIGHT (METERS)                   /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         GEOPOTENTIAL AT MID LAYERS (M**2/SEC**2)  ARGUMENT
C     PI          EXNER FUNCTION    DITTO                   ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     WKXLOX                                                LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        PHI( 53 , 45 ,7 ), PI( 53 , 45 ,7 )
      REAL        SIGT(5),SIGS(8)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12,23), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53, 45, 25 )
      SAVE
C
      DATA SIGT  /1.E0,1.E0,0.66667E0,0.33333E0,0.E0/,
     A     SIGS  /4*0.E0,1.E0,0.66667E0,0.33333E0,0.E0/
C ---------------------------------------------------------------------
C   FORM PRESSURE VALUES IN PI(1,1,1 - 7)
C ---------------------------------------------------------------------
      DO 88890 IQ2W6E = 1,NIJ
         PI(IQ2W6E,1,1) = 50.E0 + BTHICK + 3.E0 * PSIG(IQ2W6E,1,K2MID)
     *   + 3.E0 * PSIG(IQ2W6E,1,K2MID+1)
         PI(IQ2W6E,1,2) = PI(IQ2W6E,1,1)-BTHICK
         PI(IQ2W6E,1,3) = PI(IQ2W6E,1,2)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,4) = PI(IQ2W6E,1,3)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,5) = PI(IQ2W6E,1,4)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,6) = PI(IQ2W6E,1,5)-PSIG(IQ2W6E,1,K2MID+1)
         PI(IQ2W6E,1,7) = PI(IQ2W6E,1,6)-PSIG(IQ2W6E,1,K2MID+1)
88890 CONTINUE
C ---------------------------------------------------------------------
C    NOW CHANGE P TO PI FOR ALL SEVEN INTERFACES
C ---------------------------------------------------------------------
       DO 2345 L = 1, 7
         CALL WKXLOX(VT(1,1,L),PI(1,1,L),L)
2345   CONTINUE
C ---------------------------------------------------------------------
C    CHANGE SIX INTERFACE VALUES OF PI TO PI BAR BP (LAYER)
C ---------------------------------------------------------------------
       DO 2 KK = 1,6
       DO 2 J = 1, 45
       DO 2 I = 1, 53
       PI(I,J,KK) = ((PI(I,J,KK)*VT(I,J,KK) - PI(I,J,KK+1)*VT(I,J,KK+1))
     A              /(PI(I,J,KK) - PI(I,J,KK+1))) * RPK
2      CONTINUE
C ---------------------------------------------------------------------
C    NOW FINISH THE TOP LAYER (7) PI BAR BP
C ---------------------------------------------------------------------
       PPITOP = 50.E0*.05**REAL(ROCP)
       DO 3 J = 1, 45
       DO 3 I = 1, 53
       PI(I,J,7) = ((PI(I,J,7)*VT(I,J,7) - PPITOP)
     A             /(PI(I,J,7)-50.E0 )) * RPK
3      CONTINUE
C ---------------------------------------------------------------------
C    READY TO CALCULATE GZ FOR THE SEVEN LAYERS TROPOSPHERE FIRST
C    FIRST FORM PRELIMINARY QUANTITIES IN VT
C----------------------------------------------------------------------
       KTT = 4
       KTTM = KTT - 1
       DO 20 L = 1,KTTM
      DO 88960 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,L+8)=((TS(IQ2W6E,1,K7MID+L-1)+TS(IQ2W6E,1,K7MID+L
     *   ))*.5E0)*(PI(IQ2W6E,1,L)-PI(IQ2W6E,1,L+1))
         VT(IQ2W6E,1,L+15)=(TS(IQ2W6E,1,K7MID+L-1)-TS(IQ2W6E,1,K7MID+L
     *   ))*((PI(IQ2W6E,1,L)+PI(IQ2W6E,1,L+1))*0.5E0-VT(IQ2W6E,1,L+1))
88960 CONTINUE
20     CONTINUE
C
C$           1.2  THICKNESS - SURFACE TO MID LAYER AND IN LAYERS (/CP)
C
      DO 88980 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=TS(IQ2W6E,1,K7MID)*(VT(IQ2W6E,1,1)-PI(IQ2W6E,
     *   1,1))
         VT(IQ2W6E,1,23)=VT(IQ2W6E,1,22)
88980 CONTINUE
       DO 50 L = 2,KTT
      DO 89000 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+SIGT(L)*VT(IQ2W6E,1,L+14)
89000 CONTINUE
50     CONTINUE
C
C$           1.3  GET THE TROP LAYER HEIGHTS
C
       DO 60 J = 1, 45
       DO 60 I = 1, 53
       PHI(I,J,1) = ZSTAR(I,J)*9.8E0+VT(I,J,22) * CP
60     CONTINUE
       DO 80 L = 1,KTTM
      DO 89010 IQ2W6E=1,NIJ
         PHI(IQ2W6E,1,L+1)=PHI(IQ2W6E,1,L)+CP*VT(IQ2W6E,1,L+8)
89010 CONTINUE
80     CONTINUE
C
C$           2.  STRATOSPHERE
C$           2.1  PRELIMINARY COMPUTATIONS
C
       KSB  = 5
       KST  = 7
       KSTM = KST - 1
       KSBP = KSB + 1
       DO 120 L = KSB,KSTM
      DO 89020 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,L+8)=((TS(IQ2W6E,1,K7MID+L-1)+TS(IQ2W6E,1,K7MID+L
     *   ))*0.5E0)*(PI(IQ2W6E,1,L)-PI(IQ2W6E,1,L+1))
         VT(IQ2W6E,1,L+15)=(TS(IQ2W6E,1,K7MID+L-1)-TS(IQ2W6E,1,K7MID+L
     *   ))*((PI(IQ2W6E,1,L)+PI(IQ2W6E,1,L+1))*0.5E0-VT(IQ2W6E,1,L+1))
89020 CONTINUE
120    CONTINUE
C
C$           2.2  THICKNESS ACROSS TROPOPAUSE AND IN LAYERS (/CP)
C
      DO 89040 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,23)+TS(IQ2W6E,1,K7MID+KTT-1)*(PI(
     *   IQ2W6E,1,KTT)-VT(IQ2W6E,1,KSB))+TS(IQ2W6E,1,K7MID+KSB-1)*(VT(
     *   IQ2W6E,1,KSB)-PI(IQ2W6E,1,KSB))-VT(IQ2W6E,1,22)
89040 CONTINUE
       DO 150 L = 2,KTT
      DO 89050 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+VT(IQ2W6E,1,L+14)
89050 CONTINUE
150    CONTINUE
       DO 170 L = KSBP,KST
      DO 89060 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+SIGS(L)*VT(IQ2W6E,1,L+14)
89060 CONTINUE
170    CONTINUE
C
C$           2.3  MID LAYER HEIGHTS
C
       DO 180 J = 1, 45
       DO 180 I = 1, 53
       PHI(I,J,KSB) = PHI(I,J,KTT) + VT(I,J,22) * CP
180    CONTINUE
       DO 200 L = KSB,KSTM
      DO 89070 IQ2W6E=1,NIJ
         PHI(IQ2W6E,1,L+1)=PHI(IQ2W6E,1,L)+CP*VT(IQ2W6E,1,L+8)
89070 CONTINUE
200    CONTINUE
C
C$           3.  RETURN
C
       RETURN
       END
       SUBROUTINE TEND2
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND2          COMPUTES TERMS USED BY MANY EQUATIONS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES TERMS THAT ARE COMMON TO MOST EQUATIONS STORED
C   IN VECTOR TEMPORARY ARRAY VT.
C
C USAGE:  CALL TEND2
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     ALL FORECAST VARIABLES & FIXED FIELDS     /DEABLK/
C     K,KL,KH     LAYER POINTERS                            /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     VT          VECTOR TEMPORARY STORAGE SHOWN BELOW      /VTEMP/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY                                                  LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C----------------------------------------------------------------------
C   STORAGE IN VT AS FOLLOWS:
C
C   ********************************
C
C   --------------- KH -------------
C
C   ********************************
C
C   --------------- KL -------------  K
C
C   ********************************
C
C    VT(1,1,1)             = MAP FACTOR SQUARED BAR XY (DONE IN TEND3)
C    VT(1,1,KL+1-KH+1)     = US BAR XY
C    VT(1,1,KL+3-KH+3)     = VS BAR XY
C    VT(1,1,KL+5-KH+5)     = TS BAR XY
C    VT(1,1,KL+7-KH+7)     = WS BAR XY
C    VT(1,1,10-11)         = PSIG BAR XY        (DONE IN TEND3)
C    VT(1,1,12-13)         = D(PSIG)/DX BAR Y   (DONE IN TEND3)
C    VT(1,1,14-15)         = D(PSIG)/DY BAR X   (DONE IN TEND3)
C
C---------------------------------------------------------------------
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 ,3)
      COMMON /VERTV/ IVVEL( 53 , 45 ,8)
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      SAVE
C
       IF (K .GT. 1) GO TO 100
       CALL DSXY(VT(1,1,KL+1),US(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+3),VS(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+5),TS(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+7),WS(1,1,K3MID))
100    IF (K .GE. 7) GO TO 200
       CALL DSXY(VT(1,1,KH+1),US(1,1,K7MID+1))
       CALL DSXY(VT(1,1,KH+3),VS(1,1,K7MID+1))
       CALL DSXY(VT(1,1,KH+5),TS(1,1,K7MID+1))
       IF (K .GE. 3) GO TO 200
       CALL DSXY(VT(1,1,KH+7),WS(1,1,K3MID+K))
200    RETURN
       END
       SUBROUTINE TEND3
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND3          COMPUTES D(SIGMA)/DT AND D(PSIG)/DT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES FIVE INTERFACE VALUES OF SIGMADOT AND THE
C   TWO TIME TENDENCIES OF DP/D(SIGMA) (PSIG).
C
C USAGE:  CALL TEND3
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     K,KL,KH..   LAYER POINTERS                            /INDEX/
C     US,VS...    ALL FORECAST VARIABLES PLUS FIXED FIELDS  /DEABLK/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SIGDOT      D(SIGMA)/DT FOR FIVE LAYERS               /VTEMP/
C     PSIG        PSIG TENDENCIES (TWO) STORED AT K2NEW LEV /DEABLK/
C     VT(1,1,1)   MAP SCALE BAR XY                          /VTEMP/
C     VT(1,1,10)  PSIG TROP BAR XY                            DITTO
C     VT(1,1,11)  PSIG STRAT BAR XY                           DITTO
C     VT(1,1,12)  D(PSIG)/DX BAR Y  TROP                      DITTO
C     VT(1,1,13)        DITTO       STRAT                     DITTO
C     VT(1,1,14)  D(PSIG)/DY BAR X  TROP                      DITTO
C     VT(1,1,15)        DITTO       STRAT                     DITTO
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY,DSXDY,DSYDX                                      LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ), PSIG( 53 , 45 ,6),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 ,3 )
      COMMON /VERTV/ IVVEL( 53 , 45 ,8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C----------------------------------------------------------------------
C COMPUTES THE FIVE SIGMA DOT FIELDS AND STORES THEM IN SIGDOT(1,1,1-5)
C   INTERFACE INDEXES MAP INTO SIGDOT AS FOLLOWS:
C     INTERFACE  INDEX
C     ---------  -----
C           2 INTO 1
C           3 INTO 2
C           4 INTO 3
C           6 INTO 4
C           7 INTO 5
C----------------------------------------------------------------------
C   NEED SUMS OF U,V AND DIVERGENCE FOR TROPOSPHERE AND STRATOSPHERE
C   TO COMPUTE THE PSIG TENDENCIES
C   VALUES ARE STORED AS FOLLOWS:  (LOCAL TO THIS ROUTINE ONLY)
C            SUMU    TROP  VT(20)
C            SUMV    TROP  VT(21)
C            SUMDIV  TROP  VT(22)
C            SUMU    STRAT SIGDOT(3)
C            SUMV    STRAT SIGDOT(4)
C            SUMDIV  STRAT SIGDOT(5)
C----------------------------------------------------------------------
C
C   STORE MAP FACTOR SQUARED BAR XY IN VT(1,1,1) FOR USE LATER
C
       CALL DSXY(VT(1,1,1),MF)
C
C   FORM VERTICAL DERIVATIVE OF U BAR XY AND V BAR XY
C   D(U)/D(SIGMA) INTO VT(1,1,2-5)
C   D(V)/D(SIGMA) INTO VT(1,1,6-9)
C
       CALL DSXY(VT(1,1,10),US(1,1,K7MID+1))
       CALL DSXY(VT(1,1,11),US(1,1,K7MID+2))
       CALL DSXY(VT(1,1,12),VS(1,1,K7MID+1))
       CALL DSXY(VT(1,1,13),VS(1,1,K7MID+2))
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=VT(IQ2W6E,1,11)+VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,21)=VT(IQ2W6E,1,12)+VT(IQ2W6E,1,13)
         VT(IQ2W6E,1,2)=VT(IQ2W6E,1,10)-VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,6)=VT(IQ2W6E,1,12)-VT(IQ2W6E,1,13)
88890 CONTINUE
       CALL DSXY(VT(1,1,10),US(1,1,K7MID+3))
       CALL DSXY(VT(1,1,12),VS(1,1,K7MID+3))
      DO 88930 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=VT(IQ2W6E,1,20)+VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,21)=VT(IQ2W6E,1,21)+VT(IQ2W6E,1,12)
         VT(IQ2W6E,1,3)=VT(IQ2W6E,1,11)-VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,7)=VT(IQ2W6E,1,13)-VT(IQ2W6E,1,12)
88930 CONTINUE
       CALL DSXY(VT(1,1,10),US(1,1,K7MID+4))
       CALL DSXY(VT(1,1,11),US(1,1,K7MID+5))
       CALL DSXY(VT(1,1,12),VS(1,1,K7MID+4))
       CALL DSXY(VT(1,1,13),VS(1,1,K7MID+5))
      DO 88970 IQ2W6E=1,NIJ
         SIGDOT(IQ2W6E,1,3)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,11)
         SIGDOT(IQ2W6E,1,4)=VT(IQ2W6E,1,12)+VT(IQ2W6E,1,13)
         VT(IQ2W6E,1,4)=VT(IQ2W6E,1,10)-VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,8)=VT(IQ2W6E,1,12)-VT(IQ2W6E,1,13)
88970 CONTINUE
       CALL DSXY(VT(1,1,10),US(1,1,K7MID+6))
       CALL DSXY(VT(1,1,12),VS(1,1,K7MID+6))
      DO 89010 IQ2W6E=1,NIJ
         SIGDOT(IQ2W6E,1,3)=SIGDOT(IQ2W6E,1,3)+VT(IQ2W6E,1,10)
         SIGDOT(IQ2W6E,1,4)=SIGDOT(IQ2W6E,1,4)+VT(IQ2W6E,1,12)
         VT(IQ2W6E,1,5)=VT(IQ2W6E,1,11)-VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,9)=VT(IQ2W6E,1,13)-VT(IQ2W6E,1,12)
89010 CONTINUE
C----------------------------------------------------------------------
C  FORM VERTICAL DERIVATIVES OF DIVERGENCE AND STORE IN VT(1,1,16-19)
C----------------------------------------------------------------------
       CALL DSYDX(VT(1,1,10),US(1,1,K7MID+1))
       CALL DSYDX(VT(1,1,11),US(1,1,K7MID+2))
       CALL DSXDY(VT(1,1,12),VS(1,1,K7MID+1))
       CALL DSXDY(VT(1,1,13),VS(1,1,K7MID+2))
      DO 89050 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,11)+VT(IQ2W6E,1,1
     *   2)+VT(IQ2W6E,1,13)
         VT(IQ2W6E,1,16)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,12)-VT(IQ2W6E,1,1
     *   1)-VT(IQ2W6E,1,13)
89050 CONTINUE
       CALL DSYDX(VT(1,1,10),US(1,1,K7MID+3))
       CALL DSXDY(VT(1,1,12),VS(1,1,K7MID+3))
      DO 89070 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+VT(IQ2W6E,1,10)+VT(IQ2W6E,1,1
     *   2)
         VT(IQ2W6E,1,17)=VT(IQ2W6E,1,11)+VT(IQ2W6E,1,13)-VT(IQ2W6E,1,1
     *   0)-VT(IQ2W6E,1,12)
89070 CONTINUE
       CALL DSYDX(VT(1,1,10),US(1,1,K7MID+4))
       CALL DSYDX(VT(1,1,11),US(1,1,K7MID+5))
       CALL DSXDY(VT(1,1,12),VS(1,1,K7MID+4))
       CALL DSXDY(VT(1,1,13),VS(1,1,K7MID+5))
      DO 89090 IQ2W6E=1,NIJ
         SIGDOT(IQ2W6E,1,5)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,11)+VT(IQ2W6E,
     *   1,12)+VT(IQ2W6E,1,13)
         VT(IQ2W6E,1,18)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,12)-VT(IQ2W6E,1,1
     *   1)-VT(IQ2W6E,1,13)
89090 CONTINUE
       CALL DSYDX(VT(1,1,10),US(1,1,K7MID+6))
       CALL DSXDY(VT(1,1,12),VS(1,1,K7MID+6))
      DO 89110 IQ2W6E=1,NIJ
         SIGDOT(IQ2W6E,1,5)=SIGDOT(IQ2W6E,1,5)+VT(IQ2W6E,1,10)+VT
     *   (IQ2W6E,1,12)
         VT(IQ2W6E,1,19)=VT(IQ2W6E,1,11)+VT(IQ2W6E,1,13)-VT(IQ2W6E,1,1
     *   0)-VT(IQ2W6E,1,12)
89110 CONTINUE
C----------------------------------------------------------------------
C  FORM THE SECND VERTICAL DERIVATIVE OF SIGMA DOT (ZETA) FROM THE
C  CONTINUITY EQUATION AND STORE IN VT(1,1,2-5)
C----------------------------------------------------------------------
C  FIRST COMPUTE PSIG BAR XY, D(PSIG)/DX AND D(PSIG)/DY
C
       N6IJ = 6 *      2385
      DO 89130 IQ2W6E=1,N6IJ
         VT(IQ2W6E,1,10)=99.E0
89130 CONTINUE
       CALL DSXY(VT(1,1,10),PSIG(1,1,K2MID))
       CALL DSXY(VT(1,1,11),PSIG(1,1,K2MID+1))
       CALL DSYDX(VT(1,1,12),PSIG(1,1,K2MID))
       CALL DSYDX(VT(1,1,13),PSIG(1,1,K2MID+1))
       CALL DSXDY(VT(1,1,14),PSIG(1,1,K2MID))
       CALL DSXDY(VT(1,1,15),PSIG(1,1,K2MID+1))
C
C   NOW FORM THE VERTICAL SECOND DIFFERENCE
C
      DO 89140 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,2)=-DS1DX*VT(IQ2W6E,1,1)*((VT(IQ2W6E,1,16)*V
     *   T(IQ2W6E,1,10)+VT(IQ2W6E,1,2)*VT(IQ2W6E,1,12)+VT(IQ2W6E,1,6)*
     *   VT(IQ2W6E,1,14)))/VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,3)=-DS1DX*VT(IQ2W6E,1,1)*((VT(IQ2W6E,1,17)*V
     *   T(IQ2W6E,1,10)+VT(IQ2W6E,1,3)*VT(IQ2W6E,1,12)+VT(IQ2W6E,1,7)*
     *   VT(IQ2W6E,1,14)))/VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,4)=-DS2DX*VT(IQ2W6E,1,1)*((VT(IQ2W6E,1,18)*V
     *   T(IQ2W6E,1,11)+VT(IQ2W6E,1,4)*VT(IQ2W6E,1,13)+VT(IQ2W6E,1,8)*
     *   VT(IQ2W6E,1,15)))/VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,5)=-DS2DX*VT(IQ2W6E,1,1)*((VT(IQ2W6E,1,19)*V
     *   T(IQ2W6E,1,11)+VT(IQ2W6E,1,5)*VT(IQ2W6E,1,13)+VT(IQ2W6E,1,9)*
     *   VT(IQ2W6E,1,15)))/VT(IQ2W6E,1,11)
89140 CONTINUE
C
C  FINALLY CALCULATE THE SIGDOTS AND PSIG TENDENCIES
C  NEED BOUNDARY LAYER DIVERGENCE FOR SIGDOT(2)
C  WHICH WILL BE STORED IN SIGDOT(1)   (SEE ABOVE INDEX MAP)
C
       CALL DSYDX(VT(1,1,6),US(1,1,K7MID))
       CALL DSXDY(VT(1,1,7),VS(1,1,K7MID))
      DO 89180 IQ2W6E=1,NIJ
         SIGDOT(IQ2W6E,1,1)=RDELX*(VT(IQ2W6E,1,6)+VT(IQ2W6E,1,7))
     *   *VT(IQ2W6E,1,1)
         VT(IQ2W6E,1,8)=BTHK3/VT(IQ2W6E,1,10)*SIGDOT(IQ2W6E,1,1)
         VT(IQ2W6E,1,23)=DTDX*VT(IQ2W6E,1,1)
         PSIG(IQ2W6E,1,K2NEW)=0.33333333333E0*(-DTDS1*VT(IQ2W6E,1,10)*
     *   VT(IQ2W6E,1,8)-VT(IQ2W6E,1,23)*(VT(IQ2W6E,1,20)*VT(IQ2W6E,1,1
     *   2)+VT(IQ2W6E,1,21)*VT(IQ2W6E,1,14)+VT(IQ2W6E,1,10)*VT(IQ2W6E,
     *   1,22)))
         PSIG(IQ2W6E,1,K2NEW+1)=(-0.3333333E0*(VT(IQ2W6E,1,23)*(SIGDOT
     *   (IQ2W6E,1,3)*VT(IQ2W6E,1,13)+SIGDOT(IQ2W6E,1,4)*VT(IQ2W6E,1,1
     *   5)+VT(IQ2W6E,1,11)*SIGDOT(IQ2W6E,1,5))))
         SIGDOT(IQ2W6E,1,2)=0.33333333E0*(-VT(IQ2W6E,1,3)+2.E0*(-
     *   VT(IQ2W6E,1,2)+VT(IQ2W6E,1,8)))
         SIGDOT(IQ2W6E,1,3)=VT(IQ2W6E,1,2)+2.E0*SIGDOT(IQ2W6E,1,2)-VT(
     *   IQ2W6E,1,8)
         SIGDOT(IQ2W6E,1,4)=0.33333333E0*(-VT(IQ2W6E,1,5)+2.E0*(-
     *   VT(IQ2W6E,1,4)))
         SIGDOT(IQ2W6E,1,5)=VT(IQ2W6E,1,4)+2.E0*SIGDOT(IQ2W6E,1,4)
89180 CONTINUE
       RETURN
       END
       SUBROUTINE TEND4
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND4          RADIATIVE HEATING & COOLING
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES HEATING AND COOLING RATES CAUSED BY RADIATION.
C   TEND4 IS CALLED ONLY ONCE EACH HOUR (MNSTEP=1), HOWEVER THE
C   HEATING RATES ARE APPLIED EVERY TIME STEP DURING THE FORECAST.
C
C USAGE:  CALL TEND4
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C     LI,LJ..     DOMAIN SIZE POINTERS                      /INDEX/
C     K,KL..      LAYER INDICIES                             DITTO
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     H2          THREE LAYER FIELD OF HEATING RATES        /DEABLK/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY,ABSH2O                                           LFMLIB
C     MAX                                                   FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        CLDALB(3),PLEV(8),PTHH2O(8),AH2O(8),PFRAC(7)
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
C   STORE LAYER 3 WS BAR XY IN VT(1,1,16)
C   STORE LAND SEA   BAR XY IN VT(1,1,17)
C   STORE SATW       BAR XY IN VT(1,1,18-20)
C
       CALL DSXY(VT(1,1,16),WS(1,1,K3MID+2))
       CALL DSXY2N(VT(1,1,17),XM)
       CALL DSXY2N(VT(1,1,18),SATW(1,1,1))
       CALL DSXY2N(VT(1,1,19),SATW(1,1,2))
       CALL DSXY2N(VT(1,1,20),SATW(1,1,3))
C
C     DETERMINE IF CLOUDS EXIST AND SET CLDALB ACCORDINGLY
C     CLOUDS ASSUMED IF RELATIVE HUMIDITY .GE. 60 PER CENT
      N3IJ = 3 * 2385
      DO 88890 IQ2W6E=1,N3IJ
         H2(IQ2W6E,1,1)=0.E0
88890 CONTINUE
      IF(IHOUR .GE. 48 ) GO TO 3600
      DO 99 J = 1,       44
      DO 99 I = 1,       52
      TOPCLD = 0.E0
      CLDALB(1) = 0.E0
      CLDALB(2) = 0.E0
      CLDALB(3) = 0.E0
      IF (VT(I,J,16)/VT(I,J,20).GE. (-RSAT60)) CLDALB(3) = .5E0
      IF (VT(I,J,KH+7)/VT(I,J,19).GE.(-RSAT60))CLDALB(2) = .75E0
      IF(VT(I,J,KL+7)/VT(I,J,18).GE.(-RSAT60)) CLDALB(1) = 1.E0
C
C     CLOUD TOP ALBEDO
C
      TOPCLD = MAX(CLDALB(1),CLDALB(2),CLDALB(3))
C
C     DETERMINE NATURE OF SURFACE AND SET GNDALB ACCORDINGLY
C     ICE        CONTAINS 1. IF SNOW   0. IF NO SNOW
C     XM         ONTAINS 1.E-4 IF SEA   0. IF LAND
C     GNDALB = .90 OVER LAND   1.0 OVER SNOW OR SEA
C     GNDALB = .9 + 1/2(SNOW + SEA) LT = 1
C
      GNDALB =.9E0+ (ICE(I,J) + VT(I,J,17)*1.E4) * 0.5E0
      GNDALB = MIN(1.E0,GNDALB)
C
C     IF SUN UP DO  SOLAR HEATING CALCULATIONS (DEGREES/MIN)
C
      IF(COSZEN(I,J).LE.0.E0) GO TO 330
C
C     SOLAR RADIATION  1
C     MANABE@S SOLAR RADIATION EQUATION NO CLOUDS
C     FOR 6-LAYER PE MODEL
C     SR/2
C     EFFECT OF CLOUD  ON GROUND WARMING ONLY AND ONLY BY DECREASING SUN
C     SR/3   ---   PRESSURE BROADENING EXPONENT CHANGED TO 1
C     SR/4   ---   ELIMINATE CO2 WARMING  NEGLEGABLE AND TIME CONSUMING
C     COMPUTE            DISTRIBUTOIN OF WATER IN LAYERS 1 - 4
C                 (NONE ABOVE)
C
      PFRAC(1) = VT(I,J,KL+7)
      PFRAC(2) = VT(I,J,KH+7)
      PFRAC(3) = VT(I,J,16)
      PFRAC(4) = 0.E0
      PFRAC(5) = 0.E0
      PFRAC(6) = 0.E0
      PFRAC(7) = 0.E0
C
C     COMPUTE PRESSURE AT EACH SIGMA LEVEL
C
      PLEV(8) = 50.E0
      PLEV(7)=PLEV(8)+VT(I,J,11)
      PLEV(6) = PLEV(7) + VT(I,J,11)
      PLEV(5) = PLEV(6) + VT(I,J,11)
      PLEV(4) = PLEV(5) + VT(I,J,10)
      PLEV(3) = PLEV(4) + VT(I,J,10)
      PLEV(2) = PLEV(3) + VT(I,J,10)
      PLEV(1) = PLEV(2) + BTHICK
C
C     COMPUTE CUMULATIVE PATH LENGTHS FROM BOTTOM UP
C     SUBSCRIPT GIVES LEVEL TO WHICH LENGTH CALCULATED FROM LEV = 1
C     LEV = 1  GROUND     LEV = 7 TOP    (PATH LENGTH TO INFINITY)
C
      PTHH2O(1) = 0.E0
      DO 300 LEV = 1,  7
C     0.68 = PRESSURE BROADENING EXPONENT. SAME FOR ALL GASSES
C     CHANGED TO 1.  IN SR/3
C     PDIF = PLEV(LEV) - PLEV(LEV-1)
C     PBROAD = ((PLEV(LEV-1) + PDIF*0.5)*0.001)**0.68
C     PBROAD = ((PLEV(LEV-1) + PDIF*0.5)*0.001)
C
      PBROAD = (PLEV(LEV) + PLEV(LEV+1)) * .0005E0
C
C     TOTAL PATH LENGTH FROM BELOW TO LAYER MEAN PRESSURE
C
      PTHH2O(LEV+1) = PTHH2O(LEV) + PFRAC(LEV) * PBROAD
  300 CONTINUE
C
C     COMPUTE RADIATION ABSORBED DOWN TO  EACH LEVEL
C
      DO 310 LEV = 1,         8
      PT=(PTHH2O(8)-PTHH2O(LEV))/COSZEN(I,J)
      AH2O(LEV) = 0.E0
C
C     VALUE OF ABSH20 IS LESS THAN ABOUT .2
C
      IF (PT.GT.1.E-5) AH2O(LEV) = ABSH2O(PT)
  310 CONTINUE
C
C     COMPUTE HEATING LAYER BY LAYER  DEGREES/MIN
C     NEGATIVE INDICATES WARMING
C                  980. (= ACCELERATION OF GRAVITY)*(-0.016667)(MIN/SEC)
C  -6.8056E-2 = --------------------------------------------------------
C               0.24 (= SPECIFIC HEAT OF AIR) * E3 (=DYNES/CM*CM PER MB)
       ACOSZ = COSZEN(I,J) * (-6.8056E-2)
      A1 = ACOSZ/BTHICK
C
C     SOLAR RADIATIVE ENERGY ABSORBED BY H20 IN BNDRY LAYER.
C
      H2(I,J,1) = -A1*(AH2O(2)-AH2O(1))
C
C     SAME FOR LAYERS 2,3.
C
      DO 320 LAY=2,3
      H2(I,J,LAY)=(ACOSZ/(PLEV(LAY+1)-PLEV(LAY)))
     1                * (AH2O(LAY+1) - AH2O(LAY))
  320 CONTINUE
C
C     LAYER 1 WARMED ALSO BY NON-REFLECTED RADIATION REACHING THE GROUND
C     COMPUTE DOWNWARD FLUX OF TOTAL RADIATION AT BOTTOM LEVEL
C     1.86 = SOLAR CONSTANT DEPLETED BY 7 PERCENT RAYLEIGH SCATTERING
C     FLUX AT GROUND REDUCED BY CLOUDS (IF ANY)
C
      IF(TOPCLD .EQ.1.E0.OR. GNDALB.EQ.1.E0) GO TO 330
      GNDWRM = A1 *(1.86E0- AH2O(1)) * (1.E0-TOPCLD) * (1.E0-
C
C     A1 IS NEGATIVE
C
     1         GNDALB)
      H2(I,J,1) = H2(I,J,1) + GNDWRM
C
C                            ... AND IT WAS THE EVENING AND THE MORNING
C      COOL ALL LAYERS ABOVE CLOUDS BY 0.06 DEG/HOUR
C                   (= 0.001 DEG/MIN)
C
C     ONE PATH HERE IF NIGHT.
C
  330 KBOT = 1
      TOPCLD = 0.E0
      DO 335 L=1,3
C
C     KBOT IS INDEX FOR FIRST LAYER ABOVE CLOUDS
C
      IF (CLDALB(L) .GT.0.E0) KBOT = L+1
 335  CONTINUE
C
C     GO TO 336 IF NO CLOUDS
C
      IF(KBOT.EQ.1) GO TO 336
      L = KBOT-1
C
C     CLDALB IS ALBEDO OF TOP LAYER OF CLOUDS
C
      TOPCLD = CLDALB(L)
 336  IF (KBOT.GT.3) GO TO 350
C
      DO 340 L = KBOT,3
        H2(I,J,L) = H2(I,J,L) + 1.6667E-5
  340 CONTINUE
C
C     UPPER LEVEL COOLING (LAYERS 4-7, SEE TEND5)
C
 350  CONTINUE
 370  CONTINUE
C     IF NO CLOUDS BUT THERE IS SNOW AND NIGHT TIME (SOLZEN .GT. 80 DEG)
C              (TAKEN CARE OF IN CALCULATION OF COSZEN)
C     COOL BOUNDRY LAYER BY 0.1 DEG/HOUR (= 0.0016667 DEG/MIN)
C      ADDITIONAL
C     COOLING ABOVE CLOUDS ADDED
C
C      CONTINUE SPECIAL   KEEP SNOW COOLING IF APPROPRIATE
C
 360  CONTINUE
      IF (TOPCLD.NE.0.E0)   GO TO 365
      IF (ICE(I,J).EQ.0.E0) GO TO 365
C
C     IF NO CLOUDS AND IF SNOW AND IF NIGHT,
C     THEN COOL LOWEST LAYER
C
      IF (COSZEN(I,J).LE.0.E0)H2(I,J,1) = H2(I,J,1)+2.7778E-5
 365  CONTINUE
 99   CONTINUE
3600  RETURN
      END
       SUBROUTINE TEND5
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND5          COMPUTES TS AND WS TIME TENDENCIES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES SEA SURFACE HEAT AND MOISTURE FLUXES
C   AND THEN CREATES THE TIME TENDENCIES OF POTENTIAL
C   TEMPERATURE AND MOISTURE (EXCLUDING GRID AND SUBGRID SCALE PRECIP).
C
C PROGRAM HISTORY LOG:
C   93-05-14  R.E.JONES   CORRECTION FOR OVERFLOW, 32 BIT IEEE F.P HAS
C                         SMALLER EXPONENT RANGE (10E+38) THAN IBM370
C                         32 BIT FLOATING POINT (10E+75).
C
C USAGE:  CALL TEND5
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C     SIGDOT      D(SIGMA)/DT                               /VTEMP/
C     K,KL,KH..   VERTICAL LAYER INDICIES                   /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     TS,WS,PSIG  TENDENCIES STORED AT LNEW TIME LEVEL      /DEABLK/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY,DSXDY,DSYDX                                      LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 ,3 )
      COMMON /VERTV/ IVVEL( 53 , 45 ,8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12,23),BLARGE(12,       33 ,23)
      COMMON /PBNDRY/ AA( 53 ,12,23), BB(12,       33 ,23)
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      SAVE
C
C   COMPUTE SEA SURFACE HEAT FLUX INTO THE BOUNDARY LAYER
C   AND STORE IN VT(18)
C
       IF (K .GT. 1) GO TO 100
       CALL DSXY2N(VT(1,1,16),TS(1,1,K7OLD))
       CALL DSXY2N(VT(1,1,17),XM)
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,18)=0.E0
         VT(IQ2W6E,1,22)=.6E0*VT(IQ2W6E,1,17)*(VT(IQ2W6E,1,16)-ST
     *   (IQ2W6E,1))
88890 CONTINUE
         IQ2W6E=IQ2W6E
      DO 88910 IQ2W6E=1,NIJ
         IF(.NOT.(VT(IQ2W6E,1,22).LT.0.E0))GO TO 88910
         VT(IQ2W6E,1,18)=VT(IQ2W6E,1,22)
88910 CONTINUE
         IQ2W6E=IQ2W6E
C
C   COMPUTE SEA SURFACE MOISTURE FLUX (ONLY WHEN TSEA GT T B.L.)
C   STORE TENDENCY INTO BOUNDARY LAYER WS LNEW FIELD
C
       CALL DSXY2N(VT(1,1,22),WS(1,1,K3OLD))
       CALL DSXY2N(VT(1,1,19),SATW(1,1,1))
       VT(53,1,19) = 9.E0
       N3IJ = 3 *      2385
C****  WHERE(VT(1,1,22  ;  NIJ).LT.0.E0)
C****A  VT(1,1,18  ;  NIJ) = VT(1,1,22  ;  NIJ)
      DO 88920 IQ2W6E=1,N3IJ
         WS(IQ2W6E,1,K3NEW)=0.E0
88920 CONTINUE
      DO 88930 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=ST(IQ2W6E,1)-273.E0
        VT(IQ2W6E,1,16)=7.5E0*VT(IQ2W6E,1,16)/(VT(IQ2W6E,1,16)+237.3)
88930 CONTINUE
C****  WHERE(VABS(VT(1,1,16  ;  NIJ)  ;  NIJ).GT.4.E0)
C****A  VT(1,1,16  ;  NIJ) = 4.E0
         IQ2W6E=IQ2W6E
      DO 88950 IQ2W6E=1,NIJ
         IF(.NOT.(ABS(VT(1,1,16)).GT.4.E0))GO TO 88950
         VT(IQ2W6E,1,16)=4.E0
88950 CONTINUE
         IQ2W6E=IQ2W6E
      DO 88960 IQ2W6E = 1,NIJ
         VT(IQ2W6E,1,20) = 6.11*10.0**(VT(IQ2W6E,1,16))
88960 CONTINUE
C
C   31.683673E0 BELOW IS ACTUALLY EQUAL TO 50.0/.98 * 0.621
C
      DO 88970 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=31.683673E0*VT(IQ2W6E,1,20)/(1013.E0-.379E0*V
     *   T(IQ2W6E,1,20))*SATRH
         VT(IQ2W6E,1,23)=VT(IQ2W6E,1,22)/VT(IQ2W6E,1,19)
         VT(IQ2W6E,1,19)=.2E0*VT(IQ2W6E,1,17)*(VT(IQ2W6E,1,22)-VT
     *   (IQ2W6E,1,21))
88970 CONTINUE
         IQ2W6E=IQ2W6E
      DO 89000 IQ2W6E=1,NIJ
         IF(.NOT.(VT(IQ2W6E,1,18).LT..0E0.AND.VT(IQ2W6E,1,23).LE..7E0.
     *   AND.VT(IQ2W6E,1,19).LT.0.E0))GO TO 89000
         WS(IQ2W6E,1,K3NEW)=-DT*VT(IQ2W6E,1,19)
89000 CONTINUE
         IQ2W6E=IQ2W6E
C
C   FIRST CALCULATE THE VERTICAL ADVECTION TERMS FOR TS AND WS
C   TS STORED IN VT(1,1,16)
C   WS STORED IN VT(1,1,17)
C   VT(1,1,24) AND VT(1,1,25) WILL BE USED FOR HOLDING THE LOWER
C   LAYER VALUES UNTIL THE COMPLETION OF THE MASS FORCAST....
C   TOP OF BOUNDARY LAYER IS DONE HERE WHILE K IS STILL EQUAL TO ONE
C
C****  WHERE(VT(1,1,18  ;  NIJ).LT..0E0.AND.
C****A       VT(1,1,23  ;  NIJ).LE..7E0 .AND.
C****B       VT(1,1,19  ;  NIJ).LT.0.E0)
C****C       WS(1,1,K3NEW  ;  NIJ) = -DT*VT(1,1,19  ;  NIJ)
C****1  (VT(1,1,KL+5  ;  NIJ)-VT(1,1,KH+5  ;  NIJ)) *
C****A              DTDS0 / 2.E0
C****  VT(1,1,20  ;  NIJ)=BTHICK/VT(1,1,10  ;  NIJ)
      DO 89010 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=SIGDOT(IQ2W6E,1,1)*(VT(IQ2W6E,1,KL+5)-VT
     *   (IQ2W6E,1,KH+5))*DTDS0/2.E0
         VT(IQ2W6E,1,20)=BTHICK/VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,21)=BTHICK/(BTHICK+VT(IQ2W6E,1,10))
         VT(IQ2W6E,1,17)=-SIGDOT(IQ2W6E,1,1)*(VT(IQ2W6E,1,20)*VT(
     *   IQ2W6E,1,KH+7)+VT(IQ2W6E,1,KL+7)/VT(IQ2W6E,1,20))*VT(IQ2W6E,1
     *   ,21)*DT
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,16)*6.E0*BTHK3/VT(IQ2W6E,1,10)
         VT(IQ2W6E,1,25)=-VT(IQ2W6E,1,17)
89010 CONTINUE
       GO TO 200
100    CONTINUE
      DO 89070 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,18)=0.0
89070 CONTINUE
C
C   VERTICAL ADVECTION FOR K = 2,3,4,5,6,7
C   VT( 24 AND 25 ) ALREADY CONTAIN THE LOWER LAYER VALUES
C
       GO TO (20,20,30,40,50,60,70),K
20     CONTINUE
      DO 89080 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=SIGDOT(IQ2W6E,1,2)*(VT(IQ2W6E,1,KL+5)-VT
     *   (IQ2W6E,1,KH+5))*DTDS1
         VT(IQ2W6E,1,16)=(VT(IQ2W6E,1,20)+VT(IQ2W6E,1,24))*0.5E0
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,2)*(VT(IQ2W6E,1,KL+7)+VT
     *   (IQ2W6E,1,KH+7))*1.5E0*DT
         VT(IQ2W6E,1,17)=VT(IQ2W6E,1,25)-VT(IQ2W6E,1,21)
         VT(IQ2W6E,1,25)=VT(IQ2W6E,1,21)
89080 CONTINUE
       GO TO 200
30     CONTINUE
      DO 89140 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=SIGDOT(IQ2W6E,1,3)*(VT(IQ2W6E,1,KL+5)-VT
     *   (IQ2W6E,1,KH+5))*DTDS1
         VT(IQ2W6E,1,16)=(VT(IQ2W6E,1,20)+VT(IQ2W6E,1,24))*.5E0
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,20)
         VT(IQ2W6E,1,17)=VT(IQ2W6E,1,25)
89140 CONTINUE
       GO TO 200
40     CONTINUE
      DO 89180 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=VT(IQ2W6E,1,24)*.5E0
         VT(IQ2W6E,1,24)=0.E0
89180 CONTINUE
       GO TO 200
50     CONTINUE
60     CONTINUE
      DO 89200 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=SIGDOT(IQ2W6E,1,K-1)*(VT(IQ2W6E,1,KL+5)-
     *   VT(IQ2W6E,1,KH+5))*DTDS2
         VT(IQ2W6E,1,16)=(VT(IQ2W6E,1,20)+VT(IQ2W6E,1,24))*.5E0
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,20)
89200 CONTINUE
       GO TO 200
70     CONTINUE
      DO 89230 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=VT(IQ2W6E,1,24)*.5E0
89230 CONTINUE
200    CONTINUE
C
C  CALCULATE TS AND WS TENDENCIES AND STORE AT THE LNEW LEVEL
C
C  NEED DT/DX AND DT/DY FOR T TENDENCY
C
        CALL DSXDY(VT(1,1,20),TS(1,1,K7MID))
        CALL DSYDX(VT(1,1,21),TS(1,1,K7MID))
        IF( K .GT. 3 ) GO TO 300
      DO 89240 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=DTDX*VT(IQ2W6E,1,1)
         TS(IQ2W6E,1,K7NEW)=-VT(IQ2W6E,1,16)-VT(IQ2W6E,1,22)*(VT(
     *   IQ2W6E,1,KL+1)*VT(IQ2W6E,1,21)+VT(IQ2W6E,1,KL+3)*VT(IQ2W6E,1,
     *   20))-DT*H2(IQ2W6E,1,K)-DT*VT(IQ2W6E,1,18)
89240 CONTINUE
C
C  NEED DW/DX, DW/DY AND DIVERGENCE FOR WS TENDENCY
C
       CALL DSXDY(VT(1,1,20),VS(1,1,K7MID))
       CALL DSYDX(VT(1,1,21),US(1,1,K7MID))
       CALL DSXDY(VT(1,1,22),WS(1,1,K3MID+K-1))
       CALL DSYDX(VT(1,1,23),WS(1,1,K3MID+K-1))
      DO 89260 IQ2W6E=1,NIJ
         WS(IQ2W6E,1,K3NEW+K-1)=-VT(IQ2W6E,1,17)-DTDX*VT(IQ2W6E,1,1)*(
     *   VT(IQ2W6E,1,KL+1)*VT(IQ2W6E,1,23)+VT(IQ2W6E,1,KL+3)*VT(IQ2W6E
     *   ,1,22)+VT(IQ2W6E,1,KL+7)*(VT(IQ2W6E,1,20)+VT(IQ2W6E,1,21)))+W
     *   S(IQ2W6E,1,K3NEW+K-1)
89260 CONTINUE

       GO TO 900
300    CONTINUE
      DO 89270 IQ2W6E=1,NIJ
         TS(IQ2W6E,1,K7NEW)=-VT(IQ2W6E,1,16)-DTDX*VT(IQ2W6E,1,1)*
     *   (VT(IQ2W6E,1,KL+1)*VT(IQ2W6E,1,21)+VT(IQ2W6E,1,KL+3)*VT(
     *   IQ2W6E,1,20))-DT*1.6667E-5
89270 CONTINUE
900    RETURN
       END
       SUBROUTINE TEND6
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND6          MASS & MOISTURE TENDENCIES ADVANCED 1 STEP
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES FORTH ORDER BAR XY OF THE MASS AND MOISTURE
C   TIME TENDENCIES, ADDS THE LINEAR DIFFUSION TERM AND ADVANCES
C   FORWARD ONE TIME STEP.  LATERAL BOUNDARY CONDITION (DIFFUSIVE
C   NUDGE) IS ALSO CALCULATED HERE.  PERIMETER BOUNDARY VALUES ARE
C   RESTORED FOR TS, PSIG AND WS.
C
C USAGE:  CALL TEND6
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C     XDIF        ARRAY OF SCALED DIFFUSION COEFFICIENTS    /BCOEF/
C     ITSW        TIME STEP SWITCH                          /TTME/
C                       ITSW EQ 1 === FORWARD STEP
C                       ITSW NE 1 === CENTERED STEP
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     TS,WS,PSIG  TAU + 1 LEVELS STORED AT LNEW LEVEL       /DEABLK/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DEL2XY,TENXY,BNDLAP                                   LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/  SIGDOT( 53 , 45 , 5 ), VT( 53 , 45 ,25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/ IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/ MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1     SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2     ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB(12, 33 ,23 )
      COMMON /BCOEF/  XDIF( 53 , 45 ), XBND( 53 , 45 )
      SAVE
C----------------------------------------------------------------------
C   FORM XY BAR ON THE TS, WS, AND PSIG TENDENCIES AND STORE THE RESULT
C   AT THE LNEW TIME LEVEL.  ALSO ADD THE LINEAR DIFFUSION TERM TO EACH
C   LNEW VALUE, XDIF TAKES CARE OF THE INTERIOR AND LATERAL BOUNDARY
C   REGION AUTOMATICALLY.
C   VT(24) AND VT(25) ARE NOT AVAILABLE FOR TEMPORAY SPACE SO
C   VT(16) AND VT(17) WILL BE USED
C----------------------------------------------------------------------
C
C  TS FIRST FOR ALL VALUES OF K
C
       C1 = 2.E0
       IF (ITSW .EQ. 1 ) C1 = 1.E0
       CALL DEL2XY(VT(1,1,16),TS(1,1,K7OLD))
       CALL TENXY(VT(1,1,17),TS(1,1,K7NEW))
      DO 88890 IQ2W6E=1,NIJ
         TS(IQ2W6E,1,K7NEW)=XDIF(IQ2W6E,1)*VT(IQ2W6E,1,16)+VT(IQ2W6E,1
     *   ,17)*C1+TS(IQ2W6E,1,K7OLD)
88890 CONTINUE
C
C  WS FOR K = 1,2,3
C
       IF ( K.GT.3 ) GO TO 100
       CALL DEL2XY(VT(1,1,16),WS(1,1,K3OLD + K - 1))
       CALL TENXY(VT(1,1,17),WS(1,1,K3NEW + K - 1))
      DO 88900 IQ2W6E=1,NIJ
         WS(IQ2W6E,1,K3NEW+K-1)=XDIF(IQ2W6E,1)*VT(IQ2W6E,1,16)+VT
     *   (IQ2W6E,1,17)*C1+WS(IQ2W6E,1,K3OLD+K-1)
88900 CONTINUE
         IQ2W6E=IQ2W6E
      DO 88910 IQ2W6E=1,NIJ
         IF(.NOT.(WS(IQ2W6E,1,K3NEW+K-1).LT..0E0))GO TO 88910
         WS(IQ2W6E,1,K3NEW+K-1)=0.E0
88910 CONTINUE
         IQ2W6E=IQ2W6E
C
C  PSIG FOR K = 1,2
C
C****  WHERE (WS(1,1,K3NEW+K-1  ;  NIJ) .LT..0E0)
C****A           WS(1,1,K3NEW+K-1  ;  NIJ) = 0.E0
       IF ( K.GT.2 ) GO TO 100
       CALL DEL2XY(VT(1,1,16),PSIG(1,1,K2OLD + K - 1))
       CALL TENXY(VT(1,1,17),PSIG(1,1,K2NEW + K - 1))
       PCKTRL = 50.E0
       PCKTRH = 300.E0
       IF ( K.EQ.2 ) PCKTRL = 15.E0
       IF ( K.EQ.2 ) PCKTRH = 150.E0
      DO 88920 IQ2W6E=1,NIJ
         PSIG(IQ2W6E,1,K2NEW+K-1)=XDIF(IQ2W6E,1)*VT(IQ2W6E,1,16)+
     *   VT(IQ2W6E,1,17)*C1+PSIG(IQ2W6E,1,K2OLD+K-1)
88920 CONTINUE
C****  WHERE(PSIG(1,1,K2NEW+K-1  ;  NIJ).GT.PCKTRH)
C****A        PSIG(1,1,K2NEW+K-1  ;  NIJ) = PCKTRH
         IQ2W6E=IQ2W6E
      DO 88930 IQ2W6E=1,NIJ
         IF(.NOT.(PSIG(IQ2W6E,1,K2NEW+K-1).GT.PCKTRH))GO TO 88930
         PSIG(IQ2W6E,1,K2NEW+K-1)=PCKTRH
88930 CONTINUE
         IQ2W6E=IQ2W6E
C****  WHERE (PSIG(1,1,K2NEW+K-1  ;  NIJ) .LT. PCKTRL)
C****A        PSIG(1,1,K2NEW+K-1  ;  NIJ) = PCKTRL
         IQ2W6E=IQ2W6E
      DO 88940 IQ2W6E=1,NIJ
         IF(.NOT.(PSIG(IQ2W6E,1,K2NEW+K-1).LT.PCKTRL))GO TO 88940
         PSIG(IQ2W6E,1,K2NEW+K-1)=PCKTRL
88940 CONTINUE
         IQ2W6E=IQ2W6E
100    CONTINUE
C
C  SETS LATERAL BOUNDARY CONDITIONS THRU CALLS TO BNDLAP FOR T,PSIG
C
       CALL BNDLAP(ALARGE(1,1,K+14),BLARGE(1,1,K+14),TS(1,1,K7NEW))
       IF( K.GT.2 ) GO TO 10
       L2 = K + LNEW * 2
       CALL BNDLAP(ALARGE(1,1,K+21),BLARGE(1,1,K+21),PSIG(1,1,L2))
10     CONTINUE
C-----------------------------------------------------------------------
C  RESTORE PERIMETER VALUES OF WS HERE FOR ALL THREE LAYERS
C  MOVE LOLD TO LNEW AROUND THE EDGES OF THE GRID
C-----------------------------------------------------------------------
       IF ( K .GT. 3 ) RETURN
       L3O = K + LOLD * 3
       L3N = K + LNEW * 3
      DO 88950 IQ2W6E=1,LI
         WS(IQ2W6E,1,L3N)=WS(IQ2W6E,1,L3O)
         WS(IQ2W6E,45,L3N)=WS(IQ2W6E,45,L3O)
88950 CONTINUE
       DO 34 J = 2,44
       WS(1,J,L3N)    = WS(1,J,L3O)
       WS( 53 ,J,L3N) = WS( 53 ,J,L3O)
34     CONTINUE
20     CONTINUE
       RETURN
       END
       SUBROUTINE VVEL
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: VVEL           COMPUTES TWO HOUR AVERAGE OMEGA
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES OMEGA (VERT VEL) DURING TWO HOURS PRIOR TO
C   POSTING THE OUTPUT FILES.  OMEGAS ARE AVERAGED OVER THE TWO HOUR
C   PERIOD EVERY OTHER (EVEN) TIME STEP.
C USAGE:  CALL VVEL
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     (COL 7-15)           (COL 19-57)                      (COL 61-71)
C     PSIG        TENDENCIES AT TAU+1 STEP (IN LNEW)        /DEABLK/
C     SIGDOT      D(SIGMA)/DT                               /VTEMP/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     IVVEL       SCALED VERTICAL VELOCITIES (INTEGER)      /VERTV/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TENXY                                                 LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        PTEN1( 53 , 45 ), PTEN2( 53 , 45 )
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      SAVE PTEN1,PTEN2
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ), PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C    PSIG TENDENCIES INTO PTEN1 & PTEN2 WHEN K = 1 FOR USE LATER
C
       IF( K.GT.1 ) GO TO 200
      DO 88890 IQ2W6E=1,NIJ
         PTEN1(IQ2W6E,1)=PSIG(IQ2W6E,1,K2NEW)
         PTEN2(IQ2W6E,1)=PSIG(IQ2W6E,1,K2NEW+1)
88890 CONTINUE
200    CONTINUE
      DO 88910 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,23)=RDELX*VT(IQ2W6E,1,1)
88910 CONTINUE
       GO TO (10,20,30,40,50,60,70),K
10     CONTINUE
      DO 88920 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,17)=3.E0*RDT*(PTEN1(IQ2W6E,1)+PTEN2(IQ2W6E,1))
         VT(IQ2W6E,1,18)=3.E0*(VT(IQ2W6E,1,12)+VT(IQ2W6E,1,13))
         VT(IQ2W6E,1,19)=3.E0*(VT(IQ2W6E,1,14)+VT(IQ2W6E,1,15))
         VT(IQ2W6E,1,20)=SIGDOT(IQ2W6E,1,1)*BTHICH+VT(IQ2W6E,1,17)+(VT
     *   (IQ2W6E,1,KL+1)*VT(IQ2W6E,1,18)+VT(IQ2W6E,1,KL+3)*VT(IQ2W6E,1
     *   ,19))*VT(IQ2W6E,1,23)
88920 CONTINUE
1      CONTINUE
       GO TO 100
20     CONTINUE
      DO 88960 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=BTHK3/VT(IQ2W6E,1,10)*SIGDOT(IQ2W6E,1,1)+SIGD
     *   OT(IQ2W6E,1,2)
         VT(IQ2W6E,1,22)=2.5E0
88960 CONTINUE
2      CONTINUE
       GO TO 21
30     CONTINUE
      DO 88980 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,2)+SIGDOT(IQ2W6E,1,3)
         VT(IQ2W6E,1,22)=1.5E0
88980 CONTINUE
3      CONTINUE
       GO TO 21
40     CONTINUE
      DO 89000 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,3)
         VT(IQ2W6E,1,22)=0.5E0
89000 CONTINUE
4      CONTINUE
21     CONTINUE
      DO 89020 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=VT(IQ2W6E,1,21)*VT(IQ2W6E,1,10)*1.5E0+VT
     *   (IQ2W6E,1,22)*(RDT*PTEN1(IQ2W6E,1)+(VT(IQ2W6E,1,KL+1)*VT
     *   (IQ2W6E,1,12)+VT(IQ2W6E,1,KL+3)*VT(IQ2W6E,1,14))*VT(IQ2W6E,1,
     *   23))+RDT*3.E0*PTEN2(IQ2W6E,1)+(VT(IQ2W6E,1,KL+1)*3.E0*VT
     *   (IQ2W6E,1,13)+VT(IQ2W6E,1,KL+3)*3.E0*VT(IQ2W6E,1,15))*VT
     *   (IQ2W6E,1,23)
89020 CONTINUE
5      CONTINUE
       GO TO 100
50     CONTINUE
      DO 89030 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,4)
         VT(IQ2W6E,1,22)=2.5E0
89030 CONTINUE
6      CONTINUE
       GO TO 51
60     CONTINUE
      DO 89050 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,4)+SIGDOT(IQ2W6E,1,5)
         VT(IQ2W6E,1,22)=1.5E0
89050 CONTINUE
7      CONTINUE
       GO TO 51
70     CONTINUE
      DO 89070 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,21)=SIGDOT(IQ2W6E,1,5)
         VT(IQ2W6E,1,22)=0.5E0
89070 CONTINUE
8      CONTINUE
51     CONTINUE
      DO 89090 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=VT(IQ2W6E,1,21)*VT(IQ2W6E,1,11)*1.5E0+VT
     *   (IQ2W6E,1,22)*(RDT*PTEN2(IQ2W6E,1)+(VT(IQ2W6E,1,KL+1)*VT
     *   (IQ2W6E,1,13)+VT(IQ2W6E,1,KL+3)*VT(IQ2W6E,1,15))*VT(IQ2W6E,1,
     *   23))
89090 CONTINUE
9      CONTINUE
100    CONTINUE
       CALL TENXY(VT(1,1,17),VT(1,1,20))
      DO 89100 IQ2W6E=1,NIJ
         IVVEL(IQ2W6E,1,K)=IVVEL(IQ2W6E,1,K)+VVCNST*VT(IQ2W6E,1,17)
89100 CONTINUE
       RETURN
       END
       SUBROUTINE TENUV
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENUV          CONTROLS CALLS TO MOMENTUM FCST SUBS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: CONTROLS CALLS TO MOMENTUM FORECAST SUBROUTINES.  ALSO
C   KEEPS TRACK OF LEVEL INDICIES.
C USAGE:  CALL TENUV
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     K           LAYER INDEX                               /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     KL          WORKING LAYER (MOD(2)+1) =S 1 OR 2        /INDEX/
C     KH              DITTO    PLUS ONE                     /INDEX/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TENDA,TENDB                                           LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/ IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/ MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1     SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2     ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
       IF ( K.GT.1 ) GO TO 10
       KL = 1
       KH = 2
       CALL TENDA
       GO TO 3
10     KTT = KL
       KL = KH
       KH = KTT
3      CALL TENDB
       RETURN
       END
       SUBROUTINE TENDA
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENDB          COMPUTES PRESSURE GRADIENT AVERAGE
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES PRESSURE GRADIENT AVERAGE OF THE THREE TIME
C   LEVELS.  STORING THE RESULT IN PHI(X-COMPONENT) AND PI (Y-
C   COMPONENT).
C USAGE:  CALL TENDB
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         GEOPOTENTIAL AT TAU TIME LEVEL            /GEOPOT/
C     PI          EXNER FUNCTION (P/1000)**R/CP AT TAU      /GEOPOT/
C     LOLD...     TIME INDICIES                             /INDEX/
C     K7OLD..     LEVEL-TIME INDICIES                       /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         X-COMPONENT OF PRESS. GRAD. AVERAGE       /GEOPOT/
C     PI          Y             DITTO                       /GEOPOT/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     GETPGF,TEND1                                          LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /GEOPOT/ PHI( 53, 45, 7 ), PI( 53, 45, 7 )
      SAVE
C
       N7IJ = 7 * 2385
      DO 88890 IQ2W6E=1,N7IJ
         US(IQ2W6E,1,K7NEW)=PHI(IQ2W6E,1,1)
         VS(IQ2W6E,1,K7NEW)=PI(IQ2W6E,1,1)
         PHI(IQ2W6E,1,1)=0.E0
         PI(IQ2W6E,1,1)=0.E0
88890 CONTINUE
       CALL GETPGF(LMID)
       LLTMP = LMID
       LMID = LOLD
       K7TMP = K7MID
       K7MID = K7OLD
       K2TMP = K2MID
       K2MID = K2OLD
       CALL TEND1(US(1,1,K7NEW),VS(1,1,K7NEW))
       LMID = LLTMP
       K7MID = K7TMP
       K2MID = K2TMP
       CALL GETPGF(LOLD)
       LLTMP = LMID
       LMID = LNEW
       K7TMP = K7MID
       K7MID = K7NEW
       K2TMP = K2MID
       K2MID = K2NEW
       CALL TEND1(US(1,1,K7NEW),VS(1,1,K7NEW))
       LMID = LLTMP
       K7MID = K7TMP
       K2MID = K2TMP
       CALL GETPGF(LNEW)
       RETURN
       END
       SUBROUTINE GETPGF(LTIME)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: GETPGF         COMPUTE PRESS. GRAD. AVERAGE
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES THETA*D(PI)/DX + D(PHI)/DX AND
C  THETA*D(PI)/DY + D(PHI)/DY STORING THE RESULT IN PHI & PI.
C  THIS ROUTINE IS CALLED THREE TIMES, ONCE FOR EACH TIME LEVEL.
C  QUANTITIES ARE SCALED AND ACCUMULATED  IN PHI AND PI SO THAT AFTER
C  THE THIRD CALL THE PRESSURE GRADIENT AVERAGE IS IN PHI AND PI
C  DX VALUES IN PHI AND DY VALUES IN PI.
C  INPUT VALUES OF PHI AND PI HAVE BEEN COMPUTED AND STORED IN
C  US(LNEW) AND VS(LNEW) RESPECTIVELY.
C
C USAGE:  CALL GETPGF(LTIME)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LTIME       TIME LEVEL OF INPUT (TAU-1,TAU, OR TAU+1) ARGUMENT
C                 LOLD=TAU-1, LMID=TAU, LNEW=TAU+1
C     A1BRN       PRESS.GRAD. AVERAGE WEIGHT (TAU LEVEL)    /CNST/
C     A2BRN                 DITTO            (TAU-1,TAU+1)  /CNST/
C     TS          POT. TEMP. ALL THREE TIME LEVELS          /DEABLK/
C     US(1,1,K7NEW)   PHI AT LTIME LEVEL                    /DEABLK/
C     VS(1,1,K7NEW)   PI  AT LTIME LEVEL                    /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         PRESS. GRAD. AVERAGE (X-COMPONENT)        /GEOPOT/
C     PI                  DITTO         Y    DITTO          /GEOPOT/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY,DSYDX,DSXDY                                      LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ), PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ), PI( 53, 45, 7 )
      SAVE
C
       ALPWT = A1BRN
       IF (LTIME .EQ. LMID) ALPWT = A2BRN
       DO 100 L = 1,7
       LTLEV = L + LTIME*7
       CALL DSXY(VT(1,1,2),TS(1,1,LTLEV))
       CALL DSYDX(VT(1,1,3),US(1,1,K7NEW+L-1))
       CALL DSXDY(VT(1,1,4),US(1,1,K7NEW+L-1))
       CALL DSYDX(VT(1,1,5),VS(1,1,K7NEW+L-1))
       CALL DSXDY(VT(1,1,6),VS(1,1,K7NEW+L-1))
       DO 1 J = 1, 45
       DO 1 I = 1, 53
       PHI(I,J,L) = PHI(I,J,L) + ALPWT*(CP*VT(I,J,2)*VT(I,J,5) +
     A              VT(I,J,3))
       PI(I,J,L) = PI(I,J,L) + ALPWT*(CP*VT(I,J,2)*VT(I,J,6) +
     A             VT(I,J,4))
1      CONTINUE
100    CONTINUE
       RETURN
       END
       SUBROUTINE TENDB
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENDB          ADVANCES U & V FORWARD ONE STEP
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES TENDENCIES OF U & V, INCLUDING BOUNDARY LAYER
C   FRICTION AND LINEAR DIFFUSION, AND INTEGRATES FORWARD ONE TIME
C   STEP.
C
C USAGE:  CALL TENDB
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS...    FORCAST VARIABLES PLUS FIXED FIELDS       /DEABLK/
C     XDIF        ARRAY OF DIFFUSION COEFFICIENTS           /BCOEF/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS       U & V ADVANCED TO TAU+1 LEVEL           /DEABLK/
C                 STORED AT THE LNEW TIME LEVEL
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSYDX,DSXDY,DSXY,DEL2XY,TENXY                         LFM LIB
C     VHSQRT                                                FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 ,3 )
      COMMON /VERTV/ IVVEL( 53 , 45 ,8 )
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE(12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ), PI( 53 , 45 ,7 )
      COMMON /BCOEF/  XDIF( 53 , 45 ),XBND( 53 , 45 )
      SAVE
C
C  FIRST STORE VORTICITY IN VT(4)
C
       CALL DSYDX(VT(1,1,1),VS(1,1,K7MID))
       CALL DSXDY(VT(1,1,2),US(1,1,K7MID))
       CALL DSXY(VT(1,1,3),MF)
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,4)=VT(IQ2W6E,1,3)*(VT(IQ2W6E,1,1)-VT(IQ2W6E,1,2))
88890 CONTINUE
       IF ( K .NE. 1 ) GO TO 5
       CALL DSXY(VT(1,1,KL+4),US(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+6),VS(1,1,K7MID))
5      IF( K .EQ. 7 ) GO TO 6
       CALL DSXY(VT(1,1,KH+4),US(1,1,K7MID+1))
       CALL DSXY(VT(1,1,KH+6),VS(1,1,K7MID+1))
6      CONTINUE
C
C  FORM VERTICAL ADVECTION TERMS HERE, STORE IN VT(9 & 10)
C  USE VT(24 & 25) TO STORE LOWER LEVEL TERMS
C  PRIOR TO DOING THE VERTICAL BAR
C
       IF( K .NE. 1 ) GO TO 10
       CALL DSXY(VT(1,1,11),PSIG(1,1,K2MID))
      DO 88900 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,9)=SIGDOT(IQ2W6E,1,1)*(VT(IQ2W6E,1,KL+4)-VT(
     *   IQ2W6E,1,KH+4))*DTDS0/2.E0
         VT(IQ2W6E,1,10)=SIGDOT(IQ2W6E,1,1)*(VT(IQ2W6E,1,KL+6)-VT
     *   (IQ2W6E,1,KH+6))*DTDS0/2.E0
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,9)*6.E0*BTHK3/VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,25)=VT(IQ2W6E,1,10)*6.E0*BTHK3/VT(IQ2W6E,1,11)
88900 CONTINUE
       GO TO 20
10     XX = DTDS1
       IF ( K .GE. 5 ) XX = DTDS2
       IF ( K.EQ.4 .OR. K.EQ.7 ) THEN
      DO 88940 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,23)=0.E0
88940 CONTINUE
      END IF
      IF ( K.LE.3 ) THEN
      DO 88950 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,23)=SIGDOT(IQ2W6E,1,K)
88950 CONTINUE
      END IF
      IF ( K.EQ.5 .OR. K.EQ.6 ) THEN
      DO 88960 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,23)=SIGDOT(IQ2W6E,1,K-1)
88960 CONTINUE
      END IF
      DO 88970 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,11)=VT(IQ2W6E,1,23)*(VT(IQ2W6E,1,KL+4)-VT(IQ2W6E,
     *   1,KH+4))*XX
         VT(IQ2W6E,1,12)=VT(IQ2W6E,1,23)*(VT(IQ2W6E,1,KL+6)-VT(IQ2W6E,
     *   1,KH+6))*XX
         VT(IQ2W6E,1,9)=(VT(IQ2W6E,1,11)+VT(IQ2W6E,1,24))*0.5E0
         VT(IQ2W6E,1,10)=(VT(IQ2W6E,1,12)+VT(IQ2W6E,1,25))*0.5E0
         VT(IQ2W6E,1,24)=VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,25)=VT(IQ2W6E,1,12)
88970 CONTINUE
20     CONTINUE
       IF ( K.GT.2 ) GO TO 30
C
C BOUNDARY LAYER FRICTION HERE, ADDED TO VERTICAL ADVECTION TERM
C MODIFIED 29 NOV 1984 BY DENNIS DEAVEN TO INCLUDE EFFECT IN LAYER 2
C
       CALL DSXY2N(VT(1,1,11),US(1,1,K7OLD+1-K))
       CALL DSXY2N(VT(1,1,12),VS(1,1,K7OLD+1-K))
C
C  2.432 E-3  =  ROE * G /  DELTA(P)
C
       IVTMP =      2385
       AAAA = 0.2/3000.0
       BBBB = 1.0
       IF(K.EQ.2) AAAA = 0.8/3000.0
       IF(K.EQ.2) BBBB = 0.0
      DO 89030 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,13)=DT*2.432E-3*CD(IQ2W6E,1)*(AAAA*ZSTAR(IQ2W6E,1
     *   )+BBBB)*SQRT(VT(IQ2W6E,1,11)*VT(IQ2W6E,1,11) +
     *   VT(IQ2W6E,1,12)*VT(IQ2W6E,1,12))
         VT(IQ2W6E,1,9)=VT(IQ2W6E,1,9)+VT(IQ2W6E,1,13)*VT(IQ2W6E,1,11)
         VT(IQ2W6E,1,10)=VT(IQ2W6E,1,10)+VT(IQ2W6E,1,13)*VT(IQ2W6E,1,1
     *   2)
89030 CONTINUE
30     CONTINUE
C
C  CALCULATE U AND V TENDENCIES HERE
C  FIRST WE NEED THE KINETIC ENERGY DERIVATIVES WHICH WILL BE STORED IN
C  VT(11 & 12)
C
      DO 89060 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,13)=.5E0*MF(IQ2W6E,1)*(US(IQ2W6E,1,K7MID)*US
     *   (IQ2W6E,1,K7MID)+VS(IQ2W6E,1,K7MID)*VS(IQ2W6E,1,K7MID))
89060 CONTINUE
       CALL DSYDX(VT(1,1,11),VT(1,1,13))
       CALL DSXDY(VT(1,1,12),VT(1,1,13))
       CALL DSXY(VT(1,1,14),F)
      DO 89070 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,14)=1.458E-4*DT*VT(IQ2W6E,1,14)
89070 CONTINUE
       DO 16 J = 1, 45
       DO 16 I = 1, 53
       US(I,J,K7NEW) = -VT(I,J,9) + VT(I,J,KL+6)*VT(I,J,14) + DTDX *
     A                 (VT(I,J,KL+6)*VT(I,J,4)-PHI(I,J,K)-VT(I,J,11))
       VS(I,J,K7NEW) = -VT(I,J,10) -(VT(I,J,KL+4)*VT(I,J,14)) - DTDX *
     A                 (VT(I,J,KL+4)*VT(I,J,4)+PI(I,J,K) +VT(I,J,12))
16     CONTINUE
C
C  ADVANCE TENDENCIES FORWARD IN TIME
C
       C1 = 2.E0
       IF( ITSW .EQ. 1 ) C1 = 1.E0
       CALL DEL2XY(VT(1,1,16),US(1,1,K7OLD))
       CALL DEL2XY(VT(1,1,18),VS(1,1,K7OLD))
       CALL TENXY(VT(1,1,17),US(1,1,K7NEW))
       CALL TENXY(VT(1,1,19),VS(1,1,K7NEW))
       DO 17 J = 1, 45
       DO 17 I = 1, 53
       US(I,J,K7NEW) = XDIF(I,J) *  VT(I,J,16) + VT(I,J,17)
     A                 * C1 + US(I,J,K7OLD)
       VS(I,J,K7NEW) = XDIF(I,J) * VT(I,J,18) + VT(I,J,19)
     A                 * C1 + VS(I,J,K7OLD)
17     CONTINUE
       RETURN
       END
       SUBROUTINE SETBC
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SETBC          SETS LATERAL BOUNDARY CONDITIONS ON U & V
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES LATERAL BOUNDARY DIFFUSIVE NUDGE FOR U & V.
C   PERIMETER VALUES ARE ALSO RESTORED.
C
C USAGE:  CALL SETBC
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      LATERAL BOUNDARY VALUES                   /PBNDPE/
C     BLARGE                DITTO                           /PBNDPE/
C     US,VS       U & V COMPONENTS OF THE WIND              /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS       U & V WITH BOUNDARY CONDITIONS APPLIED    /DEABLK/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     BNDLAP                                                LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53, 45, 8 )
      SAVE
C
       DO 10 K = 1,7
         L7 = K + LNEW * 7
         CALL BNDLAP(ALARGE(1,1,K),BLARGE(1,1,K),US(1,1,L7))
         CALL BNDLAP(ALARGE(1,1,K+7),BLARGE(1,1,K+7),VS(1,1,L7))
10     CONTINUE
       RETURN
       END
       SUBROUTINE BNDLAP(A,B,C)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: BNDLAP         COMPUTES LATERAL BOUNDARY CONDITIONS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES LATERAL BOUNDARY REGION LAPLACIAN (DIFFUSIVE
C   NUDGE) AND RESTORES PRIMETER VALUES.
C
C USAGE:  CALL BNDLAP(A,B,C)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A           ONE LAYER OF BOUNDARY VALUES              ARGUMENT
C                 (TOP AND BOTTOM ROWS OF GRID)
C     B           ONE LAYER OF BOUNDARY VALUES (SIDES OF GRID) ARG
C     C           ANY FORECAST VARIABLE (ONE LAYER)
C     XBND        ARRAY OF DIFFUSION COEFICIENTS            ARGUMENT
C                 (ZERO INTERIOR OF GRID, .04 ELSEWHERE)
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     C           FORECAST VARIABLE WITH B.C. APPLIED       ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DEL2XY                                                LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT    REAL (A-H,O-Z)
      REAL        A( 53 ,12 ), B( 12, 33 ),C( 53 , 45 )
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
C
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE(12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /BCOEF/  XDIF( 53 , 45 ),XBND( 53 , 45 )
      SAVE
C
C  FIRST FILL VT(16) WITH VALUES FROM A & B AT PROPER LOCATIONS
C
       NIJ =      2385
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=0.E0
88890 CONTINUE
       N6I = 6 *  53
      DO 88900 IQ2W6E=1,N6I
         VT(IQ2W6E,1,16)=A(IQ2W6E,1)
         VT(IQ2W6E,45-5,16)=A(IQ2W6E,7)
88900 CONTINUE
       JEND =  45  - 6
       DO 1 J = 7,JEND
      DO 88920 IQ2W6E=1,6
         VT(IQ2W6E,J,16)=B(IQ2W6E,J-6)
         VT(IQ2W6E+47,J,16)=B(IQ2W6E+6,J-6)
88920 CONTINUE
1      CONTINUE
       CALL DEL2XY(VT(1,1,17),VT(1,1,16))
       DO 2 J = 1, 45
       DO 2 I = 1, 53
       C(I,J) = C(I,J) - XBND(I,J) * VT(I,J,17)
2      CONTINUE
      DO 88940 IQ2W6E=1,LI
         C(IQ2W6E,1)=A(IQ2W6E,1)
         C(IQ2W6E,45)=A(IQ2W6E,12)
88940 CONTINUE
       DO 3 J = 2,6
         C(1,J)    = A(1,J)
         C( 53 ,J) = A( 53 ,J)
3      CONTINUE
       JST =  45  - 5
       DO 4 J = JST,       44
         C(1,J)    = A(1,J - JST + 7)
         C( 53 ,J) = A( 53 ,J - JST + 7)
4      CONTINUE
       JEND =  45  - 6
       DO 5 J = 7,JEND
         C(1,J)    = B(1,J-6)
         C( 53 ,J) = B(12,J-6)
5      CONTINUE
       RETURN
       END
       SUBROUTINE TSMO
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TSMO           CONTROLS CALLS TO TIME SMOOTHER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: APPLIES ROBERT TIME FILTER TO US,VS,TS,PSIG,AND WS
C
C USAGE:  CALL TSMO
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES AT THREE TIME LEVELS   /DEABLK/
C     ALP         ALPHA COEFFICIENT FOR TIME SMOOTHER       /TTME/
C     LOLD        TIME INDEX FOR TAU-1 LEVEL                /INDEX/
C     LMID             DITTO     TAU   LEVEL                /INDEX/
C     LNEW             DITTO     TAU+1 LEVEL                /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES AFTER T SMOOTHER       /DEABLK/
C                 (LMID (TAU) LEVEL HAS BEEN SMOOTHED)
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DOTSMO                                                LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        ICE, MF
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53 , 12, 23 ), BB( 12, 33, 23 )
      COMMON /DEABLK/ US( 53 , 45, 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      SAVE
C
       XAA =1.E0 -2.E0 * ALP
       LEN =  53  *  45  * 7
       L1 = 1 + LOLD * 7
       L2 = 1 + LMID * 7
       L3 = 1 + LNEW * 7
       CALL DOTSMO(US(1,1,L1),US(1,1,L2),US(1,1,L3),LEN,XAA,ALP)
       CALL DOTSMO(VS(1,1,L1),VS(1,1,L2),VS(1,1,L3),LEN,XAA,ALP)
       CALL DOTSMO(TS(1,1,L1),TS(1,1,L2),TS(1,1,L3),LEN,XAA,ALP)
       LEN =  53  *  45  * 3
       L1 = 1 + LOLD * 3
       L2 = 1 + LMID * 3
       L3 = 1 + LNEW * 3
       CALL DOTSMO(WS(1,1,L1),WS(1,1,L2),WS(1,1,L3),LEN,XAA,ALP)
       LEN =  53  *  45  * 2
       L1 = 1 + LOLD * 2
       L2 = 1 + LMID * 2
       L3 = 1 + LNEW * 2
       CALL DOTSMO(PSIG(1,1,L1),PSIG(1,1,L2),PSIG(1,1,L3),LEN,XAA,ALP)
       RETURN
       END
       SUBROUTINE DOTSMO(A1,A2,A3,L,XAA,ALP)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DOTSMO         ROBERT TIME FILTER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C ABSTRACT: APPLIES ROBERT TIME FILTER TO ENTIRE LFM DOMAIN
C
C USAGE:  CALL DOTSMO(A1,A2,A3,L,XAA,ALP)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     A1          ANY FORECAST VARIABLE AT TAU-1 LEVEL      ARGUMENT
C     A2                 DITTO             TAU   LEVEL      ARGUMENT
C     A3                 DITTO             TAU+1 LEVEL      ARGUMENT
C     L           LENGTH OF VECTORS A1,A2,A3 (LI X LJ X LK) ARGUMENT
C     XAA         COEFFICIENT FOR TAU LEVEL                 ARGUMENT
C     ALP               DITTO     TAU-1 AND TAU+1 LEVELS    ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A2          TAU LEVEL AFTER TIME FILTERING            ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
       IMPLICIT REAL (A-H,O-Z)
       REAL A1(*),A2(*),A3(*)
       SAVE
C
       DO 10 I = 1,L
         A2(I) = A2(I) * XAA + ALP * (A1(I) + A3(I))
10     CONTINUE
C
       RETURN
       END
      SUBROUTINE TTOARY(JROW,A)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TTOARY         SIGMA STRIPS TO MODEL FORMAT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: MOVES ALL MODEL VARIABLES FROM SIGMA STRIP BUFFER
C   TO COMMON BLOCK. INCLUDES TAU-1 AND TAU-1 TIME LEVELS PLUS
C   ALL FIXED FIELDS.
C
C USAGE:  CALL TTOARY(JROW,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JROW        ROW INDEX FOR SIGMA STRIP BUFFER          ARGUMENT
C     A           SIGMA STRIP BUFFER (ONE ROW)              ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      REAL A( 3816 )
      REAL ICE, MF
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53, 45, 8 )
      SAVE
C
      N1  =  53 * 36 + 1
      ND  =  53 * 9
      N2  =  N1 + ND
      N3  =  N2 + ND
      N4  =  N3 + ND
      N5  =  53 * 28
      N6  =  53 * 8
      N7  =  53
      N8  =  53 * 12
      N9  =  ND +  1
      N10 =  53 * 18 + 1
      N11 =  53 * 27 + 1
      N12 =  53 *  7 + 1
      N13 =  53 *  8 + 1
      N14 =  53 * 43 + 1
      N15 =  53 * 16 + 1
      N16 =  53 * 17 + 1
      N17 =  53 * 26 + 1
      N18 =  53 * 30 + 1
      N19 =  53 * 33 + 1
      N20 =  53 * 69 + 1
      N21 =  53 * 25 + 1
      N22 =  53 * 29 + 1
      N23 =  53 * 44 + 1
      N24 =  53 * 65 + 1
      N25 =  53 * 52 + 1
      DO 10 K = 1, 7
      KOLD = K
      KMID = K +  7
      NPLUS = (K-1) *  53
      DO 88890 IQ2W6E=1,N7
         US(IQ2W6E,JROW,KMID)=A(NPLUS+IQ2W6E)
         US(IQ2W6E,JROW,KOLD)=A(N1+NPLUS+IQ2W6E-1)
         VS(IQ2W6E,JROW,KMID)=A(N9+NPLUS+IQ2W6E-1)
         VS(IQ2W6E,JROW,KOLD)=A(N2+NPLUS+IQ2W6E-1)
         TS(IQ2W6E,JROW,KMID)=A(N10+NPLUS+IQ2W6E-1)
         TS(IQ2W6E,JROW,KOLD)=A(N3+NPLUS+IQ2W6E-1)
88890 CONTINUE
      IF (K .GT. 3) GO TO 10
      KMID = K + 3
      DO 88950 IQ2W6E=1,N7
         WS(IQ2W6E,JROW,KMID)=A(N19+NPLUS+IQ2W6E-1)
         WS(IQ2W6E,JROW,KOLD)=A(N20+NPLUS+IQ2W6E-1)
         SATW(IQ2W6E,JROW,K)=A(N18+NPLUS+IQ2W6E-1)
88950 CONTINUE
      IF (K .GT. 2) GO TO 10
      KMID = K + 2
      DO 88980 IQ2W6E=1,N7
         PSIG(IQ2W6E,JROW,KMID)=A(N11+NPLUS+IQ2W6E-1)
         PSIG(IQ2W6E,JROW,KOLD)=A(N4+NPLUS+IQ2W6E-1)
88980 CONTINUE
10    CONTINUE
      DO 89000 IQ2W6E=1,N7
         ST(IQ2W6E,JROW)=A(N12+IQ2W6E-1)
         ICE(IQ2W6E,JROW)=A(N13+IQ2W6E-1)
         COSZEN(IQ2W6E,JROW)=A(N14+IQ2W6E-1)
         XM(IQ2W6E,JROW)=A(N15+IQ2W6E-1)
         F(IQ2W6E,JROW)=A(N16+IQ2W6E-1)
         CD(IQ2W6E,JROW)=A(N17+IQ2W6E-1)
         PREC(IQ2W6E,JROW)=A(N21+IQ2W6E-1)
         ZSTAR(IQ2W6E,JROW)=A(N22+IQ2W6E-1)
         ALON(IQ2W6E,JROW)=A(N23+IQ2W6E-1)
         ALAT(IQ2W6E,JROW)=A(N24+IQ2W6E-1)
         MF(IQ2W6E,JROW)=A(N25+IQ2W6E-1)
89000 CONTINUE
      RETURN
      END
       SUBROUTINE WATER
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: WATER          LARGE AND SUB-GRID SCALE MOIST PHYSICS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: PERFORMS MOIST AND DRY ADJUSTMENTS IN THE FOLLOWING ORDER:
C   (1)  LARGE SCALE PRECIPITATION INCLUDING EVAPORATION ON THE WAY
C        DOWN.  DONE EVERY TIME STEP. (NO LATENT HEAT RELEASE DURING
C        FIRST 4 HOURS OF INTEGRATION).
C   (2)  SUB GRID SCALE CONVECTION WITH NO EVAPORATION. DONE EVERY TIME
C        STEP DURING FIRST 4 HOURS OF INTEGRATION AND ONCE PER TIME
C        STEP THEREAFTER. (NO LATENT HEAT RELEASE FIRST 4 HOURS)
C   (3)  DRY CONVECTIVE ADJUSTMENT DONE EVERY TIME STEP.
C***NOTE***   THIS ROUTINE IS CALLED ONCE PER TIME STEP WHEN K = 7
C             AFTER MASS AND MOISTURE VARIABLES HAVE BEEN STEPPED
C             FORWARD IN TIME
C
C USAGE:  CALL WATER
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C     MNSTEP      TIME STEP INDEX (1,2,3...NPHOUR)          /TTME/
C     K7NEW..     LAYER/TIME INDICIES                       /INDEX/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     TS          POT. TEMP. AFTER ADJUSTMENTS TO TAU+1     /DEABLK/
C     WS          PRECIP. WATER        DITTO                  DITTO
C     PSIG        DP/D(SIGMA)          DITTO                  DITTO
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     LCL,UNPSEP                                            LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      REAL        PSS(8)
      LOGICAL     THIN,CONDIN
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ),  PI( 53 , 45 ,7 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      SAVE
C
      SV(T) = 6.1078E0 * EXP( 17.2694E0*T/(T + 237.3E0))
      EEP(T,SVA,P) = EXP((596.73E0-0.601E0*T) *
     2                   ((0.62197E0*SVA)/(P - SVA))/
     3                   (0.24E0*(T + 273.16E0)))
C----------------------------------------------------------------------
C  NOTE*****  THIS ROUTINE IS ONLY CALLED ONCE WHEN K = 7
C  USE VT(16) TO STORE RAIN ON THE WAY DOWN
C  USE VT(17) TO STORE SUPERSATURATION VALUE
C  USE VT(18) TO STORE TEMP SUPERS (B IN OLD CODE)
C----------------------------------------------------------------------
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=0.E0
88890 CONTINUE
       DO 160 LL = 1,3
      DO 88900 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,18)=VT(IQ2W6E,1,16)
88900 CONTINUE
       LAY = 4 - LL
       IF( LAY.EQ.3)THEN
      DO 88910 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,19)=VT(IQ2W6E,1,10)
88910 CONTINUE
      END IF
       IF( LAY .EQ.1 )THEN
      DO 88920 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,19)=BTHICK
88920 CONTINUE
      END IF
      DO 88930 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,17)=WS(IQ2W6E,1,K3NEW+LAY-1)-SATW(IQ2W6E,1,LAY)
         VT(IQ2W6E,1,16)=VT(IQ2W6E,1,16)+VT(IQ2W6E,1,17)
88930 CONTINUE
C****  WHERE (VT(1,1,16  ;  NIJ).LE.0.E0)VT(1,1,17  ;  NIJ) =0.E0
         IQ2W6E=IQ2W6E
      DO 88950 IQ2W6E=1,NIJ
         IF(.NOT.(VT(IQ2W6E,1,16).LE.0.E0))GO TO 88950
         VT(IQ2W6E,1,17)=0.E0
88950 CONTINUE
         IQ2W6E=IQ2W6E
C****  WHERE (VT(1,1,16  ;  NIJ).GE.0.E0)VT(1,1,18  ;  NIJ) =0.E0
         IQ2W6E=IQ2W6E
      DO 88960 IQ2W6E=1,NIJ
         IF(.NOT.(VT(IQ2W6E,1,16).GE.0.E0))GO TO 88960
         VT(IQ2W6E,1,18)=0.E0
88960 CONTINUE
         IQ2W6E=IQ2W6E
C****  WHERE (VT(1,1,16  ;  NIJ).LT.0.E0)VT(1,1,16  ;  NIJ) =0.E0
C****  VT(1,1,17  ;  NIJ) = VT(1,1,17  ;  NIJ) - VT(1,1,18  ;  NIJ)
         IQ2W6E=IQ2W6E
      DO 88970 IQ2W6E=1,NIJ
         IF(.NOT.(VT(IQ2W6E,1,16).LT.0.E0))GO TO 88970
         VT(IQ2W6E,1,16)=0.E0
88970 CONTINUE
         IQ2W6E=IQ2W6E
      DO 88980 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,17)=VT(IQ2W6E,1,17)-VT(IQ2W6E,1,18)
         WS(IQ2W6E,1,K3NEW+LAY-1)=WS(IQ2W6E,1,K3NEW+LAY-1)-VT(IQ2W6E,1
     *   ,17)
88980 CONTINUE
       IF( IHOUR .LE. 3 ) GO TO 160
      DO 89000 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,20)=1102.4145E0*VT(IQ2W6E,1,17)/(PI(IQ2W6E,1,LAY)
     *   *VT(IQ2W6E,1,19))
         TS(IQ2W6E,1,K7MID+LAY-7)=TS(IQ2W6E,1,K7MID+LAY-7)+VT(IQ2W6E,1
     *   ,20)
         TS(IQ2W6E,1,K7NEW+LAY-7)=TS(IQ2W6E,1,K7NEW+LAY-7)+VT(IQ2W6E,1
     *   ,20)
89000 CONTINUE
160    CONTINUE
      DO 89030 IQ2W6E=1,NIJ
         PREC(IQ2W6E,1)=PREC(IQ2W6E,1)+VT(IQ2W6E,1,16)
89030 CONTINUE
C----------------------------------------------------------------------
C  MOIST CONVECTION HERE ONLY FOR MNSTEP EQUAL TO ONE
C  DRY ADJUSTMENT FOR ALL VALUES OF MNSTEP
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C          CONVECTION ROUTINE    AT GRID POINTS
C     CHECK VERTICAL TEMPERATURE AT GRIDPOINTS FOR STABILITY
C
C        SKIP CONVECTION IF IN BOUNDARY ZONE
C
C
C        DO MOIST CONVECTIVE ADJUSTMENT EVERY TIME STEP
C        FOR THE FIRST FOUR HOURS OF THE FORECAST
C        (BUT DISCARD ANY LATENT HEAT AND RAIN RESULTING)
C
C        AFTER THEN, DO MOIST IN FIRST TIME STEP OF HOUR
C        AND KEEP RAIN AND LATENT HEAT. (MORE ENGINEERING).
C
C         DRY CONVECTIVE ADJUST DONE EVERY TIME, NO MATTER WHAT.
C
      JCONV = LJ - 5
      ICONV = LI - 5
      IF(IHOUR.LE.3)GO TO 8642
      IF(MNSTEP.NE.1)GO TO 2042
 8642 CONTINUE
      DO 89040 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,1)=50.E0+3.E0*PSIG(IQ2W6E,1,K2NEW)+3.E0*PSIG
     *   (IQ2W6E,1,K2NEW+1)+BTHICH
         VT(IQ2W6E,1,2)=VT(IQ2W6E,1,1)-.5E0*PSIG(IQ2W6E,1,K2NEW)-BTHIC
     *   H
         VT(IQ2W6E,1,3)=VT(IQ2W6E,1,2)-PSIG(IQ2W6E,1,K2NEW)
         VT(IQ2W6E,1,4)=VT(IQ2W6E,1,3)-PSIG(IQ2W6E,1,K2NEW)
         VT(IQ2W6E,1,5)=BTHICK
         VT(IQ2W6E,1,6)=PSIG(IQ2W6E,1,K2NEW)
         VT(IQ2W6E,1,7)=PSIG(IQ2W6E,1,K2NEW)
         VT(IQ2W6E,1,8)=PSIG(IQ2W6E,1,K2NEW)
89040 CONTINUE
      DO 830 J = 6,JCONV
      DO 830 I = 6,ICONV
      DO 830 L = 1,3
      IF (WS(I,J,K3NEW+L-1) .LT. WS(I,J,K3MID+L-1)) GO TO 830
      RH = WS(I,J,K3NEW+L-1) / SATW(I,J,L)
      CONDIN = .FALSE.
      IF (RH.GE.1.E0) GO TO 810
      IF (RH.LT.0.75E0) GO TO 830
C     TEST FOR CONDITIONAL INSTABILITY
C     FIRST COMPUTE LCL  ,  NEEDS POTENTIAL TEMP AND MIXING RATIO
C     RETURNS PRESSURE AND TEMP  (DEG C)
      THE = TS(I,J,K7NEW+L-7) + 1.5E0
      TC  = THE * PI(I,J,L ) - 273.16E0
      ES  = SV(TC)
      SATMIX = .622E0 * (ES/(VT(I,J,L) - ES))
      RMIX = (.622E0 * SATMIX * RH)/(.622E0 + SATMIX*(1.E0-RH))
      CALL LCL(THE,RMIX,PLCL,TLCL,NS)
      IF (NS .GE. 20)  GO TO 2042
      IF (PLCL.LE.VT(I,J,L+1)) GO TO 830
C     GERE ONLY IF SATURATION REACHED BY RISING PARCEL BELOW MIDDLE OF
C     LAYER ABOVE
      ES = SV(TLCL)
      PSEPOT = THE * EEP(TLCL, ES, PLCL)
      CONDIN = .TRUE.
C   NOW MAKE ADJUSTMENTS IF NECESSARY  -  SAME CODE AS FOR SATURATED
      GO TO 820
C     COME TO 810 FOR SATURATED LAYER
810   THE = TS(I,J,K7NEW+L-7)
      TC = THE * PI(I,J,L) - 273.16E0
      ES = SV(TC)
      PSEPOT = THE * EEP(TC, ES, VT(I,J,L))
C     FIND TEMP ALONG MOIST ADIABAT IN LAYER ABOVE
  820 CONTINUE
      PIIN=1.E0/PI(I,J,L+1)
      CALL UNPSEP(PSEPOT,VT(I,J,L+1),PIIN,TC,TR,NS)
C  IF(NS .GE. 100)  SKIP CONVECTION AT THIS POINT
      IF (NS .GE. 100) GO TO 2042
      TABOV = PIIN * (TR+273.16E0)
      ADJ = TS(I,J,K7NEW+L-6) - TABOV
      IF (ADJ.GE.-0.1E0) GO TO 830
      DTH2 = -ADJ *.1E0 * NPHOUR
      CNRAIN = DTH2/PIIN*4.1004E-4*VT(I,J,L+5)
      CNRAIN = MIN(CNRAIN,.25E0*WS(I,J,K3NEW+L-1))
      DTH2   = CNRAIN*PIIN/(4.1004E-4*VT(I,J,L+5))
      DTH2   = DTH2*0.5E0
      WS(I,J,K3NEW+L-1) = WS(I,J,K3NEW+L-1) - CNRAIN
C  FOR  -LFM- IF LSW6  EQ 1 NO CONVECTIVE RAIN IN PRECIP
      IF (LSW6 .EQ. 1) CNRAIN = 0.E0
      PREC(I,J)=PREC(I,J) + CNRAIN
C
C        DISCARD CONVECTIVE LATENT HEAT IN FIRST FOUR HOURS
C
      IF (IHOUR.LE.3) DTH2 = 0.E0
      TS(I,J,K7NEW+L-6) = TS(I,J,K7NEW+L-6) + DTH2
      TS(I,J,K7MID+L-6) = TS(I,J,K7MID+L-6) + DTH2
  830 CONTINUE
 2042    CONTINUE
C
C        HERE DO THE TEST FOR DRY CONVECTIVE INSTABILITY WITH
C        ADJUSTMENTS AS NECESSARY, PRECEEDED BY A TEST
C        FOR THIN STRATOSPHERE
C
      DO 31041 J = 6,JCONV
      DO 31041 I = 6,ICONV
      IF (PSIG(I,J,K2NEW+1) .GE.15.E0 ) GO TO 3041
      THIN = .TRUE.
      LE = 4
      LB = 7
      GO TO 6041
C     TEST FOR SUPERADIABATIC (DRY) POINTS
 3041 THIN = .FALSE.
C     TEST FOR SUPERADIABATIC (DRY) POINTS
      DO 5041  L = 2,  7
      LE1 = L
      IF(TS(I,J,K7NEW+L-7) .LT. TS(I,J,K7NEW+L-8))GO TO 7041
 5041 CONTINUE
C      ALL LAYERS STABLE
        GO  TO  28041
C     ADJUST AND CORRECT SUPERADIABATIC (DRY) POINTS ITERATIVELY
C     WITH WEIGHTS TO CONSERVE INTERNAL AND POTENTIAL ENERGY
 7041 LE = LE1 - 1
      LB = LE1
6041  A = PSIG(I,J,K2NEW+1)
      B = PSIG(I,J,K2NEW)
      PSS(8) = 50.E0
      PSS(7) = PSS(8) + A
      PSS(6) = PSS(7) + A
      PSS(5) = PSS(6) + A
      PSS(4) = PSS(5) + B
      PSS(3) = PSS(4) + B
      PSS(2) = PSS(3) + B
      PSS(1) = PSS(2) + BTHICK
      A = (PSS(1)*.001)**REAL(ROCP)
      B = (PSS(2)*.001)**REAL(ROCP)
      C = (PSS(3)*.001)**REAL(ROCP)
      D = (PSS(4)*.001)**REAL(ROCP)
      E = (PSS(5)*.001)**REAL(ROCP)
      FF= (PSS(6)*.001)**REAL(ROCP)
      G = (PSS(7)*.001)**REAL(ROCP)
      H = (PSS(8)*.001)**REAL(ROCP)
      PSS(1) = PSS(1)*A - PSS(2)*B
      PSS(2) = PSS(2)*B - PSS(3)*C
      PSS(3) = PSS(3)*C - PSS(4)*D
      PSS(4) = PSS(4)*D - PSS(5)*E
      PSS(5) = PSS(5)*E - PSS(6)*FF
      PSS(6) = PSS(6)*FF- PSS(7)*G
      PSS(7) = PSS(7)*G - PSS(8)*H
15041 A = 0.E0
      B = 0.E0
      DO 19041 L = LE,LB
      A = A + TS(I,J,K7NEW+L-7) * PSS(L)
      B = B + PSS(L)
19041 CONTINUE
      T2=A/B
      DO 22041 L=LE,LB
      TS(I,J,K7MID+L-7) = TS(I,J,K7MID+L-7) + T2 - TS(I,J,K7NEW+L-7)
      TS(I,J,K7NEW+L-7) = T2
22041 CONTINUE
      IF (THIN) GO TO 3041
      IF (LB .GE.7) GO TO 28041
      IF (TS(I,J,K7NEW+LB-6) .GE. TS(I,J,K7NEW+LB-7)) GO TO 3041
      LB = LB + 1
       GO  TO  15041
28041 CONTINUE
31041 CONTINUE
      RETURN
      END
      SUBROUTINE WKXLOX(R,V,K)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: WKXLOX         COMPUTES EXNER FUNCTION (P/1000)**R/CP
C   AUTHOR: COLLINS/DEAVEN   ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: COMPUTES EXNER FUNCTION (P/1000)**R/CP). ONE LAYER AT A
C   TIME.  COEFFICIENTS SELECTED ON BASIS OF INPUT LAYER SPECIALIZED
C   FOR THE LFM SIGMA STRUCTURE.
C
C USAGE:  CALL WKXLOX(R,V,K)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     V           PRESSURE (MB)                             ARGUMENT
C     K           LAYER INDEX  (1,2,3...7)                  ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     R           RESULT OF (V/1000)**R/CP                  ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
      IMPLICIT REAL (A-H,O-Z)
C     REAL*8 X, A, B, C, AA, D, DD, EE, BB, CC
      DIMENSION X( 2385 ),R( 2385 ),A(7),B(7),C(7),AA(7),
     A     V( 2385 ),D( 2385 ),DD(7),EE(7),BB(7),CC(7)
      SAVE
      DATA A /.307090716E0,.300288452E0,.286881785E0,.250809089E0,
     A     .212632530E0,.194542431E0,.171252900E0/
      DATA B /6.20227562E0,6.55567150E0,7.33116314E0,10.2172934E0,
     A     15.2834971E0,19.2592175E0,26.4526707E0/
      DATA C /10.4639377E0,11.9346462E0,15.5159921E0,34.0768245E0,
     A     87.4357754E0,155.452821E0,331.718416E0/
      DATA AA /9.07141448E0,9.80069206E0,11.4498818E0,18.1921874E0,
     A     31.8367605E0,44.1571982E0,68.8131876E0/
      DATA BB /7.62987465E0,8.89065657E0,12.0514485E0,30.0770832E0,
     A     89.5882855E0,176.475684E0,426.749841E0/
      DATA CC /-.882596408E0,-1.10730690E0,-1.72966258E0,-6.70680765E0,
     A     -33.1396840E0,-94.7528303E0,-354.116011E0/
      DATA DD /.174227313E0,.234950695E0,.420089488E0,2.50515711E0,
     A     20.0690665E0,84.9102286E0,488.623634E0/
      DATA EE /-.0196159669E0,-.0283862883E0,-.0577201979E0,
     A     -.524503282E0,-6.67778675E0,-42.5106142E0,-375.443207E0/
C
C      IMPLIED VALUE OF ROCP IS .285621891
C
      NIJ = 2385
      IF (K.NE.8 ) THEN
C
      DO 88890 IQ2W6E = 1,NIJ
         X(IQ2W6E) = .001*V(IQ2W6E)
         R(IQ2W6E) = C(K)*X(IQ2W6E)+B(K)
         R(IQ2W6E) = R(IQ2W6E)*X(IQ2W6E)+A(K)
         D(IQ2W6E) = EE(K)*X(IQ2W6E)+DD(K)
         D(IQ2W6E) = D(IQ2W6E)*X(IQ2W6E)+CC(K)
         D(IQ2W6E) = D(IQ2W6E)*X(IQ2W6E)+BB(K)
         D(IQ2W6E) = D(IQ2W6E)*X(IQ2W6E)+AA(K)
         D(IQ2W6E) = D(IQ2W6E)*X(IQ2W6E)+1.E0
         R(IQ2W6E) = R(IQ2W6E)/D(IQ2W6E)
88890 CONTINUE
C
      ELSE
C
C          TOP LEVEL ASSUMED AT 50 MB
C
      DO 88980 IQ2W6E=1,NIJ
         R(IQ2W6E)=0.424890620
88980 CONTINUE
C
      END IF
      RETURN
      END
