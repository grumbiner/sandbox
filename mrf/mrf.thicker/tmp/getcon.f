      SUBROUTINE GETCON(N1,N2,NGES,NRADR,NRADF,NNMOD,
     1 N3,N4,NFLPS,NSIGI,NSIGS,NSFCI,NZNLI,NSFCF,NZNLF,NSFCS,NZNLS,
     2 NDGI,NDGF,NGPKEN,
     3 MODS,NITER,INI,NSTEP,NFILES,
C-RSM3 NRSMI1,NRSMI2,NRFLIP,NRSMO1,NRSMO2,NRFLOP,NRSFLX,NRINIT,
     4 KSOUT,IFGES,IBRAD)
C....
C.................................................................
C................BEGIN TWOLOOP(COMFIBM)........................
C....
C    VERSION WITH STACKED TRANSFORMS
C....
C.................................................................
C................BEGIN comfspec...................................
C....
       COMMON /comfspec/ IDATE(4),RELVOR( 28 ),ABSVOR( 28 ),
     1 EPS( 4032 ),EPSI( 4032 ),GZ( 4033 )
       COMMON /comfspec/
     *    ZEM    ( 4033 , 28 ),
     *    DIM    ( 4033 , 28 ),
     *    TEM    ( 4033 , 28 ),
     *    RM     ( 4033 , 28 ),
     *    QM     ( 4033 )
       COMMON /comfspec/
     *    ZE     ( 4033 , 28 ),
     *    DI     ( 4033 , 28 ),
     *    TE     ( 4033 , 28 ),
     *    RQ     ( 4033 , 28 ),
     *    DPDLAM ( 4033 ),
     *    DPDPHI ( 4033 ),
     *    ULN    ( 4033 , 28 ),
     *    VLN    ( 4033 , 28 ),
     *    Q      ( 4033 )
       COMMON /comfspec/
     *    X      ( 4033 , 28 ),
     *    Y      ( 4033 , 28 ),
     *    RT     ( 4033 , 28 ),
     *    Z      ( 4033 ),
     *    W      ( 4033 , 28 )
C.................................................................
C................sof   comfspec...................................
C....
C.................................................................
C................BEGIN comfgrid...................................
C....
       COMMON /comfgrid/
     * COLRAD( 47 ),WGT( 47 ),WGTCS( 47 ),RCS2( 47 ),
     * COLRAB( 47 ),WGB( 47 ),WGBCS( 47 ),RBS2( 47 ),
     * SINLAT( 47 ),
     * SINLAB( 384 , 47 ),COSLAB( 384 , 47 )
C.................................................................
C................sof   comfgrid...................................
C....
C.................................................................
C................BEGIN comfphys ..................................
C....
      COMMON /comfphys/SLMSK( 384 , 47 ),HPRIME( 384 , 47 ),
     * SWH( 384 , 28 , 47 ),HLW( 384 , 28 , 47 ),
     * SFCNSW( 384 , 47 ),SFCDLW( 384 , 47 ),
     * COSZEN( 384 , 47 ),XLON( 384 , 47 ),
     * SDEC,CDEC,SLAG,SOLHR,CLSTP,LASTEP,
     * CV( 384 , 47 ),CVT( 384 , 47 ),CVB( 384 , 47 ),
     * ALBEDO( 384 , 47 ),TSFLW( 384 , 47 ),
     *       DUSFC( 384 , 47 ), DVSFC( 384 , 47 ),
     *       DTSFC( 384 , 47 ), DQSFC( 384 , 47 ),
     *      DLWSFC( 384 , 47 ),ULWSFC( 384 , 47 ),
     *GESHEM( 384 , 47 ),TSEA( 384 , 47 ),F10M( 384 , 47 ),
     *       DUGWD( 384 , 47 ),DVGWD( 384 , 47 ),
     *       U10M( 384 , 47 ),V10M( 384 , 47 ),
     *       T2M( 384 , 47 ),Q2M( 384 , 47 ),
     *       PSURF( 384 , 47 ),PSMEAN( 384 , 47 ),
     *TG3( 384 , 47 ),ZORL( 384 , 47 ),PLANTR( 384 , 47 ),
     *        SHELEG( 384 , 47 ),BENGSH( 384 , 47 ),
     *        GFLUX( 384 , 47 ),SLRAD( 384 ),
     *        SMC( 384 , 47 , 2 ),STC( 384 , 47 , 2 ),
     *        CANOPY( 384 , 47 ),RUNOFF( 384 , 47 ),
     *        TMPMAX( 384 , 47 ),TMPMIN( 384 , 47 ),
     *        EP( 384 , 47 ),CLDWRK( 384 , 47 ),
     *        HPBL( 384 , 47 ),PWAT( 384 , 47 )
      LOGICAL LASTEP
C.................................................................
C................sof   comfphys ..................................
C....
C....
C....
C.....BEGIN comfver...............................................
      COMMON/comfver/AM( 28 , 28 ),HM( 28 , 28 ),TM( 28 , 28 ),
     O               BM( 28 , 28 ),CM( 28 , 28 ),SPDMAX( 28 ),
     1 SI( 29 ),SL( 28 ),DEL( 28 ),RDEL2( 28 ),RMSDOT( 27 ),
     2 CI( 29 ),CL( 28 ),TOV( 28 ),GV( 28 ),SV( 28 ),RPI( 27 ),
     3 P1( 28 ),P2( 28 ), H1( 28 ),   H2( 28 ),RPIREC( 27 ),
     8   THOUR,DELTIM,KDT,INISTP,SL1,Z00,FHOUR,SHOUR,LIMLOW,DTCVAV,
     9   NFLIP,NFLOP,NR2DDA,FILTA,FILTB,DK,TK,PERCUT,DTSWAV,DTLWAV,
     X   COWAVE,DTWAVE,N50UFL,NUMSUM,NUMMAX,NCPUS,NCPUS1,NCLDB1
C.....sof   comfver...............................................
C....
C....
      COMMON/COMBIT/NDEX( 4032 ),SNNP1( 4032 )
      COMMON/COMBIT/LAB(4),IFIN,ICEN,IGEN,ICEN2,IENST,IENSI,RUNID,USRID
      CHARACTER*8 LAB
C....
C....
      COMMON /RADIAG/ FLUXR( 256 , 31 ,26)
C     EQUIVALENCE (FFLWUP(1,1),FLUXR(1,1,1)),(FFSWUP(1,1),FLUXR(1,1,2)),
C    1            (FSSWUP(1,1),FLUXR(1,1,3)),(FSSWDN(1,1),FLUXR(1,1,4)),
C    2            (CCHI  (1,1),FLUXR(1,1,5)),(CCMID (1,1),FLUXR(1,1,6)),
C    3            (CCLO  (1,1),FLUXR(1,1,7)),(CTPH  (1,1),FLUXR(1,1,8)),
C    4         (CTPM  (1,1),FLUXR(1,1,9)), (CTPL  (1,1),FLUXR(1,1,10)),
C    5         (CBTH  (1,1),FLUXR(1,1,11)),(CBTM  (1,1),FLUXR(1,1,12)),
C    6         (CBTL  (1,1),FLUXR(1,1,13)),(CTHTMP(1,1),FLUXR(1,1,14)),
C    7         (CTMTMP(1,1),FLUXR(1,1,15)),(CTLTMP(1,1),FLUXR(1,1,16)),
C    8         (ALBDO (1,1),FLUXR(1,1,17)),(FFSWDN(1,1),FLUXR(1,1,18)),
C    9         (SLWDN (1,1),FLUXR(1,1,19)),(SLWUP (1,1),FLUXR(1,1,20)),
C    1         (FLWUPC(1,1),FLUXR(1,1,21)),(FSWUPC(1,1),FLUXR(1,1,22)),
C    2         (SSWDNC(1,1),FLUXR(1,1,23)),(SSWUPC(1,1),FLUXR(1,1,24)),
C    3         (SLWDNC(1,1),FLUXR(1,1,25)),(TOTALC(1,1),FLUXR(1,1,26))
      COMMON /RADIAG/ CVAVG( 384 , 47 )
      COMMON /RADIAG/ ILEFT( 384 ),IRGHT( 384 ),WGTLON( 384 )
      COMMON /RADIAG/ INSLAT( 47 ),WGTLAT( 47 )
C.............................................................
C.................SOF  TWOLOOP(COMFIBM)........................
C................................................................
C...SOF INCLUDE..........................................
C  FORECAST SELECTION PARAMETERS
      COMMON/COMCON/ CON(1700),NUM(1700)
      PARAMETER(NVRKEN= 80 + 9 * 28 ,NPTKEN= 50 )
      PARAMETER(NSTKEN= 24 )
      COMMON/COMGPD/ SVDATA(NVRKEN,NPTKEN,NSTKEN),
     1               IGRD(NPTKEN),JGRD(NPTKEN),
     2               IGRDR(NPTKEN),JGRDR(NPTKEN),
     3               ITNUM,NPOINT,ISAVE,ISSHRT,ILSHRT,IKFREQ
C-WAV COMMON/COMWAV/ HSTR,USTRGG( 384 , 47 ),VSTRGG( 384 , 47 )
      NAMELIST/NAMSMF/ CON,NUM,LABL,ENDHOUR,LDEBUG,FILTA,ICEN,IGEN,ICEN2
     &                ,IENST,IENSI,RUNID,USRID,NCPUS
       LIMLOW=1
       JCAP= 62
       LEVS= 28
       FILTA= 0.92
       DT80=939.14 E 0
       MODS=4
       NITER=2
       PERCUT=27502. E 0
       ICEN=7
       IGEN= 80
       ICEN2=0
       IENST=0
       IENSI=0
       RUNID=0
       USRID=0
       CALL GNCPUS(NCPUS)
C..........................................
C
C  DEFINE UNIT NUMBERS
C
C  INPUT
      N1    = 11
      N2    = 12
      NGES  = 13
      NFLIP = 14
      NRADR = 21
      NRADF = 22
      NNMOD = 23
      NMTNV = 24
C-RSM NRSMI1= 30
C-RSM NRSMI2= 31
C-RSM NRFLIP= 32
C  OUTPUT
      N3    = 51
      N4    = 52
      NFLOP = 53
      NFLPS = 54
      NSIGI = 55
      NSIGS = 56
      NZNLI = 61
      NSFCI = 62
      NSFCF = 63
      NZNLF = 64
      NDGI  = 65
      NDGF  = 66
      NGPKEN= 67
      NSFCS = 68
      NZNLS = 69
C-RSM NRSMO1= 70
C-RSM NRSMO2= 71
C-RSM NRFLOP= 72
C-RSM NRSFLX= 73
C-RSM NRINIT= 74
C  WORK FILES
      NR2DDA = 98
C....
C.... CMEAN,CLSTP CONTROL TIME AVERAGING OF CONVECTIVE CLDS IN KUO
C....
      CLSTP=99.
C....
C...  AVERAGING INTERVAL FOR CONV CLD APPROX 3 HRS (NUM OF TIMESTEPS)
C....
      READ(NMTNV) HPRIME
      CALL ROW1NS(HPRIME)
      PRINT 100, JCAP, LEVS
100   FORMAT (1H0,'GETCON ',I3,I3,'CREATED APRIL 92')
      FILTB =(1. E 0-FILTA) * 0.5 E 0
      CALL SETSIG(CI,SI,DEL,SL,CL,RPI,N1)
      SL1=SL(1)
      DO 3 LEV=1, 28
      TOV(LEV)=300. E 0
3     CONTINUE
      CALL AMBMSV( 28 ,SI,SL,TOV,AM,BM,SV,GV,CM)
      CALL GLATS( 47 , COLRAD, WGT, WGTCS, RCS2)
      CALL GLATS( 47 , COLRAB, WGB, WGBCS, RBS2)
      CALL EPSLON(EPS,  62 )
C
C     RPI(K) = (SL(K+1)/SL(K))**RK  FROM SETSIG  K=1... 27
C
      DO 9 K=1, 27
      RPIREC(K) = 1. E 0/RPI(K)
9     CONTINUE
      DO 10 K=1, 28
      RDEL2(K)=0.5 E 0/DEL(K)
10    CONTINUE
      IND=0
      DO 7 LL=1, 63
      N=LL-2
      MAXI= 63 +1-LL
      DO 6 I=1,MAXI
      IND=IND+1
      N=N+1
      NDEX(IND*2-1) = N
      NDEX(IND*2  ) = N
      FACT=FLOAT(N*(N+1))
      SNNP1(IND*2-1) = FACT
      SNNP1(IND*2  ) = FACT
6     CONTINUE
7     CONTINUE
      DLON = 2. E 0 *  3.141593E+0  /  192 . E 0
      DO 20 J=1, 47
      DO 20 I=1, 192
        XLON(I,J) = DLON * (I-1)
        XLON(I+ 192 ,J) = XLON(I,J)
  20  CONTINUE
      DO 25 J=1, 47
        SINLAT(J) = COS(COLRAD(J))
  25  CONTINUE
      DO 30 J=1, 47
      SINLAJ = COS(COLRAB(J))
      COSLAJ = SQRT(1.  E  0 - SINLAJ*SINLAJ)
      DO 30 I=1, 192
        SINLAB(I,J) = SINLAJ
        COSLAB(I,J) = COSLAJ
        SINLAB(I+ 192 ,J) = -SINLAJ
        COSLAB(I+ 192 ,J) =  COSLAJ
  30  CONTINUE
C    INITIALIZE CV, CVT AND CVB
      DO 40 J=1, 47
      DO 40 I=1, 384
        CV (I,J) = 0. E 0
        CVT(I,J) = 0. E 0
        CVB(I,J) = 100. E 0
  40  CONTINUE
C
      DO 1 I=1,28
1     NUM(I)=0
      NUM( 1) = 11
      NUM( 2) = 11
      NUM( 3) = 51
      NUM( 4) = 52
      NUM( 5) =  0
      NUM( 6) =  1
      NUM( 7) =  0
      NUM( 8) =  1
      NUM( 9) =  8
      NUM(10) = 15
      NUM(11) =  1
      NUM(12) = 23
      NUM(13) =  1
      NUM(14) = 55
      NUM(15) =  0
      NUM(16) = 11
      NUM(17) = 51
      NUM(18) =  4
      NUM(19) =  2
      NUM(20) =  6
      NUM(21) = 15
      NUM(22) = 10
      NUM(23) =  1
      NUM(24) =  0
      NUM(25) =  0
      NUM(26) =  0
      NUM(27) =  0
      NUM(28) =  0
CTEMPORARILY SET SOME CONS AND NUMS (SETC BLOCK DATA NOT YET INCLUDED)
      NUM(1)=0
      CON(1)=0.
      CON(3)=0.        ! GSM DFINI INITIALIZATION IN HOUR OR NOT (0.)
      CON(6)=0.
      CON(7)=12.
      CON(4)=1.        ! DTSWAV IN HOUR
      CON(5)=3.        ! DTLWAV IN HOUR
C-WAV CON(10)=0.
C-RSM CON(11)=400.     ! RSM DELTIM
C-RSM CON(12)=21600.   ! RSM NESTING PERIOD IN SECOND
C-RSM CON(13)=6.0      ! RSM INITIALIZATION STEP IN HOUR OR NOT (0.)
C-RSM CON(14)=1.       ! RSM DTSWAV IN HOUR
C-RSM CON(15)=1.       ! RSM DTLWAV IN HOUR
C-RSM CON(16)=0.       ! RSM START FORECAST PERIOD
C-RSM CON(17)=36.      ! RSM ENDING FORECAST PERIOD
      NUM(799)=1
      NUM(1023)=0
      NUM(50)=0
      ENDHOUR=0.
      READ(5,NAMSMF,END=199)
      GOTO 202
CTEMPORARILY READ FROM ORIGINAL INPUT CARD IF NAMELIST IS MISSING
199   CONTINUE
      REWIND 5
      READ(5,200)(NUM(I),I=1,28)
  200 FORMAT(28I2)
202   CONTINUE
C
C  TEMPORARILY RESET SOME CONS AND NUMS
C
      READ(N1)
      READ(N1) FHOUR
      REWIND N1
      IF(FHOUR.EQ.0.) THEN
        IF(NUM(5).EQ.-1) NUM(5)=2
        IF(NUM(5).EQ.-2) NUM(5)=1
      ELSE
        IF(NUM(5).EQ.-1) NUM(5)=0
        IF(NUM(5).EQ.-2) NUM(5)=0
      ENDIF
      IF(CON(1).LE.0.) CON(1)=DT80 *80./JCAP
      IF(NUM(7).LE.0) THEN
        NUM(7)=3600.*CON(7)/CON(1)+0.99
        CON(1)=NINT(3600.*CON(7)/NUM(7))
      ELSE
        CON(7)=NUM(7)*CON(1)/3600.
      ENDIF
      IF(NUM(1023).EQ.0) NUM(1023)=NUM(7)
      IF(NUM(1).GT.0) CON(6)=NUM(1)
C
C....
C.... DTSWAV IS INTERVAL BETWEEN SHORT-WAVE HEATING CALCULATIONS
C.... DTLWAV IS INTERVAL BETWEEN LONG-WAVE HEATING CALCULATIONS
C....
      DTSWAV=CON(4)
      DTLWAV=CON(5)
      COWAVE=0.
      DTWAVE=0.
C-WAV COWAVE=CON(10)
C-WAV DTWAVE=ABS(CON(10))
C
C>YH  CVMINT - MAXIMUM CONV. CLD ACCUMULATION TIME INTERVAL IN HOURS
C....          CURRENTLY HARDWIRED AS 3 HOURS, BUT MAY BE AS AN
C....          INPUT VARIABLE.
      CVMINT = 3. E 0
      CVMINT = CON(4)
      DTCVAV = AMIN1(CVMINT, AMAX1(DTSWAV,DTLWAV))
C
C
      PRINT 201,(NUM(I),I=1,28)
201   FORMAT(1H0,'NUM=',28(1X,I2))
      PRINT *,'CON'
      PRINT *,(CON(I),I=1,10)
C
      IF(CON(6).GT.0) THEN
        KSOUT=3600.*CON(6)/CON(1)+0.5
      ELSE
        KSOUT=0
      ENDIF
C
      IF(NUM(18).NE.0)MODS=NUM(18)
      IF(NUM(19).NE.0)NITER=NUM(19)
      INI=NUM(5)
C TEST IF A GUESS FILE IS AVAILABLE (IF SO ,SET IFGES=1)
      IFGES=0
      IF(INI.NE.0) THEN
        REWIND NGES
        READ(NGES,END=6782)
        IFGES=1
 6782   CONTINUE
        REWIND NGES
      ENDIF
      IBRAD=1
      REWIND NRADR
      READ(NRADR,END=6792)
      IBRAD=0
 6792 CONTINUE
      REWIND NRADR
      NSTEP=NUM(6)
      IF(NSTEP.EQ.1)NSTEP=7
      NFILES=NUM(11)
      DK=NUM(9)
      DK=DK*(10. E 0)**NUM(10)
      TK=NUM(20)
      TK=TK*(10. E 0)**NUM(21)
      IF(NUM(20).EQ.0)TK=DK
      PRINT 105,CON(1),FILTA,DK,TK
105   FORMAT(1H ,5X,F5.0,1X,F4.2,1X,E8.2,1X,E8.2)
      NPOINT=NUM(1300)
      IF(NPOINT.LT.0.OR.NPOINT.GT.NPTKEN) THEN
        PRINT *,'KEN POINTS DISABLED - GRID POINTS EXCEED ',NPTKEN
        NPOINT=0
      ENDIF
      ISAVE=0
      ITNUM=0
      IF(NPOINT.NE.0) THEN
        ISAVE=1
        ITNUM=1
        ISSHRT=NUM(1301)
        ILSHRT=NUM(1302)
        IKFREQ=NUM(1303)
        CALL KENPRE(CON,COLRAD, 192 , 47 ,NFLIP)
      ENDIF
      N50UFL=NUM(50)
      NCPUS1=NCPUS+1
      NCLDB1=NCPUS* 384 / 256 +1
CC
CC    CALL  CMPIND  TO SET COMMON/COMIND/ FOR SUBS. TRANSI,TRANSO.
      CALL  CMPIND
CC
      CALL GPVS
      CALL GTDP
      CALL GTHE
      CALL GTMA
      CALL GPLN2I
      CALL EPSILO(EPSI, 62 )
      CALL GGOZRM(EPSI)
      CALL GFT 192
      CALL GFT 192
      CALL GRDDF
      CALL GRDKT
C
      RETURN
      END
