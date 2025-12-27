       PROGRAM F 62   28
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: FCST         MAKE GLOBAL FORECAST WITH SPECTRAL MODEL
C   PRGMMR: SELA             ORG: NMC23       DATE: 81-01-01
C
C ABSTRACT: MAKE GLOBAL FORECAST WITH SPECTRAL MODEL.
C
C PROGRAM HISTORY LOG:
C   81-01-01  SELA
C
C INPUT FILES:
C   UNIT   11    SIGMA FILE (ANALYSIS OR AT TIME T-DT)
C   UNIT   12    SIGMA FILE (AT TIME T IF NOT ANALYSIS)
C   UNIT   14    SURFACE FILE
C   UNIT   15    CO2 CONSTANTS (DEPENDENT ON VERTICAL RESOLUTION)
C   UNIT   24    MOUNTAIN VARIANCE (DEPENDENT ON HORIZONTAL RESOLUTION)
C   UNIT   43    CLOUD TUNING
C
C OUTPUT FILES:
C   UNIT   51    SIGMA FILE (AT TIME T-DT)
C   UNIT   52    SIGMA FILE (AT TIME T)
C   UNIT   53    SURFACE FILE
C   UNIT   61    INITIAL ZONAL DIAGNOSTICS
C   UNIT   63    FLUX DIAGNOSTICS
C   UNIT   64    FINAL ZONAL DIAGNOSTICS
C   UNIT   67    GRID POINT DIAGNOSTICS
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
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
C..........................................
CD      CALL W3LOG('$S','$M')
      PRINT 100
100   FORMAT (1H0,'E2 SMF2 62  28 ',
     X        ' CREATED APRIL 92  IBM ORDER ')
C
      CALL GETCON(N1,N2,NGES,NRADR,NRADF,NNMOD,
     1 N3,N4,NFLPS,NSIGI,NSIGS,NSFCI,NZNLI,NSFCF,NZNLF,NSFCS,NZNLS,
     2 NDGI,NDGF,NGPKEN,
     3 MODS,NITER,INI,NSTEP,NFILES,
C-RSM3 NRSMI1,NRSMI2,NRFLIP,NRSMO1,NRSMO2,NRFLOP,NRSFLX,NRINIT,
     4 KSOUT,IFGES,IBRAD)
C
C DFINI:  DO DIGITAL FILTER INITIALIZATION SETUP
C         NUMMAX AND NUMSUM SAVED AND PASSED IN COMVER
C-DFI IF( CON(3).NE.0.0 ) THEN
C-DFI    NUMMAX=NINT(CON(3)*3600./CON(1)/2.)
C-DFI    NUMSUM=-NUMMAX-1
C-DFI    PRINT *,' DO GSM DIGITAL FILTER INITIALIZATION '
C-DFI    CALL DFINI(0,CON(3),CHOUR,SOLSEC)
C-DFI ELSE
C-DFI   NUMMAX=0
C-DFI   NUMSUM=-1
C-DFI ENDIF
      KDT=1
      INISTP=0
C
      IF(IBRAD.NE.1) THEN
        CALL GETRAD(NRADR,Q,QM,SFCNSW,SFCDLW,COSZEN,SDEC,CDEC,SLAG,
     1              SWH,HLW)
      ENDIF
C
C-DG3 CALL INDDIA
C-DG3 CALL ZERDIA(FHOUR)
      CALL ZNLZER
C-WAV IF(DTWAVE.GT.0.) THEN
C-WAV HSTR=FHOUR
C-WAV USTRGG=0.
C-WAV VSTRGG=0.
C-WAV ENDIF
      IF(INI.NE.0)THEN
      ISAVE=0
      NANLH=81
      NGESH=80
      NGEST=82
      NGESTH=83
      IF(INI.EQ.2.AND.IFGES.EQ.1)CALL DIABH(NGES,NGESH,NSTEP,INI,N1)
      IF(             IFGES.EQ.1)CALL GEST(NGES,NGEST,N1)
      IF(INI.EQ.2)               CALL DIABH(N1  ,NANLH,NSTEP,INI,N1)
      CALL DOINI(N1,NANLH,IFGES,NGEST,NGESH,NGESTH,MODS,NITER,NNMOD,INI)
      IF(NUM(13).EQ.0) GO TO 5
      CALL TWRITE(NSIGI,FHOUR,IDATE,Z,Q,TE,DI,ZE,RQ,SL,SI,GZ,Z00,N1)
      WRITE(NSIGI)GESHEM
      CLOSE(NSIGI)
5     CONTINUE
C-DG3 CALL ZERDIA(FHOUR)
      CALL ZNLZER
C-WAV IF(DTWAVE.GT.0.) THEN
C-WAV HSTR=FHOUR
C-WAV USTRGG=0.
C-WAV VSTRGG=0.
C-WAV ENDIF
      CALL STEP1(JUNK1,JUNK1,0,INI,JUNK1,SOLSEC)
      ELSE
      IF(FHOUR.EQ.0.)CALL STEP1(N1,N1,1,INI,N1,SOLSEC)
      IF(FHOUR.NE.0.)CALL STEP1(N1,N2,1,INI,JUNK1,SOLSEC)
      ENDIF
C DFINI : CALL AFTER STEP1
C-DFI IF( CON(3).NE.0.0 ) CALL DFINI(1,CON(3),CHOUR,SOLSEC)
C-DFI LIMLOW=LIMLOW-NUMMAX
      IF(FHOUR.EQ.0.) KDT=1
      IF(FHOUR.EQ.0.) THOUR=FHOUR+DELTIM/3600.
      IF(FHOUR.EQ.0.)
     &CALL GLOOPZ(NZNLI,NSFCI)
C-DG3 IF(FHOUR.EQ.0.)
C-DG3&  CALL WRIDIA (FHOUR,FHOUR,IDATE,SL,COLRAB,SLMSK,
C-DG3&               TSEA,SMC,SHELEG,STC,TG3,CANOPY,
C-DG3&               ZORL,GESHEM,BENGSH,DUSFC,DVSFC,DTSFC,DQSFC,
C-DG3&               FLUXR,CVAVG,ILEFT,IRGHT,WGTLON,INSLAT,WGTLAT,
C-DG3&               NDGI)
C...............................................................
C RSM : INITIAL CALL TO REGIONAL SPECTRAL MODEL
C-RSM CALL RSMINI(FHOUR,FILTA,N1,N2,
C-RSM&            NRSMI1,NRSMI2,NRFLIP,
C-RSM&            NRSMO1,NRSMO2,NRFLOP,NRSFLX,NRINIT)
C...............................................................
C...      SMOOTH START
C...............................................................
      DELTIM=CON(1)
      MAXSTP=NUM(7)
      ISTEPS=NUM(8)
      XHOUR=SHOUR
      INISTP=0
      TSTEP0=TIMEF()*0.001
81    FORMAT(1H ,'KDT IN MAIN=',I3)
      DTHR = DELTIM/3600. E 0
      HDTHR = 0.5 * DTHR
      DO 20000 ISTEP=1,ISTEPS
C................................................
C...        TIME LOOP
C................................................
      CALL GSICDF(DELTIM,AM,BM,GV,SV,CM)
      IF(NPOINT.GT.0) THEN
        ISAVE = 1
        ITNUM = 1
        IF(FHOUR.EQ.0.) THEN
          IF(ISTEP.EQ.1) THEN
           IF (IKFREQ.GT.1) ISAVE = 0
           IF (IKFREQ.EQ.1) ITNUM = 2
          END IF
        END IF
      END IF
      DO 10000 JDT=LIMLOW,MAXSTP
      IF(NPOINT.GT.0.AND.ITNUM.GT.NSTKEN) THEN
        PRINT *,'KEN POINTS DISABLED - TIME LEVELS EXCEED ',NSTKEN
        NPOINT=0
      ENDIF
      KDT=JDT
      PRINT 81,KDT
      LASTEP=KDT.EQ.MAXSTP.OR.
     &       (KSOUT.GT.0.AND.MOD(KDT,MAX(KSOUT,1)).EQ.0)
C     CALL RMSGT( Q, DI, TE, ZE,DEL,RQ)
      CALL GLOOPR
      SHOUR=SHOUR+DELTIM
      XHOUR=XHOUR+DELTIM
      CHOUR=SHOUR/3600. E 0
      RHOUR=FHOUR+CHOUR
C     IF(ENDHOUR.GT.0..AND.RHOUR.GE.ENDHOUR) THEN
C       PRINT "(/' FORECAST DONE. RHOUR,ENDHOUR=',2F6.1)",RHOUR,ENDHOUR
C       STOP 'TOOSOON'
C     ENDIF
      IHOUR=CHOUR+0.5 E 0
      CHOUR=IHOUR
      THOUR=FHOUR+CHOUR
      CALL GLOOPA
C...
      CALL SICDIF(DIM,TEM,QM,X,Y,Z,ULN,VLN)
      CALL DELDIF(RT,W,DELTIM,QM,SL,X,Y)
      DO 25500 J=1, 4032
      QM(J)=Q(J)
       Q(J)=Z(J)
25500 CONTINUE
      CALL FILTR1(TEM,TE,Y,DIM,DI,X,ZEM,ZE,W,RM,RQ,RT,FILTA)
C
C...  SET SWITCH FOR SAVING KUO DATA (FOR INTERACTIVE CLOUDS)..
      CVMOD=AMOD(SOLHR+DTHR,DTCVAV)
      IF(CVMOD.LT.HDTHR.OR.CVMOD.GE.DTCVAV-HDTHR) THEN
        CLSTP=MIN(DTCVAV,SHOUR/3600.)
      ELSEIF(CLSTP.GT.0.) THEN
        CLSTP=0.
      ELSE
        CLSTP=-10.
      ENDIF
      CALL GLOOPB
      CALL DAMPUX(X,W,Y,RT,DELTIM,ULN,VLN,SPDMAX)
      CALL FILTR2(TEM,TE,Y,DIM,DI,X,ZEM,ZE,W,RM,RQ,RT,FILTA)
C-WAV IF(DTWAVE.GT.0.) CALL GWAVE(RHOUR)
C
C DFINI : CALL DIGITAL FILTER INITIALIZATION EVERY STEP IF CON(3).GT.0.0
C-DFI IF( CON(3).NE.0.0 ) CALL DFINI(1,CON(3),CHOUR,SOLSEC)
C
C  ADVANCE SOLHR
C
      SOLSEC=SOLSEC+DELTIM
      SOLHR=SOLSEC/3600. E 0
      IDAY=SOLHR/24. E 0
      SOLHR=SOLHR-IDAY*24. E 0
C..... FOR GRID POINT DIAG ADVANCE ITNUM, IF PROPER TIME, AND SET ISAVE
      ISAVE = 0
C-DFI IF(NUMSUM.LT.0) THEN
      IF (IKFREQ.GT.1) THEN
       IMODK = MOD(JDT,IKFREQ)
       IF (IMODK.EQ.0) THEN
        ISAVE = 1
        ITNUM = ITNUM + 1
       END IF
      ELSE
       ISAVE = 1
       ITNUM = ITNUM + 1
      END IF
C-DFI END IF
C
C  CHECK FOR INTERMEDIATE OUTPUT
C
      IF(KSOUT.GT.0.AND.MOD(KDT,MAX(KSOUT,1)).EQ.0.AND.KDT.NE.MAXSTP)
     &  THEN
      KSOUT=0
      XHOUR=0. E 0
C  WRITE INTERMEDIATE ASFC FILE
      CALL FIXIO(THOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     1            CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,1,NFLIP,NFLPS)
C  WRITE INTERMEDIATE SIGMA FILE
      CALL TWRITE(NSIGS,THOUR,IDATE,Z,Q,TE,DI,ZE,RQ,SL,SI,GZ,Z00,N1)
      CALL ROWSEP(GESHEM)
      WRITE(NSIGS)GESHEM
      CALL ROW1NS(GESHEM)
      CLOSE(NSIGS)
      CALL GLOOPZ(NZNLS,NSFCS)
      ENDIF
C................................................
C RSM : CALL MAIN ROUTINE OF REGIONAL FORECAST
C-RSM CALL RSMSMF(FHOUR,SHOUR,GZ,Q,TE,DI,ZE,RQ)
C
10000 CONTINUE
C................................................
C...       TIME LOOP
C................................................
      CALL RMSGT(Q,DI,TE,ZE,DEL,RQ)
      PRINT 102,DELTIM,CHOUR
102   FORMAT(1H0,'STEP=',E10.2,2X,'FCST SEGMENT OF ',E10.2,' H')
      CALL TWRITE(N4,THOUR,IDATE,Z,Q,TE,DI,ZE,RQ,SL,SI,GZ,Z00,N1)
      CALL ROWSEP(GESHEM)
      WRITE(N4)GESHEM
      CALL ROW1NS(GESHEM)
      CLOSE(N4)
      IHOUR=THOUR+0.5 E 0
      MOD12=MOD(IHOUR,12)
      PRINT 107,MOD12
107   FORMAT(1H ,'MOD12=',I2)
      LIMLOW=1
      IF(NPOINT.GT.0) THEN
C...      NOTE, THAT IN SEVERAL SCENERIOS, ITNUM=ITNUM+1 AT THE
C           BOTTOM OF THE 10000 LOOP, SO UNDO IT
       IF (IKFREQ.GT.1) THEN
         IF (IMODK.LE.0) THEN
           ITNUM = ITNUM - 1
         END IF
       ELSE
         ITNUM = ITNUM - 1
       END IF
       PRINT 1047,ITNUM,NPOINT
 1047  FORMAT(1H0,I6,' STEPS OF KEN(CAMPANA) GRIDPT DATA SAVED FOR ',
     1            I5,' POINTS')
C...    NOTE : NOTHING SPECIAL DONE FOR OUTBOARD RADIATION (IBRAD NE 1)
C          IN THIS CASE, TO GET THE RADIATION,CLDS INTO KEN PTS,
C          SOME WORK NEEDS TO BE DONE IN GETRAD....K.A.C.
       DO 730 J=1,NPOINT
        DO 730 K=1,ITNUM
C...      IF OLR.LE ZERO,THEN RADIATION FIELDS HAVE NOT BEEN FILLED
C           FOR THIS TIMESTEP, SO CARRY THE PREVIOUS DATA FORWARD
         IF (SVDATA(44,J,K).LE.0.) THEN
          DO I=25,27
           SVDATA(I,J,K) = SVDATA(I,J,K-1)
          ENDDO
          DO I=41,46
           SVDATA(I,J,K) = SVDATA(I,J,K-1)
          ENDDO
          DO I = 48, 49
             SVDATA(I,J,K) = SVDATA(I,J,K-1)
          ENDDO
          DO I = 51, 58
             SVDATA(I,J,K) = SVDATA(I,J,K-1)
          ENDDO
          SVDATA(38,J,K) = SVDATA(38,J,K-1)
          DO LV=1, 28
           SVDATA(LV+ 80 +8* 28 ,J,K)=
     1             SVDATA(LV+ 80 +8* 28 ,J,K-1)
          ENDDO
         ENDIF
         IF (SVDATA(50,J,K).LE.0.) THEN
          SVDATA(47,J,K) = SVDATA(47,J,K-1)
          SVDATA(50,J,K) = SVDATA(50,J,K-1)
         END IF
  730  CONTINUE
       WRITE(NGPKEN) LAB
       WRITE(NGPKEN) FHOUR,IDATE,SI,SL
       WRITE(NGPKEN) NVRKEN,NPTKEN,NSTKEN,NPOINT,ITNUM
       DO 333 J=1,NPOINT
        WRITE(NGPKEN) ((SVDATA(I,J,K),K=1,ITNUM),I=1,NVRKEN)
  333  CONTINUE
       CLOSE(NGPKEN)
      ENDIF
20000 CONTINUE
       TSTEP=TIMEF()*0.001-TSTEP0
       NSTEPS=ISTEPS*(MAXSTP-LIMLOW+1)
       PRINT*,' TIME, STEPS, TIME PER STEP: ',TSTEP,NSTEPS,TSTEP/NSTEPS
      CALL TWRITE(N3,THOUR,IDATE,Z,QM,TEM,DIM,ZEM,RM,SL,SI,GZ,Z00,N1)
      CALL ROWSEP(GESHEM)
      WRITE(N3)GESHEM
      CALL ROW1NS(GESHEM)
      CLOSE(N3)
C  CREATE SPECIAL DIAGNOSTIC FIELDS
      INISTP = 3
      CALL GLOOPZ(NZNLF,NSFCF)
C... WRITE FIXED FIELDS FOR RADIATION PROG. TO DISK...
      CALL FIXIO(THOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     1            CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,1,NFLIP,NFLOP)
C-DG3   CALL WRIDIA (FHOUR,RHOUR,IDATE,SL,COLRAB,SLMSK,
C-DG31               TSEA,SMC,SHELEG,STC,TG3,CANOPY,
C-DG31               ZORL,GESHEM,BENGSH,DUSFC,DVSFC,DTSFC,DQSFC,
C-DG31               FLUXR,CVAVG,ILEFT,IRGHT,WGTLON,INSLAT,WGTLAT,
C-DG31               NDGF)
C RSM .................. SAVE RSM OUT.................
C-RSM CALL RSMSAV(FHOUR)
C....................................................
CD      CALL W3LOG('$E')
      STOP
      END
