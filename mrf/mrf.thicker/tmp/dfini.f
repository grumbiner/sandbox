       SUBROUTINE DFINI(ICALL,HRINI,CHOUR,SOLSEC)
C.................................................................
C.....
C
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
C.....
      COMMON/INIGSM/   DTHOUR , DSHOUR, DCHOUR, DSOLSEC, TOTSUM
     2                        ,QS( 4032 )
     2  ,TES( 4032 , 28 ),RQS( 4032 , 28 )
     2  ,DIS( 4032 , 28 ),ZES( 4032 , 28 )
C
      IF(NUMSUM.GE.NUMMAX) RETURN
C
      IF( ICALL.EQ.0 ) THEN
      PRINT *,' INITIAL DFINI '
      PRINT *,' INI TIME IS ',HRINI,' HOUR.'
      DO 10 K=1, 28
      DO 10 I=1, 4032
      DIS(I,K) = 0.0
      ZES(I,K) = 0.0
      TES(I,K) = 0.0
  10  CONTINUE
      DO 11 K=1, 28
      DO 11 I=1, 4032
      RQS(I,K) = 0.0
  11  CONTINUE
      DO 12 I=1, 4032
      QS(I) = 0.0
  12  CONTINUE
      TOTSUM=0.0
      ENDIF
C
      NUMSUM=NUMSUM+1
      PRINT *,' ---- IN DFINI ---- NUMSUM NUMMAX ',NUMSUM,NUMMAX
      IF( NUMSUM.NE.0 ) THEN
        SC =  3.141593E+0  / NUMMAX
        SX= NUMSUM*SC
        TX= NUMSUM* 3.141593E+0
        WX= TX/ ( NUMMAX+1 )
        DIGFIL= SIN(WX)/WX * SIN(SX)/TX
      ELSE
        DIGFIL = 1.0/NUMMAX
      ENDIF
      TOTSUM = TOTSUM + DIGFIL
C
C
C------------------------DO SUMMATION WITH WINDOW---
C
C FIRST LAT LOOP
CMIC$ DO ALL
CMIC$1 SHARED(DI ,ZE ,TE ,RQ , Q )
CMIC$1 SHARED(DIS,ZES,TES,RQS, QS)
CMIC$1 SHARED(DIGFIL)
CMIC$1 PRIVATE(J)
C      AUTOSCOPE
C
C .......OBTAIN FULL FIELD VALUES
      DO 110 K=1, 28
      DO 110 J=1, 4032
      DIS(J,K) = DIS(J,K) + DIGFIL*DI(J,K)
      ZES(J,K) = ZES(J,K) + DIGFIL*ZE(J,K)
      TES(J,K) = TES(J,K) + DIGFIL*TE(J,K)
 110  CONTINUE
      DO 111 K=1, 28
      DO 111 J=1, 4032
      RQS(J,K) = RQS(J,K) + DIGFIL*RQ(J,K)
 111  CONTINUE
      DO 120 J=1, 4032
      QS(J) = QS(J) + DIGFIL*Q(J)
 120  CONTINUE
C................................................
C SAVE
      IF( NUMSUM.EQ.0 ) THEN
        DTHOUR=THOUR
        DSHOUR=SHOUR
        DCHOUR=CHOUR
        DSOLSEC=SOLSEC
        PRINT *,' NUMSUM=0, SAVE THOUR= ',DTHOUR
        CALL FIXIO(THOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     1            CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,1,NFLIP,NFLOP)
      ENDIF
C................................................
C RESTORE
      IF( NUMSUM.EQ.NUMMAX ) THEN
        PRINT *,' NUMSUM=NUMMAX REASSIGN PERTURBATION '
        PRINT *,' WITH NORMALIZED FACTOR=',TOTSUM,' AT HOUR=',DTHOUR
        HRINI=0
        THOUR=DTHOUR
        SHOUR=DSHOUR
        CHOUR=DCHOUR
        SOLSEC=DSOLSEC
        CALL FIXIO(THOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     1            CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,0,NFLOP,0)
CMIC$ DO ALL
CMIC$1 SHARED(DI ,ZE ,TE ,RQ , Q )
CMIC$1 SHARED(DIS,ZES,TES,RQS, QS)
CMIC$1 SHARED(DIM,ZEM,TEM, RM, QM)
CMIC$1 SHARED(TOTSUM)
CMIC$1 PRIVATE(J)
C      AUTOSCOPE
        DO 210 K=1, 28
        DO 210 J=1, 4032
        DI (J,K) = DIS(J,K) / TOTSUM
        ZE (J,K) = ZES(J,K) / TOTSUM
        TE (J,K) = TES(J,K) / TOTSUM
        DIM(J,K) = DIS(J,K) / TOTSUM
        ZEM(J,K) = ZES(J,K) / TOTSUM
        TEM(J,K) = TES(J,K) / TOTSUM
 210    CONTINUE
        DO 211 K=1, 28
        DO 211 J=1, 4032
        RQ (J,K) = RQS(J,K) / TOTSUM
        RM (J,K) = RQS(J,K) / TOTSUM
 211    CONTINUE
        DO 220 J=1, 4032
        QM(J) = QS(J) / TOTSUM
        Q (J) = QS(J) / TOTSUM
 220    CONTINUE
      DO 230 L=1, 47
      DO 230 J=1, 384
      GESHEM(J,L)=0.5*GESHEM(J,L)
230   CONTINUE
      CALL MLTFLX(0.5,DUSFC,DVSFC,DTSFC,DQSFC,DLWSFC,ULWSFC,
     1 BENGSH,GFLUX,
     2 DUGWD,DVGWD,PSMEAN)
C-DG3 CALL MLTDIA(0.5)
      CALL ZNLMLT(0.5)
      FLUXR=0.5*FLUXR
      CVAVG=0.5*CVAVG
      ENDIF
C................................................
C
      RETURN
      END
