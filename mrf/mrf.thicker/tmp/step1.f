      SUBROUTINE STEP1(N1,N2,ITREAD,INI,NANL,SOLSEC)
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
      DIMENSION IDSAVE(4)
      PRINT 9876,N1,N2,ITREAD,FHOUR
9876  FORMAT(1H ,'N1,N2,ITREAD,FHOUR IN STEP1',3(I4,1X),F6.2)
      IF(ITREAD.EQ.0)GO TO 2000
      CALL TREAD(N1,FHOUR,IDATE,GZ,QM,TEM,DIM,ZEM,RM,SL,SI,Z00)
      REWIND N1
      PRINT 9877,N1,ITREAD,FHOUR
9877  FORMAT(1H ,'N1,ITREAD,FHOUR AFTER TREAD',2(I4,1X),F6.2)
      CALL RMSGT(QM,DIM,TEM,ZEM,DEL,RM)
      CALL TREAD(N2,FHOUR,IDATE,GZ,Q,TE,DI,ZE,RQ,SL,SI,Z00)
      REWIND N2
      PRINT 9878,N2,ITREAD,FHOUR
9878  FORMAT(1H ,'N2,ITREAD,FHOUR AFTER TREAD',2(I4,1X),F6.2)
      CALL RMSGT(Q,DI,TE,ZE,DEL,RQ)
C     SET INITIAL SOLHR
      SOLHR=FHOUR+IDATE(1)
      IDAY=SOLHR/24. E 0
      SOLHR=SOLHR-IDAY*24. E 0
      SOLSEC=SOLHR*3600.
2000  CONTINUE
C............................................................
C............................................................
      DO 22000 L=1, 47
      DO 22000 J=1, 384
      GESHEM(J,L)=0. E 0
      TMPMAX(J,L) = 0.
      TMPMIN(J,L) = 1.E10
22000 CONTINUE
C..
C....READ FIXED FIELDS FROM FIXFLD PROG............
C..
      CALL FIXIO(FHOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     &           CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,0,NFLIP,NFLOP)
C ..............................................................
      CALL ZERFLX(DUSFC,DVSFC,DTSFC,DQSFC,DLWSFC,ULWSFC,
     1 BENGSH,GFLUX,RUNOFF,EP,CLDWRK,
     2 DUGWD,DVGWD,PSMEAN)
C ..............................................................
C...  FIRST STEP IS FORWARD. THEN 2 LEAPFROGS, DOUBLING DELTIM.
C ..............................................................
      SHOUR=0.
      IF(FHOUR.NE.0.0.AND.INI.EQ.0) RETURN
      NFSTEP=2
      DELTIM=CON(1)/2. E 0**2
      INISTP=1
      ISAVE=1
      IF(ITREAD.EQ.1) THEN
        IF(N1.EQ.N2 .AND. N2.NE.NANL) THEN
          SAVFHR=FHOUR
          IDSAVE=IDATE
          Z=Q
          W=TE
          X=DI
          Y=ZE
          RT=RQ
          REWIND NANL
          CALL TREAD(NANL,FHOUR,IDATE,GZ,Q,TE,DI,ZE,RQ,SL,SI,Z00)
          REWIND NANL
        ENDIF
        CALL GLOOPR
        IF(N1.EQ.N2 .AND. N2.NE.NANL) THEN
          FHOUR=SAVFHR
          IDATE=IDSAVE
          Q=Z
          TE=W
          DI=X
          ZE=Y
          RQ=RT
        ENDIF
      ENDIF
      SHOUR=SHOUR+DELTIM
      DO 5000 JDT=1,2
      IF (JDT.GT.1) INISTP=0
      IF (JDT.GT.1) ISAVE = 0
      KDT=JDT
      PRINT 102,KDT
102   FORMAT(1H ,'KDT IN FIRST STEP=',I6)
      LASTEP=KDT.EQ.2
      SHOUR=SHOUR+DELTIM
      CALL GSICDF(DELTIM,AM,BM,GV,SV,CM)
      CALL GLOOPA
      CALL RMSGT(Z ,X  ,Y  ,W  ,DEL,RT)
      CALL SICDIF(DIM,TEM,QM,X,Y,Z,ULN,VLN)
      CALL DELDIF(RT,W,DELTIM,QM,SL,X,Y)
      DO 3 J=1, 4032
      Q(J)=Z(J)
3     CONTINUE
      CALL GLOOPB
      CALL DAMPUX(X,W,Y,RT,DELTIM,ULN,VLN,SPDMAX)
      DO 5 K=1, 28
      DO 4 J=1, 4032
      DI(J,K)=X(J,K)
      ZE(J,K)=W(J,K)
      TE(J,K)=Y(J,K)
4     CONTINUE
5     CONTINUE
      DO 7 K=1, 28
      DO 8 J=1, 4032
      RQ(J,K)=RT(J,K)
8     CONTINUE
7     CONTINUE
      DELTIM=DELTIM*2. E 0
5000  CONTINUE
      SOLSEC=SOLSEC+DELTIM
      SOLHR=SOLSEC/3600. E 0
C...............................................................
C...  FIN SMOOTH START
C...............................................................
      DELTIM=CON(1)
      LIMLOW=2
      RETURN
      END
