      SUBROUTINE GLOOPA
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GLOOP  COMPUTES DYNAMIC NON-LINEAR TENDENCY TERMS
C OF TEMP. DIV. LN(PS)
C COMPUTES PREDICTED VALUES OF VORTICITY AND MOISTURE
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 88-05-13
C
C ABSTRACT:
C   PROGRAM  STARTS WITH SPECTRAL  COEFFICIENTS TEMP.
C   OF VORTICITY, DIVERGENCE, SPECIFIC HUMIDITY, AND
C   LN((PSFC).  CONVERTS THEM TO THE GAUSSIAN GRID AT EACH
C   LATITUDE AND CALLS FIDI,  FOR THE NORTHERN AND SOUTHERN
C   HEMISPHERES AT THE SAME TIME.  AFTER RETURN FROM FIDI
C   SR.  COMPLETES CALCULATION OF TENDENCIES OF TEMP. DIV. AND LNPS.
C   SPECIFIC HUMIDITY, AND VORTICITY ARE PREDICTED BY SR. SIGVOR
C   ALL INPUT/OUTPUT  IS VIA COMMONS.
C
C PROGRAM HISTORY LOG:
C   91-03-06  JOSEPH SELA
C
C USAGE:    CALL GLOOPA
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
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
      COMMON/COMDHC/SPDLAT( 28 , 47 )
C...
C.................................................................
C SYN(1, 0* 28 +0* 28 +1, LAN)  ZE
C SYN(1, 1* 28 +0* 28 +1, LAN)  DI
C SYN(1, 2* 28 +0* 28 +1, LAN)  TE
C SYN(1, 3* 28 +0* 28 +1, LAN)  RQ
C SYN(1, 3* 28 +1* 28 +1, LAN)  DPDLAM
C SYN(1, 3* 28 +1* 28 +2, LAN)  DPDPHI
C SYN(1, 3* 28 +1* 28 +3, LAN)  ULN
C SYN(1, 4* 28 +1* 28 +3, LAN)  VLN
C.................................................................
C DYN(1, 0* 28 +0* 28 +1, LAN)  D(T)/D(PHI)
C DYN(1, 1* 28 +0* 28 +1, LAN)  D(RQ)/D(PHI)
C DYN(1, 1* 28 +1* 28 +1, LAN)  D(T)/D(LAM)
C DYN(1, 2* 28 +1* 28 +1, LAN)  D(RQ)/D(LAM)
C DYN(1, 2* 28 +2* 28 +1, LAN)  D(U)/D(LAM)
C DYN(1, 3* 28 +2* 28 +1, LAN)  D(V)/D(LAM)
C DYN(1, 4* 28 +2* 28 +1, LAN)  D(U)/D(PHI)
C DYN(1, 5* 28 +2* 28 +1, LAN)  D(V)/D(PHI)
C.................................................................
C ANL(1, 0* 28 +0* 28 +1, LAN)  X     DVDT
C ANL(1, 1* 28 +0* 28 +1, LAN)  Y     DTDT
C ANL(1, 2* 28 +0* 28 +1, LAN)  RT    DRDT
C ANL(1, 2* 28 +1* 28 +1, LAN)  Z     DQDT
C ANL(1, 2* 28 +1* 28 +2, LAN)  W     DUDT
C.................................................................
      PARAMETER(LOTS    =5* 28 +1* 28 +2,LOTST=2* 28 +1,
     &          KSZ     =0* 28 +0* 28 +1,
     &          KSD     =1* 28 +0* 28 +1,
     &          KST     =2* 28 +0* 28 +1,
     &          KSR     =3* 28 +0* 28 +1,
     &          KSPLAM  =3* 28 +1* 28 +1,
     &          KSPPHI  =3* 28 +1* 28 +2,KSTB=3* 28 +1* 28 +2,
     &          KSU     =3* 28 +1* 28 +3,
     &          KSV     =4* 28 +1* 28 +3)
      PARAMETER(LOTD    =6* 28 +2* 28 +0,
     &          KDTPHI  =0* 28 +0* 28 +1,
     &          KDRPHI  =1* 28 +0* 28 +1,
     &          KDTLAM  =1* 28 +1* 28 +1,
     &          KDRLAM  =2* 28 +1* 28 +1,
     &          KDULAM  =2* 28 +2* 28 +1,
     &          KDVLAM  =3* 28 +2* 28 +1,
     &          KDUPHI  =4* 28 +2* 28 +1,
     &          KDVPHI  =5* 28 +2* 28 +1)
      PARAMETER(LOTA    =3* 28 +1* 28 +1,LOTAT=2* 28 ,
     &          KAV     =0* 28 +0* 28 +1,
     &          KAT     =1* 28 +0* 28 +1,
     &          KAR     =2* 28 +0* 28 +1,
     &          KAP     =2* 28 +1* 28 +1,
     &          KAU     =2* 28 +1* 28 +2)
C...
       DIMENSION
     1 QTT( 4032 ,NCPUS),QVV( 4158 ,NCPUS),QDD( 4032 ,NCPUS),
     2 SYN( 386 ,LOTS,NCPUS),SYNTOP(2, 63 ,LOTST),
     3 DYN( 386 ,LOTD,NCPUS),
     4 ANL( 386 ,LOTA,NCPUS),ANLTOP(2, 63 ,LOTAT),
     5 FLP(2, 63 ,LOTA,NCPUS),FLM(2, 63 ,LOTA,NCPUS)
C
C.................................................................
C
      CALL DELLNP(Q,DPDPHI,SYNTOP(1,1,1),DPDLAM)
C
      CALL DZUVLE(DI,ZE,ULN,VLN,SYNTOP(1,1,2),SYNTOP(1,1, 28 +2))
C
      ANLTOP=0.
CMIC$ DO ALL
CMIC$1 SHARED(W,X,Y,RT)
CMIC$1 PRIVATE(J)
      DO K=1, 28
       DO J=1, 4032
         W(J,K)=0. E 0
         X(J,K)=0.0
         Y(J,K)=0. E 0
       ENDDO
      ENDDO
      DO K=1, 28
       DO J=1, 4032
         RT(J,K)=0. E 0
       ENDDO
      ENDDO
C
      DO J=1, 4032
       Z(J)=0. E 0
      ENDDO
C COMPUTE LATITUDE BAND LIMITS
      LAST=MOD( 47 ,NCPUS)
      NGGS=( 47 -LAST)/NCPUS
      IF(LAST.NE.0)NGGS=NGGS+1
      INCLAT=NCPUS
      LAT1=1-NCPUS
      LAT2=0
      LATDON=0
CC
      DO 10000 NGG=1,NGGS
      DYN=0.
      IF((NGG.EQ.NGGS).AND.(LAST.NE.0)) INCLAT=LAST
      LAT1=LAT1+NCPUS
      LAT2=LAT2+INCLAT
CC    LATPRT=2
C
C           LAT LOOP
C
C FIRST LAT LOOP
CMIC$ DO ALL
CMIC$1 SHARED(SYNTOP,SYN,QTT,QVV,LAT1,LAT2,LATDON,COLRAD)
CMIC$1 SHARED(Q,DPDLAM,DPDPHI,ULN,VLN,DI,TE,ZE,RQ)
CMIC$1 PRIVATE(LAT,LAN)
C
      DO 1000 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
      CALL PLN2I(QTT(1,LAN),QVV(1,LAN),COLRAD,LAT)
C
      CALL SUMS2I(ZE,SYN(1,1,LAN),QTT(1,LAN),LOTS)
C
      CALL SUMTOP(SYN(1,KSTB,LAN),SYNTOP,QVV(1,LAN),LOTST, 192 , 192 /2)
1000  CONTINUE
C
C COMPUTE MERID. DERIVS. OF TEMP. AND MOISTURE USING QDD.
C
CMIC$ DO ALL
CMIC$1 SHARED(DYN,QDD,WGT)
CMIC$1 SHARED(RCS2,EPSI)
CMIC$1 SHARED(QTT,QVV,LAT1,LAT2,LATDON)
CMIC$1 SHARED(TE,RQ)
CMIC$1 PRIVATE(LAT,LAN)
C
      DO 1100 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
      CALL GOZRIN(QTT(1,LAN),QVV(1,LAN),QDD(1,LAN),
     1            EPSI,LAT,RCS2,WGT)
C
      CALL SUMS2I(TE,DYN(1,1,LAN),QDD(1,LAN), 28 + 28 )
C
1100  CONTINUE
CMIC$ DO ALL
CMIC$1 SHARED(DYN,RCS2)
CMIC$1 SHARED(SYN,ANL,LAT1,LAT2,LATDON,SPDLAT)
CMIC$1 SHARED(DEL,RDEL2,CI,P1,P2,H1,H2,TOV)
CMIC$1 PRIVATE(LAT,LAN,J,K)
C
      DO 2000 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
C   CALCULATE T RQ U V ZONAL DERIVS. BY MULTIPLICATION WITH I*L
C
      CALL DERIVS(SYN(1,1,LAN),DYN(1,1,LAN),RCS2(LAT))
C
      DO K=1, 28
       DO J=1, 384
        SYN(J,KST-1+K,LAN)=SYN(J,KST-1+K,LAN)-TOV(K)
       ENDDO
      ENDDO
C
      CALL GFIDIU(SYN(1,KSD,LAN),SYN(1,KST,LAN),
     1            SYN(1,KSZ,LAN),SYN(1,KSU,LAN),
     1            SYN(1,KSV,LAN),SYN(1,KSR,LAN),
     1            SYN(1,KSPPHI,LAN),SYN(1,KSPLAM,LAN),
     1 RCS2(LAT),DEL,RDEL2,CI,P1,P2,H1,H2,TOV,SPDLAT(1,LAT),
     1            DYN(1,KDTPHI,LAN),DYN(1,KDTLAM,LAN),
     1            DYN(1,KDRPHI,LAN),DYN(1,KDRLAM,LAN),
     1            DYN(1,KDULAM,LAN),DYN(1,KDVLAM,LAN),
     1            DYN(1,KDUPHI,LAN),DYN(1,KDVPHI,LAN),
     1            ANL(1,KAP,LAN),ANL(1,KAT,LAN),
     1            ANL(1,KAR,LAN),ANL(1,KAU,LAN),
     1            ANL(1,KAV,LAN))
C
      CALL FTI 192 (ANL(1,1,LAN),ANL(1,1,LAN),2*LOTA,-1)
C
C
C AT THIS POINT ARRAYS HOLD TWO LATITUDES  OF FOURIER COEFS
C
2000  CONTINUE
C
C
CMIC$ DO ALL
CMIC$1 SHARED(LATDON,LAT1,LAT2)
CMIC$1 SHARED(ANL,FLP,FLM)
CMIC$1 PRIVATE(LAT,LAN)
C
      DO 2500 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
      CALL FLPFLM(FLP(1,1,1,LAN),FLM(1,1,1,LAN),
     1            ANL(1,1,LAN))
2500  CONTINUE
C
C
      DO 3000 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
C
      CALL FL2I(FLP(1,1,1,LAN),FLM(1,1,1,LAN),X,QTT(1,LAN),LOTA)
C
      CALL UVSUMS(FLP(1,1,KAU,LAN),FLM(1,1,KAU,LAN),
     1            FLP(1,1,KAV,LAN),FLM(1,1,KAV,LAN),
     2            ANLTOP(1,1,1),ANLTOP(1,1, 28 +1),
     3            QVV(1,LAN), 28 ,WGT(LAT))
C
C     PRINT 4324, LAN, LAT, LATDON
C4324 FORMAT(/ 1H ,'RMS TENDEN GLOOPAV INSIDE LOOP 3000        ',
C    1'COL2=U COL4=V LAN=',I3, '  LAT=',I3, '  LATDON=',I3 )
C     CALL RMSGT(Z ,X  ,Y  ,W  ,DEL,RT )
C
C
3000  CONTINUE
      LATDON=LATDON+(LAT2-LAT1+1)
C......................................................
C......................................................
10000 CONTINUE
C......................................................
C
CMIC$ DO ALL
CMIC$1 SHARED(SPDMAX,SPDLAT)
CMIC$1 PRIVATE(K,LAT)
      DO K=1, 28
       SPDMAX(K) = 0.0
       DO LAT=1, 47
        SPDMAX(K)=MAX(SPDMAX(K),SPDLAT(K,LAT))
       ENDDO
       SPDMAX(K)=SQRT(SPDMAX(K))
      ENDDO
C......................................................
C
C
C     INPUT : W=D(U)/D(T) X=D(V)/D(T)
C     OUTPUT: ULN=D(DI)/D(T) VLN=D(ZE)/D(T)
C
      CALL UVTODZ(W  ,X  ,ULN,VLN,ANLTOP(1,1,1),ANLTOP(1,1, 28 +1))
C.....................................................
C  SUBTRACT OFF LINEAR DEPENDENCE ON DIVERGENCE
      DO 850 K=1, 28
      DO 840 J=1, 28
      DO 830 I=1, 4032
              Y(I,K)=Y(I,K)-BM(K,J)*DI(I,J)
830         CONTINUE
840   CONTINUE
850   CONTINUE
C MOVE DIV TENDENCY INTO X AND ADD TOPOG. CONTRIB.
C INTEGRATE VORTICITY AMD MOISTURE IN TIME
C REMEMBER ULN IS OLD X
C REMEMBER VLN IS OLD W
C
      DO K=1, 28
       DO I=1, 4032
         X(I,K)=ULN(I,K)+GZ(I)
         W(I,K)=ZEM(I,K)+2.*DELTIM*VLN(I,K)
       ENDDO
      ENDDO
      DO K=1, 28
       DO I=1, 4032
        RT(I,K)= RM(I,K)+2.*DELTIM* RT(I,K)
       ENDDO
      ENDDO
      DO K=1, 28
       W(1,K)=0. E 0
       W(2,K)=0. E 0
      ENDDO
C
      PRINT 100,(SPDMAX(K),K=1, 28 )
100   FORMAT(' SPDMX(01:10)=',10F5.0,:/' SPDMX(11:20)=',10F5.0,
     &     :/' SPDMX(21:30)=',10F5.0,:/' SPDMX(31:40)=',10F5.0,
     &     :/' SPDMX(41:50)=',10F5.0,:/' SPDMX(51:60)=',10F5.0,
     &     :/' SPDMX(61:70)=',10F5.0,:/' SPDMX(71:80)=',10F5.0,
     &     :/' SPDMX(81:90)=',10F5.0,:/' SPDMX(91:00)=',10F5.0)
C
      RETURN
      END
