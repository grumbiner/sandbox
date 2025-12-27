      SUBROUTINE GLOOPB
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
C  DIAGNOSTIC INDEXES AND FLAGS
C-DG3 PARAMETER(KDGDA=13)
C-DG3 PARAMETER(KDTLARG=1,KDTCONV=2,KDQCONV=3,KDTSHAL=4,KDQSHAL=5,
C-DG3&          KDTVRDF=6,KDUVRDF=7,KDVVRDF=8,KDQVRDF=9,
C-DG3&          KDTHSW=10,KDTHLW=11,KDTCLD=12,KDTCCV=13)
C-DG3 CHARACTER*8 CNMGDA
C-DG3 COMMON /COMGDC/ CNMGDA(KDGDA)
C-DG3 PARAMETER(NTGDA=92)
C-DG3 PARAMETER(NWGDA=(( 384 * 28 -1)/512+1)*512)
C-DG3 PARAMETER(NRGDA= 47 )
C-DG3 COMMON /COMGDA/ IPUGDA(KDGDA),IBMGDA(KDGDA)
C-DGM COMMON /COMGDD/ GDD(NWGDA*KDGDA*NRGDA)
C-DG3 DIMENSION GDA(NWGDA,KDGDA,NCPUS)
C.................................................................
C SYN(1, 0* 28 +0* 28 +1, LAN)  DPDLAM
C SYN(1, 0* 28 +0* 28 +2, LAN)  DPDPHI
C SYN(1, 0* 28 +0* 28 +3, LAN)  ULN
C SYN(1, 1* 28 +0* 28 +3, LAN)  VLN
C SYN(1, 2* 28 +0* 28 +3, LAN)  Q
C SYN(1, 2* 28 +0* 28 +4, LAN)  X
C SYN(1, 3* 28 +0* 28 +4, LAN)  Y
C SYN(1, 4* 28 +0* 28 +4, LAN)  RT
C.................................................................
C ANL(1, 0* 28 +0* 28 +1, LAN)  ZE    DUDT
C ANL(1, 1* 28 +0* 28 +1, LAN)  DI    DVDT
C ANL(1, 2* 28 +0* 28 +1, LAN)  TE    DTDT
C ANL(1, 3* 28 +0* 28 +1, LAN)  RQ    DRDT
C.................................................................
      PARAMETER(LOTS    =4* 28 +1* 28 +3,LOTST=2* 28 +1,
     &          KSPLAM  =0* 28 +0* 28 +1,
     &          KSPPHI  =0* 28 +0* 28 +2,KSTB=0* 28 +0* 28 +2,
     &          KSU     =0* 28 +0* 28 +3,
     &          KSV     =1* 28 +0* 28 +3,
     &          KSP     =2* 28 +0* 28 +3,
     &          KSD     =2* 28 +0* 28 +4,
     &          KST     =3* 28 +0* 28 +4,
     &          KSR     =4* 28 +0* 28 +4)
      PARAMETER(LOTA    =3* 28 +1* 28 +0,LOTAT=2* 28 ,
     &          KAU     =0* 28 +0* 28 +1,
     &          KAV     =1* 28 +0* 28 +1,
     &          KAT     =2* 28 +0* 28 +1,
     &          KAR     =3* 28 +0* 28 +1)
C...
      DIMENSION
     2 QTT( 4032 ,NCPUS),QVV( 4158 ,NCPUS),
     X FPL(2, 63 ,LOTA,NCPUS),FML(2, 63 ,LOTA,NCPUS),
     1 SYN( 386 ,LOTS,NCPUS),ANL( 386 ,LOTA,NCPUS)
      DIMENSION SYNTOP(2, 63 ,LOTST)
      DIMENSION ANLTOP(2, 63 ,LOTAT)
      DIMENSION TGMXL(NCPUS),IGMXL(NCPUS),KGMXL(NCPUS)
      DIMENSION TGMNL(NCPUS),IGMNL(NCPUS),KGMNL(NCPUS)
C-RAS LOGICAL RAS
C-RAS PARAMETER (RAS=.TRUE.)
C-RAS PARAMETER (LMX= 28 )
C-RAS PARAMETER (CP= 1.0046E+3 , ALHL= 2.5000E+6 , GRAV= 9.8000E+0 , RGA
C    1S= 2.8705E+2 )
C-RAS DIMENSION SIG(LMX+1), PRJ(LMX+1), PRH(LMX),   FPK(LMX), HPK(LMX)
C-RAS*,         SGB(LMX),   ODS(LMX),   RASAL(LMX), PRNS(LMX/2)
C-RAS*,         RANNUM(LMX)
C.................................................................
      LOGICAL LADJ
      PARAMETER(LADJ=.TRUE.)
C.................................................................
C-RAS IF (RAS) CALL SETRAS(LMX, SI, SL, DEL, CP, RGAS, DELTIM
C-RAS*,                    SIG, SGB, PRH, PRJ, HPK, FPK, ODS, PRNS
C-RAS*,                    RASAL, LM, KRMIN, KRMAX, NSTRP
C-RAS*,                    NCRND, RANNUM, AFAC, UFAC)
C
      CALL DELLNP(Q,DPDPHI,SYNTOP,DPDLAM)
      CALL DZUVLE(X,W,ULN,VLN,SYNTOP(1,1,2),SYNTOP(1,1, 28 +2))
C
      DO  33 K=1, 28
      DO 220 J=1, 4032
      ZE(J,K)=0. E 0
      DI(J,K)=0. E 0
      TE(J,K)=0. E 0
  220 CONTINUE
   33 CONTINUE
      DO 221 K=1, 28
      DO 221 J=1, 4032
      RQ(J,K)=0. E 0
  221 CONTINUE
      ANLTOP=0.
      TGMX=-1.E20
      TGMN= 1.E20
C
C COMPUTE LATITUDE BAND LIMITS
      LAST=MOD( 47 ,NCPUS)
      NGGS=( 47 -LAST)/NCPUS
      IF(LAST.NE.0)NGGS=NGGS+1
      INCLAT=NCPUS
      LAT1=1-NCPUS
      LAT2=0
      LATDON=0
      DO 10000 NGG=1,NGGS
      IF((NGG.EQ.NGGS).AND.(LAST.NE.0)) INCLAT=LAST
      LAT1=LAT1+NCPUS
      LAT2=LAT2+INCLAT
C-DG3 DO 200 LAT=LAT1,LAT2
C-DG3 LAN=LAT-LATDON
C-DG3 CALL GETDIA(LAT,NWGDA*KDGDA,GDA(1,1,LAN))
200   CONTINUE
C-DG3 CALL SYNDIA
      TGMXL=TGMX
      TGMNL=TGMN
C   FIRST  LAT LOOP
CMIC$ DO ALL
CMIC$1 SHARED(SYN,ANL,QTT,QVV,LAT1,LAT2,LATDON)
CMIC$1 SHARED(COLRAB,DPDLAM,SYNTOP)
CMIC$1 SHARED(TGMXL,IGMXL,KGMXL,TGMNL,IGMNL,KGMNL,GDA)
CMIC$1 SHARED(RAS,LMX,CP,ALHL,GRAV,RGAS)
CMIC$1 SHARED(SIG, SGB, PRH, PRJ, HPK, FPK, ODS, PRNS)
CMIC$1 SHARED(RASAL, LM, KRMIN, KRMAX, NSTRP)
CMIC$1 SHARED(NCRND, RANNUM, AFAC, UFAC)
CMIC$1 PRIVATE(DUMMY,LAT,LAN)
CMIC$1 AUTOSCOPE
C
C           LAT LOOP
C
      DO 1000 LAT =LAT1,LAT2
      LAN=LAT-LATDON
C
C.... SINLAB= COS(COLRAB(LAT))
C
      CALL PLN2I(QTT(1,LAN),QVV(1,LAN),COLRAB,LAT)
C
      CALL SUMS2I(DPDLAM,SYN(1,1,LAN),QTT(1,LAN),LOTS)
C
      CALL SUMTOP(SYN(1,KSTB,LAN),SYNTOP,QVV(1,LAN),LOTST, 192 , 192 /2)
C
      CALL FTI 192 (SYN(1,1,LAN),DUMMY,2*LOTS,1)
C
C
       CALL GBPHYS(
     X SYN(1,KSPLAM,LAN),SYN(1,KSPPHI,LAN),
     X SYN(1,KSU,LAN),SYN(1,KSV,LAN),SYN(1,KSP,LAN),
     X SYN(1,KST,LAN),SYN(1,KSR,LAN),SYN(1,KSD,LAN),
     X ANL(1,KAT,LAN),ANL(1,KAR,LAN),ANL(1,KAU,LAN),ANL(1,KAV,LAN),
     X TGMXL(LAN),IGMXL(LAN),KGMXL(LAN),
     X TGMNL(LAN),IGMNL(LAN),KGMNL(LAN),
C-DG3X GDA(1,1,LAN),
C-RASX RAS,LMX,CP,ALHL,GRAV,RGAS,
C-RASX SIG, SGB, PRH, PRJ, HPK, FPK, ODS, PRNS,
C-RASX RASAL, LM, KRMIN, KRMAX, NSTRP,
C-RASX NCRND, RANNUM, AFAC, UFAC,
     X LAT)
C
C
      CALL FTI 192 (ANL(1,1,LAN),ANL(1,1,LAN),2*LOTA,-1)
C
1000  CONTINUE
C-DG3 DO 2200 LAT=LAT1,LAT2
C-DG3 LAN=LAT-LATDON
C-DG3 CALL PUTDIA(LAT,NWGDA*KDGDA,GDA(1,1,LAN))
2200  CONTINUE
C
CMIC$ DO ALL
CMIC$1 SHARED(QTT,QVV,WGB,LATDON,LAT1,LAT2)
CMIC$1 SHARED(ANL,FPL,FML,RCS2)
CMIC$1 PRIVATE(LAT,LAN,I,J,K)
C
      DO 2500 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
      DO 714 I=1, 4032
      QTT(I,LAN)=QTT(I,LAN)*WGB(LAT)
714   CONTINUE
C
      DO K=1, 28
       DO J=1, 384
         ANL(J,KAU-1+K,LAN)=ANL(J,KAU-1+K,LAN)*RCS2(LAT)
         ANL(J,KAV-1+K,LAN)=ANL(J,KAV-1+K,LAN)*RCS2(LAT)
       ENDDO
      ENDDO
C
      CALL FBPFBM(FPL(1,1,1,LAN),FML(1,1,1,LAN),ANL(1,1,LAN))
C
2500  CONTINUE
C
C  LATITUDE LOOP
      DO 3000 LAT=LAT1,LAT2
      LAN=LAT-LATDON
C
      CALL FL2I(FPL(1,1,1,LAN),FML(1,1,1,LAN),ZE,QTT(1,LAN),LOTA)
C
C  ZE=U    CORRECTION
C  DI=V    CORRECTION
C  TE=T    CORRECTION
C  RQ=R    CORRECTION
C
C NEW CALL: NOTE QVV IS NOT MULTIPLIED BY WGT YET
C
      CALL UVSUMS(FPL(1,1,KAU,LAN),FML(1,1,KAU,LAN),
     1            FPL(1,1,KAV,LAN),FML(1,1,KAV,LAN),
     2            ANLTOP(1,1,1),ANLTOP(1,1, 28 +1),
     3            QVV(1,LAN), 28 ,WGT(LAT))
C
      IF(TGMXL(LAN).GT.TGMX) THEN
        TGMX=TGMXL(LAN)
        IGMX=IGMXL(LAN)
        KGMX=KGMXL(LAN)
        JGMX=LAT
      ELSE IF(TGMNL(LAN).LT.TGMN) THEN
        TGMN=TGMNL(LAN)
        IGMN=IGMNL(LAN)
        KGMN=KGMNL(LAN)
        JGMN=LAT
      ENDIF
3000  CONTINUE
      LATDON=LATDON+(LAT2-LAT1+1)
10000 CONTINUE
C
      CALL UVTODZ(ZE,DI,ULN,VLN,ANLTOP(1,1,1),ANLTOP(1,1, 28 +1))
C
      IF(LADJ) THEN
      DO 531 J=1, 4032
      Z(J)=0.0
531   CONTINUE
      ENDIF
CC
CMIC$ DO ALL
CMIC$1 SHARED(Z,Q,DEL,RQ,RT,DI,X,ZE,W,TE,Y,ULN,VLN)
CMIC$1 PRIVATE(J,K)
      DO  18 K=1, 28
      DO 530 J=1, 4032
      IF(LADJ) THEN
C  INCLUDE MOISTURE CHANGE IN PRESSURE TENDENCY
CHUUG Z(J)=Z(J)+(RQ(J,K)-RT(J,K))*DEL(K)
      DI(J,K)=ULN(J,K)-X(J,K)
      TE(J,K)=TE(J,K)-Y(J,K)
      ELSE
C  INCLUDE MOISTURE CHANGE IN PRESSURE TENDENCY
CHUUG Q(J)=Q(J)+(RQ(J,K)-RT(J,K))*DEL(K)
      X(J,K)=ULN(J,K)
      Y(J,K)=TE(J,K)
      ENDIF
      W(J,K)=VLN(J,K)
  530 CONTINUE
   18 CONTINUE
      DO 532 K=1, 28
      DO 532 J=1, 4032
      RT(J,K)=RQ(J,K)
  532 CONTINUE
      IF(LADJ) THEN
      CALL IMPADJ(X,Y,Q,DI,TE,Z,ULN,VLN)
      ENDIF
      PRINT '(" GLOOPB T RANGE ",2(4X,F6.1," @I,K,LAT ",3I4))',
     & TGMX,IGMX,KGMX,JGMX,TGMN,IGMN,KGMN,JGMN
C-DG3 CALL SYNDIA
      RETURN
      END
