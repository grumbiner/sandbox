      SUBROUTINE CNSTS
C     ******************************************************************
C     *                                                                *
C     *  ROUTINE FOR INITIALIZATION OF CONSTANTS AND VARIABLES         *
C     *                                                                *
C     ******************************************************************
                             P A R A M E T E R
     & (D00=0.0,D5=5.E-1,D01=1.00E-2,H1=1.0,HM1=-1.0
     &, H90=90.0,H360=360.0,EPS=1.E-10)
C----------------------------------------------------------------------
      INCLUDE "parmeta.res"
      INCLUDE "parmsoil"
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (ITB=076,JTB=134,ITBQ=152,JTBQ=440)
                             P A R A M E T E R
     & (IM2=IM-2,JM1=JM-1,JM2=JM-2,JM3=JM-3,JM4=JM-4,JM5=JM-5
     &, IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, KHN=IM-1,KHS=-IM
     &, KNE=IM    ,KNW=IM-1 ,KSW=-IM    ,KSE=-IM+1
     &, KHL00=1                    ,KHH00=IM*JM-JM/2
     &, LP1=LM+1)
c    &, LP1=LM+1,nglats=190,nglons=384)
C-----------------------------------------------------------------------
                             L O G I C A L 
     & SIGMA
                             L O G I C A L * 1
     & NUSNOSST, LUSAF
C-----------------------------------------------------------------------
                             C O M M O N  /PTETA/
     & IDAT(3),PT ,DETA  (LM),AETA  (LM),ETA   (LP1),DFL   (LP1)
C
     &,RES   (IM,JM),FIS   (IM,JM),ALBEDO(IM,JM)
     &,SNO   (IM,JM),SST   (IM,JM),SI    (IM,JM)
     &,SM    (IM,JM),LMH   (IM,JM),LMV   (IM,JM)
     &,SMC(IM,JM,NSOIL),STC(IM,JM,NSOIL),CMC(IM,JM),SH2O(IM,JM,NSOIL)
C-----------------------------------------------------------------------
                             C O M M O N  /SFCTYPES/
     & IVGTPK(IM,JM),ISLTPK(IM,JM),ISPTPK(IM,JM)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & KHL0  (JM),KHH0  (JM),KVL0  (JM),KVH0  (JM)
     &,KHL1  (JM),KHH1  (JM),KVL1  (JM),KVH1  (JM)
     &,KHL3  (JM),KHH3  (JM),KVL3  (JM),KVH3  (JM)
     &,KHL4  (JM),KHH4  (JM),KVL4  (JM),KVH4  (JM)
     &,KHL5  (JM),KHH5  (JM),KVL5  (JM),KVH5  (JM)
     &,KHL6  (JM),KHH6  (JM),KVL6  (JM),KVH6  (JM)
     &,DXJ   (JM),WPDARJ(JM),CPGFUJ(JM),CURVJ (JM),FCPJ  (JM)
     &,FDIVJ (JM),EMJ   (JM),EMTJ  (JM),FADJ  (JM)
     &,DDMPUJ(JM),DDMPVJ(JM),HDACJ (JM)
C
     &,QSOLD (JTB),POLD  (JTB),QSNEW (JTB),PNEW  (JTB)
     &,Y2P   (JTB),APP   (JTB),AQP   (JTB)
     &,THEOLD(JTB),TOLD  (JTB),THENEW(JTB),TNEW  (JTB)
     &,Y2T   (JTB),APT   (JTB),AQT   (JTB),IGDATE(4)
c    &,Y2T   (JTB),APT   (JTB),AQT   (JTB),GGRID1(NGLONS,NGLATS)
c    &, GLATS(NGLATS), GGSMC2(NGLONS,NGLATS,2),IGDATE(4)
c    &, GGSTC2(NGLONS,NGLATS,2),GGTG(NGLONS,NGLATS),GGSLI(NGLONS,NGLATS)
c    &, SLAT(NGLATS),WLAT(NGLATS)
C
       CHARACTER*32 GLABEL
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & KHLA  (JAM),KHHA  (JAM),KVLA  (JAM),KVHA  (JAM)
     &,KHL2  (JM),KHH2  (JM),KVL2  (JM),KVH2  (JM)
C-----------------------------------------------------------------------
                             C O M M O N /MASKS/
     & HBM2  (IM,JM),VBM2  (IM,JM),VBM3  (IM,JM),SICE (IM,JM)
C
     &,HTM   (IM,JM,LM),VTM   (IM,JM,LM)
C-----------------------------------------------------------------------
       COMMON /LATLONS/
     & HLAT(IM,JM),HLON(IM,JM),VLAT(IM,JM),VLON(IM,JM)
C-----------------------------------------------------------------------
       COMMON /IMMSK/ MSKSCVH (1024,1024)
       COMMON /AFMSK/ MSKAF   (512,512)
C
       COMMON /OLDALB/ ALBASE(IM,JM)
C
                             D I M E N S I O N
     & RDETA (LM),F4Q2  (LM)
     &,EM    (JAM),EMT   (JAM)
C
     &,DX   (IM,JM),WPDAR(IM,JM),CPGFU(IM,JM),CURV (IM,JM),FCP  (IM,JM)
     &,FDIV (IM,JM),FAD  (IM,JM),F    (IM,JM),DDMPU(IM,JM),DDMPV(IM,JM)
     &,GLAT (IM,JM),GLON (IM,JM)
     &,ALBC1(361,180),ALBC2 (361,180),ALBC3(361,180),ALBC4(361,180)
     &,ALBC(361,180),VGFRCK(IM,JM)
     &,SNALMX(361,180),MXSNAL(IM,JM)
C    &,KPDS(25), KGDS(22), JPDS(25), JGDS(22)
C
C     LOGICAL*1 BITMAP(2500,1250)
C
      COMMON /SSTUFF/ I1D(360,180)
      COMMON /VEGGIE/ FPAR1(2500,1250),FPAR2(2500,1250)
      COMMON /TGRND/ TG(IM,JM)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & DETA2(LM),AETA2(LM),ETA2(LP1),DFRLG(LP1)
     &,QS0   (JTB),SQS   (JTB),THE0  (ITB),STHE  (ITB)
     &,                        THE0Q(ITBQ),STHEQ(ITBQ)
C
     &,EPSR  (IM,JM),GFFC   (IM,JM)
     &,HDAC  (IM,JM),HDACV (IM,JM)
C
     &,PTBL  (ITB,JTB),TTBL  (JTB,ITB),TTBLQ (JTBQ,ITBQ)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & DETA1 (LM),AETA1 (LM),ETA1  (LP1), JULM(13)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & SLDPTH(NSOIL),RTDPTH(NSOIL)
C
c      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::GLATS
c      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::GGTG,GGSLI,GGRID1
c      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::GGSMC2,GGSTC2
C
C--------------HORIZONTAL GRID CONSTANTS--------------------------------
                             N A M E L I S T /ECONDATA/
     & TLM0D,TPH0D,WBD,SBD,DLMD,DPHD
     &,DT,W,IDTAD,IDTCF,COAC,CODAMP,TDDAMP,DFC,DDFC,SIGMA,NUSNOSST,LUSAF
     &,SLDPTH,RTDPTH
C--------------STEREOGRAPHIC MAP CONSTANTS------------------------------
                             D A T A
     & JULM/0,31,59,90,120,151,181,212,243,273,304,334,365/
                             D A T A
     & CMLD/-80.00/,DP30/022.86/,X1P/108./,Y1P/120./,IXM/145/,IYM/130/
C--------------UNIVERSAL CONSTANTS--------------------------------------
                             D A T A
     & A/6376000./,G/9.800/,TWOM/.00014584/,R/287.04/,CP/1004.6/
     &,PI/3.141592654/
C--------------SURFACE DATA---------------------------------------------
                             D A T A
     & ROS/1500./,CS/1339.2/,DS/.100/,AKS/.0000005  /,DZG/2.5000/
     &,TG0/258.16/,TGA/30./
     &,ROI/916.6/,CI/2060.0/,DI/.100/,AKI/.000001075/,DZI/2.0/
     &,TI0/271.16/,THL/210./,PLQ/70000./
C--------------LOGICAL FILE NAMES---------------------------------------
                             D A T A
     & LOROG/14/,NHIBU /54/,NFCST /53/,NBC   /55/,LIST  /06/
C
      DATA SNUP,SALP /0.040, 2.6/
      real*8 timef
      real snohires_tim,ssthires_tim,sst14k_tim,getveg_tim,putveg_tim
      real gautoeta_tim,readsfc_tim,readit_tim
      REAL MXSNAL
C***********************************************************************
C
      READ(15,ECONDATA)
      WRITE(6,ECONDATA)
C--------------DERIVED VERTICAL CONSTANTS-------------------------------
                             DO 110 L=1,LP1
                             DFRLG(L)=DFL(L)
 110                         DFL(L)=DFL(L)*G
C--------------DUMMY CONSTANTS------------------------------------------
      PT1=PT
      PT2=PT
      R1=R
      R2=R
                             DO 120 L=1,LP1
      ETA1(L)=ETA(L)
 120  ETA2(L)=ETA(L)
                             DO 130 L=1,LM
      DETA1(L)=DETA(L)
      AETA1(L)=AETA(L)
      DETA2(L)=DETA(L)
 130  AETA2(L)=AETA(L)
C--------------DERIVED GEOMETRICAL CONSTANTS----------------------------
      DTR=PI/180.
      DPR=180./PI
      TPH0=TPH0D*DTR
      WB=WBD*DTR
      SB=SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      RDLM=1./DLM
      RDPH=1./DPH
C
      WBI=WB+TDLM
      SBI=SB+TDPH
      EBI=WB+IM2*TDLM
      ANBI=SB+JM3*DPH
C
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
C---------------TIME STEPPING RELATED CONSTANTS-------------------------
      TSPH=3600./DT
      NDDAMP=TDDAMP*TSPH+.5
C
      DTAD=IDTAD
      DTCF=IDTCF
C--------------DERIVED HORIZONTAL GRID CONSTANTS------------------------
      DY=A*DPH
      CPGFV=-DT/(48.*DY)
      EN= DT/( 4.*DY)*DTAD
      ENT=DT/(16.*DY)*DTAD
C
      TPH=SB-DPH
              DO 140 J=1,JM
C
              KHL0(J)=IM*(J-1)-(J-1)/2+1
              KVL1(J)=IM*(J-1)-(J-1)/2+1
              KHL2(J)=IM*(J-1)-(J-1)/2+2
              KVL3(J)=IM*(J-1)-(J-1)/2+2
              KHL4(J)=IM*(J-1)-(J-1)/2+3
              KVL5(J)=IM*(J-1)-(J-1)/2+3
              KHL6(J)=IM*(J-1)-(J-1)/2+4
C
              KVL0(J)=IM*(J-1)-J/2+1
              KHL1(J)=IM*(J-1)-J/2+2
              KVL2(J)=IM*(J-1)-J/2+2
              KHL3(J)=IM*(J-1)-J/2+3
              KVL4(J)=IM*(J-1)-J/2+3
              KHL5(J)=IM*(J-1)-J/2+4
              KVL6(J)=IM*(J-1)-J/2+4
C
              KHH0(J)=IM*J-J/2
              KHH2(J)=IM*J-J/2-1
              KVH1(J)=IM*J-J/2-1
              KHH4(J)=IM*J-J/2-2
              KVH3(J)=IM*J-J/2-2
              KHH6(J)=IM*J-J/2-3
              KVH5(J)=IM*J-J/2-3
C
              KHH1(J)=IM*J-(J+1)/2
              KVH0(J)=IM*J-(J+1)/2
              KHH3(J)=IM*J-(J+1)/2-1
              KVH2(J)=IM*J-(J+1)/2-1
              KHH5(J)=IM*J-(J+1)/2-2
              KVH4(J)=IM*J-(J+1)/2-2
              KVH6(J)=IM*J-(J+1)/2-3
C
              TPH=TPH+DPH
              DXP=A*DLM*COS(TPH)
              DXJ(J)=DXP
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C             WPDARJ(J)=-DT*W*100000./(32.*DXP*DY)
        WPDARJ(J)=-W*((A*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY**2)
     2             /(DT*32.*DXP*DY)*.88
C    2             /(DT*32.*DXP*DY)
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
              CPGFUJ(J)=-DT/(48.*DXP)
              CURVJ(J)=.5*DT*TAN(TPH)/A
              FCPJ(J)=DT/(CP*192.*DXP*DY)
              FDIVJ(J)=1./(12.*DXP*DY)
              EMJ(J)= DT/( 4.*DXP)*DTAD
              EMTJ(J)=DT/(16.*DXP)*DTAD
              FADJ(J)=-DT/(48.*DXP*DY)*DTAD
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C             ACDT=COAC*DT*DTCF
C             ACDT=COAC*DT
C    2            *SQRT((A*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY**2)
              ACDT=DT
     2            *SQRT((A*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY**2)
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C             CDDAMP=CODAMP*ACDT
              CDDAMP=0.04*CODAMP*ACDT
C             HDACJ(J)=ACDT/(4.*DXP*DY)
              HDACJ(J)=COAC*ACDT/(4.*DXP*DY)
              DDMPUJ(J)=CDDAMP/DXP
              DDMPVJ(J)=CDDAMP/DY
 140  CONTINUE
C--------------SPREADING OF UPSTREAM HEIGHT-POINT ADVECTION FACTOR------
      JA=0
              DO 161 J=3,5
          JA=JA+1
          KHLA(JA)=KHL2(J)
          KHHA(JA)=KHH2(J)
 161      EMT(JA)=EMTJ(J)
              DO 162 J=JM4,JM2
          JA=JA+1
          KHLA(JA)=KHL2(J)
          KHHA(JA)=KHH2(J)
 162      EMT(JA)=EMTJ(J)
              DO 163 J=6,JM5
          JA=JA+1
          KHLA(JA)=KHL2(J)
          KHHA(JA)=KHL3(J)
 163      EMT(JA)=EMTJ(J)
              DO 164 J=6,JM5
          JA=JA+1
          KHLA(JA)=KHH3(J)
          KHHA(JA)=KHH2(J)
 164      EMT(JA)=EMTJ(J)
C--------------SPREADING OF UPSTREAM VELOCITY-POINT ADVECTION FACTOR----
      JA=0
              DO 171 J=3,5
          JA=JA+1
          KVLA(JA)=KVL2(J)
          KVHA(JA)=KVH2(J)
 171      EM(JA)=EMJ(J)
              DO 172 J=JM4,JM2
          JA=JA+1
          KVLA(JA)=KVL2(J)
          KVHA(JA)=KVH2(J)
 172      EM(JA)=EMJ(J)
              DO 173 J=6,JM5
          JA=JA+1
          KVLA(JA)=KVL2(J)
          KVHA(JA)=KVL3(J)
 173      EM(JA)=EMJ(J)
              DO 174 J=6,JM5
          JA=JA+1
          KVLA(JA)=KVH3(J)
          KVHA(JA)=KVH2(J)
 174      EM(JA)=EMJ(J)
C--------------CORIOLIS PARAMETER IN TLL SYSTEM & RELATED CONSTANTS-----
              TPH=SB-DPH
              DO 180 J=1,JM
C
              TLM=WB-TDLM+MOD(J,2)*DLM
              TPH=TPH+DPH
              STPH=SIN(TPH)
              CTPH=COS(TPH)
C
          DO 180 I=1,IM
      TLM=TLM+TDLM
      FP=TWOM*(CTPH0*STPH+STPH0*CTPH*COS(TLM))
 180  F(I,J)=0.5*DT*FP
C--------------GEOGRAPHIC LAT AND LONG OF TLL GRID POINTS-----------
              TPH=SB-DPH
              DO 183 J=1,JM
C
              TLM=WB-TDLM+MOD(J+1,2)*DLM
              TPH=TPH+DPH
              STPH=SIN(TPH)
              CTPH=COS(TPH)
C
          DO 183 I=1,IM
      TLM=TLM+TDLM
      SINPHI=CTPH0*STPH+STPH0*CTPH*COS(TLM)
      SINPHI=AMIN1(SINPHI,1.)
      SINPHI=AMAX1(SINPHI,-1.)
      GLAT(I,J)=ASIN(SINPHI)
      COSLAM=CTPH*COS(TLM)/(COS(GLAT(I,J))*CTPH0)-TAN(GLAT(I,J))*
     1 TAN(TPH0)
      IF(COSLAM.GT.H1)COSLAM=H1
      IF(COSLAM.LT.-1.)COSLAM=-1.
      FACT=H1
      IF(TLM.GT.D00)FACT=HM1
      GLON(I,J)=-TLM0D*DTR+FACT*ACOS(COSLAM)
  183 CONTINUE
C-----------------------------------------------------------------------
C READ MAX SNOW ALBEDO FILE
C 90N-20N VIA DAVE ROBINSON, JCAM, 1985, VOL. 24, 402-411
C 20N-90S VIA LAND-SFC TYPE 'CORRELATED' TO ROBINSON DATABASE
C SNALMX = GLOBAL 1-DEG x 1-DEG MAXIMUM SNOW ALBEDO
C
      READ(20)SNALMX
C-----------------------------------------------------------------------
C READ ALBEDO FILES
C GLOBAL 1-DEG x 1-DEG (4) SEASONAL SNOWFREE ALBEDO
C 90N-90S VIA MATTHEWS ***get reference
C ALBC1 = 3-MONTH AVERAGE CENTERED ON 31 Jan
C ALBC2 = 3-MONTH AVERAGE CENTERED ON 30 Apr
C ALBC3 = 3-MONTH AVERAGE CENTERED ON 31 Jul
C ALBC4 = 3-MONTH AVERAGE CENTERED ON 31 Oct
C *NOTE: in future, replace w/high-res 0.144-deg albedo NESDIS database
C   ...and follow similar spatial/temporal averaging for greenness frac
C
      READ(21)ALBC1
      READ(22)ALBC2
      READ(23)ALBC3
      READ(24)ALBC4
      print *,'ok after reading albedo files'
C-----------------------------------------------------------------------
C TEMPORAL INTERPOLATION OF BASELINE SNOWFREE ALBEDO
C INPUT:   ALBC1,ALBC2,ALBC3,ALBC4
C OUTPUT:  ALBC (GLOBAL 1-DEG x 1-DEG) FOR A GIVEN JULIAN DAY
C FIND JULIAN DAY OF YEAR
C
      JULD = JULM(IDAT(1)) + IDAT(2)
C-----------------------------------------------------------------------
C JAN:  AVERAGE USING 1ST/4TH QTR ALBEDOS
C
      IF(JULD.LE.32) THEN
        S1 = 32 - JULD
        S2 = JULD + 30
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC1(I,J) + WGT2 * ALBC4(I,J)
        END DO
        END DO
C-----------------------------------------------------------------------
C FEB/MAR/APR:  AVERAGE USING 1ST/2ND QTR ALBEDOS
C
      ELSE IF(JULD.LE.121.AND.JULD.GT.32) THEN
        S1 = 121 - JULD
        S2 = JULD - 32
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC2(I,J) + WGT2 * ALBC1(I,J)
        END DO
        END DO
C-----------------------------------------------------------------------
C MAY/JUN/JUL:  AVERAGE USING 2ND/3RD QTR ALBEDOS
C
      ELSE IF(JULD.LE.213.AND.JULD.GT.121) THEN
        S1 = 213 - JULD
        S2 = JULD - 121
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC3(I,J) + WGT2 * ALBC2(I,J)
        END DO
        END DO
C-----------------------------------------------------------------------
C AUG/SEP/OCT:  AVERAGE USING 3RD/4TH QTR ALBEDOS
C
      ELSE IF(JULD.LE.305.AND.JULD.GT.213) then
        S1 = 305 - JULD
        S2 = JULD - 213
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC4(I,J) + WGT2 * ALBC3(I,J)
        END DO
        END DO
      ELSE
C-----------------------------------------------------------------------
C NOV/DEC:  AVERAGE USING 4TH/1ST QTR ALBEDOS
C
        S1 = 365 - JULD + 32
        S2 = JULD - 305
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC1(I,J) + WGT2 * ALBC4(I,J)
        END DO
        END DO
      END IF
C-----------------------------------------------------------------------
C BEGIN SPATIAL INTERPOLATION FOR BASELINE SNOWFREE ALBEDO AND MAX SNOW
C ALBEDOS
C
          DO J=1,JM
          DO I=1,IM
          if(i.eq.303.and.j.eq.143) print *,i,j,sm(i,j)
          if(i.eq.309.and.j.eq.143) print *,i,j,sm(i,j)
          IF (SM(I,J) .LT. 0.9) THEN
C
C-----------------------------------------------------------------------
C IF LANDMASS, DETERMINE LAT,LON AND WEIGHTS
C
            ELAT=H90+GLAT(I,J)/DTR
            ELON=H360-GLON(I,J)/DTR
            IF(ELON.GT.H360)ELON=ELON-H360
            ILON1=INT(ELON)
            DIF=ELON-ILON1
            IF(DIF.GT.D5)ILON1=ILON1+1
            IF(ILON1.EQ.D00)ILON1=360
            ILON2=ILON1+1
            ILAT1=INT(ELAT)
            DIF=ELAT-ILAT1
            IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
            ILAT2=ILAT1+1
            W1=ELON-ILON1+D5
            IF(W1.LT.D00)W1=W1+H360
            W2=ELAT-ILAT1+D5
C
            AR1=W1*W2
            AR2=W1*(H1-W2)
            AR3=(H1-W1)*(H1-W2)
            AR4=(H1-W1)*W2
C-----------------------------------------------------------------------
C INTERPOLATE BASELINE SNOWFREE ALBEDO TO E GRID
C INPUT:  ALBC (GLOBAL 1-DEG x 1-DEG)
C OUTPUT: ALBASE (SPECIFIED ETA GRID)
C
C INTERPOLATE MAX SNOW ALBEDO TO E GRID
C INPUT:  SNALMX (GLOBAL 1-DEG x 1-DEG)
C OUTPUT: MXSNAL (SPECIFIED ETA GRID)
C
C DOING BOTH BASELINE SNOWFREE ALBEDO AND MAX SNOW ALBEDO INTERPOLATIONS 
C IN THE SAME BLOCK IS POSSIBLE SINCE THEY HAVE IDENTICAL LAND-SEA MASKS
            IF ( (ALBC(ILON2,ILAT2) .NE. 0.) .AND.
     .           (ALBC(ILON2,ILAT1) .NE. 0.) .AND.
     .           (ALBC(ILON1,ILAT1) .NE. 0.) .AND.
     .           (ALBC(ILON1,ILAT2) .NE. 0.) ) THEN
C-----------------------------------------------------------------------
C ALL 4 SURROUNDING POINTS ARE LAND POINTS
              ALBASE(I,J)=AR1*ALBC(ILON2,ILAT2)+
     .	                  AR2*ALBC(ILON2,ILAT1)+
     .                    AR3*ALBC(ILON1,ILAT1)+
     .                    AR4*ALBC(ILON1,ILAT2)
              MXSNAL(I,J)=AR1*SNALMX(ILON2,ILAT2)+
     .	                  AR2*SNALMX(ILON2,ILAT1)+
     .                    AR3*SNALMX(ILON1,ILAT1)+
     .                    AR4*SNALMX(ILON1,ILAT2)
C
            if(i.eq.303 .and .j.eq.143 .or.
     &         i.eq.309 .and. j.eq.143) then
c           if(mod(i,20).eq.0 .and. mod(j,20).eq.0) then
              print *,'albase ',i,j,ar1,ar2,ar3,ar4,ALBC(ILON2,ILAT2),
     &            ALBC(ILON2,ILAT1),ALBC(ILON1,ILAT1),
     &            ALBC(ILON1,ILAT2),ALBASE(I,J)
              print *,'mxsnal ',i,j,ar1,ar2,ar3,ar4,SNALMX(ILON2,ILAT2),
     &            SNALMX(ILON2,ILAT1),SNALMX(ILON1,ILAT1),
     &            SNALMX(ILON1,ILAT2),MXSNAL(I,J)
            endif
C
            ELSE
C-----------------------------------------------------------------------
C ONE OR MORE OF THE 4 SURROUNDING POINT ARE NOT LAND POINTS
C TAKE NEAREST NEIGHBOR LAND POINT IN THE FOLLOWING ORDER:
C (ILON2,ILAT2),(ILON2,ILAT1),(ILON1,ILAT1),(ILON1,ILAT2)
C
              ALBASE(I,J)=0.
              MXSNAL(I,J)=0.
              IF (ALBC(ILON2,ILAT2) .NE. 0.) THEN
	        ALBASE(I,J)=ALBC(ILON2,ILAT2)
	        MXSNAL(I,J)=SNALMX(ILON2,ILAT2)
                if(i.eq.303 .and .j.eq.143 .or.
     &            i.eq.309 .and. j.eq.143) then
                  print *,'albase 1',i,j,albase(i,j)
                  print *,'mxsnal 1',i,j,mxsnal(i,j)
                endif
                IF ( (AR2 .GT. AR1) .AND.
     .               (ALBC(ILON2,ILAT1) .NE. 0.) ) THEN
	          ALBASE(I,J)=ALBC(ILON2,ILAT1)
	          MXSNAL(I,J)=SNALMX(ILON2,ILAT1)
                  if(i.eq.303 .and .j.eq.143 .or.
     &               i.eq.309 .and. j.eq.143) then
                     print *,'albase 2',i,j,albase(i,j)
                     print *,'mxsnal 2',i,j,mxsnal(i,j)
                  endif
                  IF ( (AR3 .GT. AR2) .AND.
     .                 (ALBC(ILON1,ILAT1) .NE. 0.) ) THEN
	            ALBASE(I,J)=ALBC(ILON1,ILAT1)
	            MXSNAL(I,J)=SNALMX(ILON1,ILAT1)
                    if(i.eq.303 .and .j.eq.143 .or.
     &                 i.eq.309 .and. j.eq.143) then
                       print *,'albase 3',i,j,albase(i,j)
                       print *,'mxsnal 3',i,j,mxsnal(i,j)
                    endif
                    IF ( (AR4 .GT. AR3) .AND.
     .                   (ALBC(ILON1,ILAT2) .NE. 0.) ) THEN
	              ALBASE(I,J)=ALBC(ILON1,ILAT2)
	              MXSNAL(I,J)=SNALMX(ILON1,ILAT2)
                      if(i.eq.303 .and .j.eq.143 .or.
     &                   i.eq.309 .and. j.eq.143) then
                         print *,'albase 4',i,j,albase(i,j)
                         print *,'mxsnal 4',i,j,mxsnal(i,j)
                      endif
	            ENDIF
	          ENDIF
	        ENDIF
	      ENDIF
C
C-----------------------------------------------------------------------
C NO SURROUNDING POINTS ARE LAND:
C SET DEFAULT SNOWFREE ALBEDO=20 PERCENT, DEFAULT SNOWALB=55 PERCENT
C
              IF (ALBASE(I,J) .EQ. 0.) THEN
	        ALBASE(I,J)=20.
	        MXSNAL(I,J)=55.
                 PRINT *,'AT: LAT,LON',ELAT,ELON
                 PRINT *,'SNOWFREE ALBEDO SET TO DEFAULT VALUE OF 20%'
                 PRINT *,'MAX SNOW ALBEDO SET TO DEFAULT VALUE OF 55%'
	      ENDIF
            ENDIF
	  ENDIF
C-----------------------------------------------------------------------
C CONVERT ALBEDO UNITS: PERCENT TO FRACTION
          ALBASE(I,J)=ALBASE(I,J)*D01
          MXSNAL(I,J)=MXSNAL(I,J)*D01
        ENDDO
      ENDDO
      print *,'303,143 ',albase(303,143),mxsnal(303,143)
      print *,'309,143 ',albase(309,143),mxsnal(309,143)
C--------------DERIVED VERTICAL GRID CONSTANTS--------------------------
      EF4T=.5*DT/CP
      F4Q =   -DT*DTAD
      F4D =-.5*DT*DTAD
                                 DO 195 L=1,LM
                             RDETA(L)=1./DETA(L)
 195                         F4Q2(L)=-.25*DT*DTAD/DETA(L)
C--------------BOUNDARY MASKS-------------------------------------------
      DO J=1,JM
      DO I=1,IM
       HBM2(I,J)=0.
       VBM2(I,J)=0.
       VBM3(I,J)=0.
      ENDDO
      ENDDO
      DO J=3,JM2
        KHH=IM-2+MOD(J,2)
        DO I=2,KHH
         HBM2(I,J)=1.
        ENDDO
      ENDDO
      DO J=3,JM2
        KVH=IM-1-MOD(J,2)
        DO I=2,KVH
         VBM2(I,J)=1.
        ENDDO
      ENDDO
      DO J=4,JM3
        KVL=3-MOD(J,2)
        DO I=KVL,IM-2
         VBM3(I,J)=1.
        ENDDO
      ENDDO
C
C  6/15/99 TOPOGRAPHY MASKS NOW READ VIA TERRAIN FILE IN UNIT 14
C
      REWIND LOROG
      READ(LOROG)
      READ(LOROG)
      READ(LOROG) HTM
      READ(LOROG) VTM
      READ(LOROG) LMH
      READ(LOROG) LMV
C
C--------------SPREADING OF LATITUDE DEPENDENT CONSTANTS----------------

      DO J=1,JM
        DO I=1,IM
c       KHH=IM-1+MOD(J,2)
c       DO I=1,KHH
          DX(I,J)=DXJ(J)
          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
          FCP(I,J)=FCPJ(J)
          FDIV(I,J)=FDIVJ(J)
          FAD(I,J)=FADJ(J)
          HDAC(I,J)=HDACJ(J)*1.25*HBM2(I,J)
c       ENDDO
c       KVH=IM-MOD(J,2)
c       DO I=1,KVH
          HDACV(I,J)=HDACJ(J)
          CPGFU(I,J)=CPGFUJ(J)
          CURV(I,J)=CURVJ(J)
        ENDDO
      ENDDO
C--------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------
      DO J=3,JM2
        IF (J.LE.5.OR.J.GE.JM4) THEN
          KHH=IM-2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
          KHH=IM-2+MOD(J,2)
          DO I=IM-2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO
CMEB          KHL2(J)=IM*(J-1)-(J-1)/2+2 ! 2 if j odd or j even
CMEB          KHL3(J)=IM*(J-1)-J/2+3     ! 3 if j odd, 2 if j even
CMEB          KHH2(J)=IM*J-J/2-1         ! IM-1 if j odd, IM-2 if j even
CMEB          KHH3(J)=IM*J-(J+1)/2-1     ! IM-2 if j odd, or even

C-----------------------------------------------------------------------
      DO J=1,JM
CMEB          KVL0(J)=IM*(J-1)-J/2+1  1 if j odd, 1 if J even
CMEB          KVH0(J)=IM*J-(J+1)/2    IM-1 if j odd, IM if J even
        KVH=IM-MOD(J,2)
        DO I=1,KVH
         DDMPU(I,J)=DDMPUJ(J)*VBM2(I,J)
         DDMPV(I,J)=DDMPVJ(J)*VBM2(I,J)
         HDACV(I,J)=HDACV(I,J)*VBM2(I,J)
        ENDDO
      ENDDO
C--------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------
      DO J=3,JM2
CMEB          KVL3(J)=IM*(J-1)-(J-1)/2+2 2 if j odd, 3 if j even
CMEB          KVL2(J)=IM*(J-1)-J/2+2     2 if j odd or even
CMEB          KVH2(J)=IM*J-(J+1)/2-1     IM-2 if j odd, IM-1 if j even
CMEB          KVH3(J)=IM*J-J/2-2         IM-2 if j odd or even
        IF (J.LE.5.OR.J.GE.JM4) THEN
          KVH=IM-1-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
          KVH=IM-1-MOD(J,2)
          DO I=IM-2,KHH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO
C
C  READ NESDIS AND USAF SNOW MASKS
C
C   READ THE NESDIS-IMS LAND-WATER MASK
C                                          (SEA=0,LAND=1)
      DO J = 1, 1024
         READ(43,12345) (MSKSCVH(I,J),I=1,1024)
12345    FORMAT(80I1)
c        READ(43,'(80I1)') (MSKSCVH(I,J),I=1,1024)
      ENDDO
C
C   READ THE USAF AFGWC LAND/COAST/SEA MASK
C                           (SEA=1,LAND=2,COASTAL-LAND=4,OFFWORLD=9)
      READ(42)  MSKAF
C
C%%%%%%%%%%----SURFACE PARAMETERS----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  THE NEXT THREE SUBROUTINE CALLS INTERPOLATE TO THE ETA GRID:
C    A) - HI RES DAILY NESDIS/IMS 23-KM SNOW/ICE, 
C                DAILY N.H. 47-KM USAF SNOW,
C    B) - HI RES DAILY GLOBAL 1-DEG SST (WHICH IS OVERLAID WITH C BELOW)
C    C) - HI RES DAILY WESTERN N.H. 50-KM SST AND GREAT LAKES 14-KM SST
C
C  ROUTINE SNOHIRES WILL RETURN LUSAF=FALSE IF USAF SNOW I/O
C  PROBLEMS OR IF USAF SNOW ANAL TOO OLD.  WHEN SNOHIRES RETURNS
C  LUSAF=FALSE, THEN IT RETURNS SNOW COVER OF 0'S AND 1'S DERIVED FROM
C  WEEKLY 190-KM NESDIS/SAB ANAL.  WHEN SNOHIRES RETURNS LUSAF=TRUE THEN
C  DAILY USAF SNOWDEPTH IN THE RANGE OF 0 TO 10.9 METERS IS RETURNED.
C  OVER SEA, SNOHIRES RETURNS ICE FLAGS OF 0'S AND 1'S FROM NESDIS ANAL.
C
       btim=timef()
       CALL SNOHIRES (SI,SM,GLAT,GLON,LIST,LUSAF,DTR)
       snohires_tim=timef()-btim
       print *,'ok after snohires, time = ',snohires_tim
C
       btim=timef()
       CALL SSTHIRES (SST,SM,GLAT,GLON,IDAT,LIST,DTR)
       ssthires_tim=timef()-btim
       print *,'ok after ssthires, time = ',ssthires_tim
C
C   NEXT ROUTINE READS IN AND INTERPOLATES A WESTERN N.H. 14-KM SST FLD.
C   THIS 14-KM FLD SPANS A DOMAIN APPROX COVERING (10N-60N, 35W-160W).
C   NOTE: THIS 14-KM SST FLD WAS CREATED IN A PREVIOUS EXTERNAL JOBSTEP
C         AND IS IN REALITY MOSTLY A 50-KM (0.5 DEG) SST WITH 14-KM
C         SST OVERLAID ONLY OVER THE GREAT LAKES
C
       btim=timef()
       CALL SST14K   (SST,SM,GLAT,GLON)
       sst14k_tim=timef()-btim
       print *,'ok after sst14, time = ',sst14k_tim
C
      TPH=SB-DPH
      DO J=1,JM
C
        TLM=WB-TDLM+MOD(J+1,2)*DLM
        TPH=TPH+DPH
        STPH=SIN(TPH)
        CTPH=COS(TPH)
        KHH=IM-MOD(J,2)
C
        DO I=1,KHH
          TLM=TLM+TDLM
          CTLM=COS(TLM)
          STLM=SIN(TLM)
          APH=ASIN(STPH0*CTPH*CTLM+CTPH0*STPH)
C
C
C  IF LUSAF=.FALSE., THEN SI ARE TRADITIONAL FLAGS OF 0'S AND 1'S
C  SO USE TRADITIONAL LOGIC TO SET SNOW DEPTH UP TO 1 METER.
C  AS A FUNCTION OF LATITUDE AND TERRAIN HEIGHT
C
      IF (.NOT.LUSAF.AND.SM(I,J).LT.0.9)SI(I,J)=0.333*SI(I,J)*SIN(APH)
C
C........ IF(FIS(I,J).LT.0.0196.AND.APH/DTR.LT.66.)    SI(I,J)=0.
C
C  DEEP SOIL TEMP TG BELOW NOT INITIALIZED HERE AFTER JIF OF
C  31 JAN 96, SINCE NOW INITIALIZED FROM GDAS
C
COFF...   TG(I,J)=TG0+TGA*COS(APH)+FIS(I,J)/3333.
C
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C  DO-LOOP BELOW DEFINES SOME MASKS AND CHANGES THE MEANING OF OTHERS.
C  MASKS DEFINITIONS FOLLOWING THIS DO-LOOP ARE THOSE CONVENTIONS
C    EXPECTED BY ETA MODEL.
C  --
C  PRIOR TO THIS DO-LOOP:
C
C    SM      LANDMASS (=0), OPEN SEA OR SEAICE (=1)
C    SI      SM=0
C              SI=SNOW DEPTH (0 TO 10.9 M)
C            SM=1
C              SI=1, SEAICE
C              SI=0, OPEN SEA
C    SNO     UNDEFINED
C    SICE    UNDEFINED
C    ALBEDO  UNDEFINED
C  --
C  AFTER THIS DO-LOOP:
C
C    SM      LANDMASS OR SEAICE (=0), OPEN SEA (=1)
C    SI      SNOW DEPTH (M) (ALWAYS ZERO OVER OPEN SEA OR SEAICE)
C    SNO     SNOW WATER EQUIVALENT (M), = 0.2*SI
C    SICE    SICE=0
C              LANDMASS OR OPEN SEA
C            SICE=1
C              SEAICE
C    ALBEDO  SM=1
C              ALBEDO=0.06 OVER OPEN SEA
C            SM=0
C              SICE=0, ALBEDO=DYNAMIC LAND ALBEDO, A FUNCTION OF THE
C                BASELINE SNOWFREE ALBEDO (ALBASE, FROM MATTHEWS) AND
C                INCLUDING A SNOW EFFECT (FROM ROBINSON)
C                SO, DYNAMIC LAND ALBEDO=FCT(ALBASE,MXSNAL,SNO) 
C              SICE=1, ALBEDO=0.60 OVER SEAICE
C-----------------------------------------------------------------------
C
      DO J=1,JM
        DO I=1,IM
        if(i.eq.180.and.j.eq.440) print *,'enter ice ',i,j,
     &     sm(i,j),si(i,j)

          SICE(I,J)=0.
          SNO (I,J)=0.
          IF(ALBEDO(I,J).GT.0.99999999)    ALBEDO(I,J)=0.99999999

          IF(SM(I,J).GT.0.9) THEN
C-----------------------------------------------------------------------
C  SEA
            EPSR(I,J)=.97
            GFFC(I,J)=0.
            ALBEDO(I,J)=.06
c           ALBASE(I,J)=.06

            IF(SI (I,J).GT.0.    ) THEN
C-----------------------------------------------------------------------
C  SEA-ICE      
              SM(I,J)=0.
	      SI(I,J)=0.
              SICE(I,J)=1.
C              TG(I,J)=TI0
              GFFC(I,J)=ROI*CI*AKI/DZI
              ALBEDO(I,J)=.60
              ALBASE(I,J)=.60
            END IF

C-----------------------------------------------------------------------
C LAND
C CALCULATE WATER EQUIVALENT SNOWDEPTH (SNO) FROM ACTUAL SNOWDEPTH
C   USING UNIVERSAL SNOW DENSITY FROM 1:5 RATIO.
          ELSE
            EPSR(I,J)=1.0
            GFFC(I,J)=ROS*CS*AKS/DZG
            SICE(I,J)=0.
            SNO(I,J)=SI(I,J)*.20
          END IF
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C    MONTHLY GREEN VEG FRACTION, INTERPOLATE TO DAY OF YEAR.
C    THE MONTHLY FIELDS ASSUMED VALID AT 15TH OF MONTH.
C    VALUES RANGE FROM 0.001 TO 1.0 OVER LAND, 0.0 OVER WATER.
C    INTERPOLATE IN SPACE.
C
C    READ FROM A FILE THAT HAS 13 RECORDS, ONE PER MONTH WITH
C    JANUARY OCCURRING BOTH AS RECORD 1 AND AGAIN AS RECORD 13,
C    THE LATTER TO SIMPLIFY TIME INTERPOLATION FOR DAYS
C    BETWEEN DEC 16 AND JAN 15. WE TREAT JAN 1 TO JAN 15
C    AS JULIAN DAYS 366 TO 380 BELOW, I.E WRAP AROUND YEAR.
C *** THE FOLLOWING PART WAS REVISED BY F. CHEN 7/96 TO REFLECT 
C        A NEW NESDIS VEGETATION FRACTION PRODUCT (FIVE-YEAR 
C        CLIMATOLOGY WITH 0.144 DEGREE RESOLUTION 
C        FROM 89.928S, 180W TO 89.928N, 180E)
C
       REWIND 38
C
C ****  DO TIME INTERPOLATION ****
       JULD=JULM(IDAT(1))+IDAT(2)
       IF(JULD.LE.15) JULD=JULD+365
       MON2=IDAT(1)
       IF(IDAT(2).GT.15) MON2=MON2+1
       IF(MON2.EQ.1) MON2=13
       MON1=MON2-1
C **** ASSUME DATA VALID AT 15TH OF MONTH 
       DAY2=JULM(MON2)+15
       DAY1=JULM(MON1)+15
       RDAY=JULD
       WGHT1=(DAY2-RDAY)/(DAY2-DAY1)
       WGHT2=(RDAY-DAY1)/(DAY2-DAY1)
C
       btime=timef()
       CALL GETVEG(MON1,MON2,WGHT1,WGHT2)
       getveg_tim=timef()-btim
       print *,'done with getveg, time = ',getveg_tim
C       
c      print *,'begin read of veg.eta file'
c      CALL BAOPEN(38,'fort.38',IRETO)
c      JPDS = -1
c      CALL GETGB(38,0,2500*1250,MON1-1,JPDS,JGDS,KF,KNUM,
c    &    KPDS,KGDS,BITMAP,FPAR1,IRET)
c      WRITE(6,*) 'AFTER GETGB FOR MONTH ', MON1,' IRET=', IRET
C
c      JPDS = -1
c      CALL GETGB(38,0,2500*1250,MON2-1,JPDS,JGDS,KF,KNUM,
c    &    KPDS,KGDS,BITMAP,FPAR2,IRET)
c      WRITE(6,*) 'AFTER GETGB FOR MONTH ', MON2,' IRET=', IRET
C
c      DO J=1,1250
c       DO I=1,2500
c        FSVE=WGHT1*FPAR1(I,J)+WGHT2*FPAR2(I,J)
c        FPAR1(I,J)=FSVE
c      END DO
C ** SPACE INTERPOLATION OF FPAR1 TO E GRID 
       btime=timef()
       CALL PUTVEG(TLM0D,TPH0D,DLMD,DPHD,HLAT,HLON,
     &             FPAR1,SM,SICE,VGFRCK)
       putveg_tim=timef()-btim
       print *,'done with putveg, time = ',putveg_tim
C
C    FIRST THE VEGETATION TYPES
       REWIND 30
       print *,'ok after unit 30 read'     
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          IVGTPK(I,J)=7
         ENDDO
       ENDDO
       CALL PUTEM(30,IVGTPK)
C
C    SECOND THE SOIL TYPES TYPES
       REWIND 31
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          ISLTPK(I,J)=2
         ENDDO
       ENDDO
       CALL PUTEM(31,ISLTPK)
       DO J = 1,JM
         DO I = 1,IM
	 IF(ISLTPK(I,J).EQ.13) ISLTPK(I,J) = 9
         END DO
       END DO
C
C    THIRD THE SURFACE SLOPE TYPES
       REWIND 32
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          ISPTPK(I,J)=1
         ENDDO
       ENDDO
       CALL PUTEM(32,ISPTPK)
       print *,'done with reads of units 30-32'
C
C    NOW THE VEGETATION TYPES
       REWIND 30
       print *,'ok after unit 30 read'     
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          IVGTPK(I,J)=7
         ENDDO
       ENDDO
       CALL PUTEM(30,IVGTPK)
C
C    NEXT THE SOIL TYPES TYPES
       REWIND 31
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          ISLTPK(I,J)=2
         ENDDO
       ENDDO
       CALL PUTEM(31,ISLTPK)
       DO J = 1,JM
         DO I = 1,IM
	 IF(ISLTPK(I,J).EQ.13) ISLTPK(I,J) = 9
         END DO
       END DO
C
C    FINALLY THE SURFACE SLOPE TYPES
       REWIND 32
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       DO J = 1, JM
         DO I =1, IM
          ISPTPK(I,J)=1
         ENDDO
       ENDDO
       CALL PUTEM(32,ISPTPK)
C-----------------------------------------------------------------------
C DETERMINE ALBEDO OVER LAND
      DO J=1,JM
        DO I=1,IM
          IF(SM(I,J).LT.0.9.AND.SICE(I,J).LT.0.9) THEN
C SNOWFREE ALBEDO
            IF ( (SNO(I,J) .EQ. 0.0) .OR.
     .           (ALBASE(I,J) .GE. MXSNAL(I,J) ) ) THEN      
              ALBEDO(I,J) = ALBASE(I,J)         
            if(i.eq.303 .and .j.eq.143 .or.
     &         i.eq.309 .and. j.eq.143) then
                print *,i,j,sno(i,j),albase(i,j),mxsnal(i,j)
            endif
            ELSE
C MODIFY ALBEDO IF SNOWCOVER:
C BELOW SNOWDEPTH THRESHOLD...
              IF (SNO(I,J) .LT. SNUP) THEN
                RSNOW = SNO(I,J)/SNUP
                SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
C ABOVE SNOWDEPTH THRESHOLD...
              ELSE
                SNOFAC = 1.0
              ENDIF
C CALCULATE ALBEDO ACCOUNTING FOR SNOWDEPTH AND VGFRCK 
              ALBEDO(I,J) = ALBASE(I,J)
     .          + (1.0-VGFRCK(I,J))*SNOFAC*(MXSNAL(I,J)-ALBASE(I,J))
            ENDIF
          END IF
        ENDDO
      ENDDO
C
C READ GLOBAL GAUSSIAN FIELDS FROM THE BGES FILE AND INTERPOLATE THEM TO
C THE ETA GRID
C
      btim=timef()
      CALL SFCGDAS
c overwrite GDAS land states from ops EDAS land states
c  inside ops ETA domain
c     CALL SFCEDAS
      CALL SFCEDASC
      readsfc_tim=timef()-btim
      print *,'ok after readsfc, time =',readsfc_tim
C
      DO J = 1,JM
        DO I = 1,IM
          IF(SICE(I,J).EQ.1.0) THEN
           TG(I,J) = TI0
          END IF
        END DO
      END DO
C
c     DEALLOCATE(GGRID1)
c     DEALLOCATE(GLATS)
c     DEALLOCATE(GGSMC2)
c     DEALLOCATE(GGSTC2)
c     DEALLOCATE(GGTG)
c     DEALLOCATE(GGSLI)
C
C  SET VEGETATION CANOPY WATER CONTENT EQUAL TO ZERO EVERYWHERE FOR NOW
      CMC = 0.0
C-----------------------------------------------------------------------
      DO J=1,JM
       DO I=1,IM
        GFFC(I,J)=GFFC(I,J)*HBM2(I,J)
        EPSR(I,J)=EPSR(I,J)*HBM2(I,J)
       ENDDO
      ENDDO
C-----------------------------------------------------------------------
      CALL TABLE(PTBL,TTBL,PT
     1,          RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)
      CALL TABLEQ(TTBLQ,RDPQ,RDTHEQ,PLQ,THL,STHEQ,THE0Q)
C-----------------------------------------------------------------------
      print *,'ok after tables'
C
      WRITE(6,999)LMH(73,33),LMV(73,33)
999   FORMAT(1X,' LMH LMV AT (73,33) = ',2(1X,I8))
      DO L = 1, LM
        WRITE(6,1000)L,HTM(73,33,L),VTM(73,33,L)
1000    FORMAT(1X,' L HTM VTM ',I5,2(1X,E12.5))
      ENDDO
C
      print *,'start write of nhb file'
      NFCSTO = 13
      NBCO = 16
      WRITE(NHIBU)
     1 NFCSTO,NBCO,LIST
     2,DT,IDTAD,SIGMA
     3,KHLA,KHHA,KVLA,KVHA,KHL2,KHH2,KVL2,KVH2
      WRITE(NHIBU)LMH
      WRITE(NHIBU)LMV
      WRITE(NHIBU)HBM2
      WRITE(NHIBU)VBM2
      WRITE(NHIBU)VBM3
      WRITE(NHIBU)SM
      WRITE(NHIBU)SICE
      DO L=1,LM
        WRITE(NHIBU)((HTM(I,J,L),I=1,IM),J=1,JM)
      END DO
      DO L=1,LM
        WRITE(NHIBU)((VTM(I,J,L),I=1,IM),J=1,JM)
      END DO
      WRITE(NHIBU)
     1 DY,CPGFV,EN,ENT,R,PT,TDDAMP
     2,F4D,F4Q,EF4T,DETA,RDETA,AETA,F4Q2,ETA,DFL
     3,EM,EMT
      WRITE(NHIBU)DX
      WRITE(NHIBU)WPDAR
      WRITE(NHIBU)CPGFU
      WRITE(NHIBU)CURV
      WRITE(NHIBU)FCP
      WRITE(NHIBU)FDIV
      WRITE(NHIBU)FAD
      WRITE(NHIBU)F
      WRITE(NHIBU)DDMPU
      WRITE(NHIBU)DDMPV
      WRITE(NHIBU)PT2,GLAT
      WRITE(NHIBU)GLON
      WRITE(NHIBU)PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
      WRITE(NHIBU)
     1 ROS,CS,DS,ROI,CI,DI
     2,PL,THL,RDQ,RDTH,RDP,RDTHE
     3,DETA2,AETA2,DFRLG
     4,QS0,SQS,STHE,THE0
      WRITE(NHIBU)MXSNAL
      WRITE(NHIBU)EPSR
      WRITE(NHIBU)TG
      WRITE(NHIBU)GFFC
      WRITE(NHIBU)SST
      WRITE(NHIBU)ALBASE
      WRITE(NHIBU)HDAC
      WRITE(NHIBU)HDACV
      WRITE(NHIBU)TTBLQ
      WRITE(NHIBU)
     1 PTBL,TTBL
     2,R1,PT1,TSPH
     3,WBD,SBD,TLM0D,TPH0D,DLMD,DPHD,CMLD,DP30,X1P,Y1P,IXM,IYM
     4,DETA1,AETA1,ETA1
C-----------------------------------------------------------------------
C    TIME TO WRITE THE SOIL MODEL STUFF TO NHIBU
C
C    FIRST THE VEGETATION TYPES
       WRITE(NHIBU) IVGTPK
C
C    SECOND THE SOIL TYPES TYPES
       WRITE(NHIBU) ISLTPK
C
C    THIRD THE SURFACE SLOPE TYPES
       WRITE(NHIBU) ISPTPK
C
C    FOURTH VARIABLE IS VEGFRC
C    WRITE OUT GREENNESS FRACTION
         WRITE(NHIBU) VGFRCK
C************* END OF TIME AND SPACE INTERPOLATION OF FPAR ***
C
C    FIFTH THE SOIL DEPTH  
C    VARIABLE IS SLDPTH
         WRITE(NHIBU) SLDPTH
C    SIXTH THE ROOTING DENSITY
C    VARIABLE IS RDTPTH
         WRITE(NHIBU) RTDPTH
C-----------------------------------------------------------------------
                             RETURN
                             END
