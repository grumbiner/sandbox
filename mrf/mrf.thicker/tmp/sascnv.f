CFPP$ NOCONCUR R
CFPP$ EXPAND(FPVS)
C-----------------------------------------------------------------------
      SUBROUTINE SASCNV(IM,IX,KM,JCAP,DELT,DEL,SL,SLK,PS,
     &              Q1,T1,CLDWRK,RN,KBOT,KTOP,KUO,SPD,LAT,SLIMSK,DOT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SASCNV      COMPUTES CONVECTIVE HEATING AND MOISNG
C   PRGMMR: HUA-LU PAN       ORG: W/NMC23    DATE: 92-03-01
C
C ABSTRACT: COMPUTES CONVECTIVE HEATING AND MOISTENING USING A ONE
C   CLOUD TYPE ARAKAWA-SCHUBERT CONVECTION SCHEME ORIGINALLY DEVELOPED
C   BY GEORG GRELL. THE SCHEME INCLUDES UPDRAFT AND DOWNDRAFT EFFECTS.
C   THE CLOSURE IS THE CLOUD WORK FUNCTION. BOTH UPDRAFT AND DOWNDRAFT
C   ARE ASSUMED TO BE SATURATED AND THE HEATING AND MOISTENING ARE
C   ACCOMPLISHED BY THE COMPENSATING ENVIRONMENT. THE NAME COMES FROM
C   "SIMPLIFIED ARAKAWA-SCHUBERT CONVECTION PARAMETERIZATION".
C
C PROGRAM HISTORY LOG:
C   92-03-01  HUA-LU PAN
C
C USAGE:    CALL SASCNV(IM,IX,KM,JCAP,DELT,DEL,SL,SLK,PS,QN,TN,
C    &                Q1,T1,RN,KBOT,KTOP,KUO,SPD,LAT,SLIMSK)
C
C   INPUT ARGUMENT LIST:
C     IM       - INTEGER NUMBER OF POINTS
C     IX       - LEADING DIMENSION OF QN,TN,Q1,T1,SPD
C     KM       - INTEGER NUMBER OF LEVELS
C     JCAP     - INTEGER SPECTRAL TRUNCATION
C     DT       - REAL TIME STEP IN SECONDS
C     DEL      - REAL (KM) SIGMA LAYER THICKNESS
C     SL       - REAL (KM) SIGMA VALUES
C     SLK      - REAL (KM) SIGMA VALUES TO THE KAPPA
C     PS       - REAL (IM) SURFACE PRESSURE IN KILOPASCALS (CB)
C     QN       - REAL (IX,KM) PREVIOUS SPECIFIC HUMIDITY IN KG/KG
C     TN       - REAL (IX,KM) PREVIOUS TEMPERATURE IN KELVIN
C     Q1       - REAL (IX,KM) CURRENT SPECIFIC HUMIDITY IN KG/KG
C     T1       - REAL (IX,KM) CURRENT TEMPERATURE IN KELVIN
C     SPD      - REAL (IX,KM) CURRENT WIND SPEED
C     LAT      - INTEGER  CURRENT LATITUDE INDEX
C     SLIMSK   - REAL (IM) LAND(1),SEA(0), ICE(2) FLAG
C
C   OUTPUT ARGUMENT LIST:
C     Q1       - REAL (IX,KM) ADJUSTED SPECIFIC HUMIDITY IN KG/KG
C     T1       - REAL (IX,KM) ADJUSTED TEMPERATURE IN KELVIN
C     RN       - REAL (IM) CONVECTIVE RAIN IN METERS
C     KBOT     - INTEGER (IM) CLOUD BOTTOM LEVEL
C     KTOP     - INTEGER (IM) CLOUD TOP LEVEL
C     KUO      - INTEGER (IM) BIT FLAG INDICATING DEEP CONVECTION
C
C SUBPROGRAMS CALLED:
C   FPVS     - FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
C
C REMARKS: FUNCTION FPVS IS INLINED BY FPP.
C          NONSTANDARD AUTOMATIC ARRAYS ARE USED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77.
C   MACHINE:  CRAY.
C
C$$$
      DIMENSION DEL(KM),SL(KM),SLK(KM),PS(IM),
     &          Q1(IX,KM),T1(IX,KM),RN(IM),KBOT(IM),KTOP(IM),KUO(IM)
      DIMENSION SPD(IM,KM),SLIMSK(IM),DOT(IM,KM),CLDWRK(IM)
C  PHYSICAL PARAMETERS
      PARAMETER(G= 9.8000E+0 ,RD= 2.8705E+2 ,RV= 4.6150E+2 ,
     &          CP= 1.0046E+3 ,HVAP= 2.5000E+6 ,T0C= 2.7315E+2 )
      PARAMETER(CPOEL=CP/HVAP,ELOCP=HVAP/CP,
     &          EL2ORC=HVAP*HVAP/(RV*CP),EPS=RD/RV,EPSM1=RD/RV-1.)
      PARAMETER(TERR=0.,C0=.002,DELTA= 0.6077338 )
      PARAMETER(FACT1=( 1.8460E+3 - 4.1855E+3 )/RV,FACT2=HVAP/RV-FACT1*T
     10C)
C  LOCAL VARIABLES AND ARRAYS
      DIMENSION P(IM,KM),PDOT(IM),ACRTFCT(IM),
     &  TO(IM,KM),QO(IM,KM)
      DIMENSION QESO(IM,KM)
      DIMENSION TVO(IM,KM),DBYO(IM,KM),
     &  ZO(IM,KM),
     &  HEO(IM,KM),HESO(IM,KM),
     &  XQ(IM,KM),XQES(IM,KM),XT(IM,KM),
     &  XTV(IM,KM),XZ(IM,KM),
     &  XHE(IM,KM),XHES(IM,KM),PW(IM,KM),
     &  TOL(IM,KM),QOL(IM,KM),QESOL(IM,KM),HEOL(IM,KM),
     &  HESOL(IM,KM),XTL(IM,KM),XQL(IM,KM),XQESL(IM,KM),
     &  XHEL(IM,KM),XHESL(IM,KM),
     &  QRCD(IM,KM),DELLAH(IM,KM),DELLAQ(IM,KM),
     &  DELLAT(IM,KM),
     &  HCKO(IM,KM),
     &  QCKO(IM,KM),ETA(IM,KM),
     &  ETAD(IM,KM),XHCK(IM,KM),XQCK(IM,KM),
     &  QRCDO(IM,KM),
     &  PWO(IM,KM),PWDO(IM,KM),DTCONV(IM),DELTV(IM),ACRT(IM)
      DIMENSION PSFC(IM),HMAX(IM),KB(IM),
     &  HKBO(IM),QKBO(IM),KBCON(IM),PBCDIF(IM),
     &  VMAX(IM),KDS(IM),
     &  HMIN(IM),LMIN(IM),JMIN(IM),PWAVO(IM),
     &  AA1(IM),VSHEAR(IM),SHRMAX(IM),
     &  KSHMAX(IM),EDT(IM),
     &  EDTO(IM),PWEVO(IM),
     &  QCOND(IM),
     &  HCDO(IM),QCDO(IM),DDP(IM),PP2(IM),ADET(IM),AATMP(IM),
     &  XHKB(IM),XQKB(IM),XPWAV(IM),XPWEV(IM),XHCD(IM),
     &  XAA0(IM),F(IM),XK(IM),XMB(IM),KTCON(IM),
     &  EDTX(IM),XQCD(IM),
     &  HSBAR(IM),XMBMAX(IM),XLAMB(IM),XLAMD(IM),KBDTR(IM)
      DIMENSION DELHBAR(IM),DELQBAR(IM),DELTBAR(IM)
      DIMENSION PCRIT(15), ACRITT(15), ACRIT(15)
      REAL MBDT
      SAVE PCRIT, ACRITT
      LOGICAL TOTFLG, CNVFLG(IM), DWNFLG(IM), DWNFLG2(IM), FLG(IM)
      DATA PCRIT/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,
     &           350.,300.,250.,200.,150./
      DATA ACRITT/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,
     &           .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
C  GDAS DERIVED ACRIT
C     DATA ACRITT/.203,.515,.521,.566,.625,.665,.659,.688,
C    &            .743,.813,.886,.947,1.138,1.377,1.896/
C-----------------------------------------------------------------------
C  INITIALIZE ARRAYS
C
      DO I=1,IM
        RN(I)=0.
        KBOT(I)=KM+1
        KTOP(I)=0
        KUO(I)=0
        CNVFLG(I) = .TRUE.
        DTCONV(I) = 3600.
        CLDWRK(I) = 0.
        PDOT(I) = 0.
      ENDDO
      DO K = 1, 15
        ACRIT(K) = ACRITT(K) * (975. - PCRIT(K))
      ENDDO
      DT2 = 2. * DELT
C  MODEL TUNABLE PARAMETERS ARE ALL HERE
      MBDT = 10.
      EDTMAX = .3
      ALPHA = .5
      BETA = .15
      EVEF = 0.07
      PDPDWN=0.
      PDETRN=200.
      W1 = -2.E-3 * (JCAP / 62.) **2
      W2 = -1.E-2 * (JCAP / 62.) **2
      W3 = -1.E-2 * (JCAP / 62.) **2
      W4 = -1.E-3 * (JCAP / 62.) **2
CCCCC IF(IM.EQ.384) THEN
CCCCC   LATD = 45
CCCCC   LOND = 376
CCCCC ELSEIF(IM.EQ.768) THEN
CCCCC   LATD = 80
CCCCC   LOND = 81
CCCCC ELSE
CCCCC   LATD = 0
CCCCC   LOND = 0
CCCCC ENDIF
C
C  DEFINE TOP LAYER FOR SEARCH OF THE DOWNDRAFT ORIGINATING LAYER
C  AND THE MAXIMUM THETAE FOR UPDRAFT
C
      KBMAX = KM
      KBM = KM
      KMAX = KM
      DO K = 1, KM
        IF(SL(K).GT..45) KBMAX = K + 1
        IF(SL(K).GT..7) KBM = K + 1
        IF(SL(K).GT..04) KMAX = K + 1
      ENDDO
C
C   CONVERT SURFACE PRESSURE TO MB FROM CB
C
      DO I = 1, IM
        PSFC(I) = PS(I) * 10.
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          P(I,K) = PSFC(I) * SL(K)
          PWO(I,K) = 0.
          PWDO(I,K) = 0.
          TO(I,K) = T1(I,K)
          QO(I,K) = Q1(I,K)
          DBYO(I,K) = 0.
        ENDDO
      ENDDO
C
C  COLUMN VARIABLES
C  P IS PRESSURE OF THE LAYER (MB)
C  T IS TEMPERATURE AT T-DT (K)..TN
C  Q IS MIXING RATIO AT T-DT (KG/KG)..QN
C  TO IS TEMPERATURE AT T+DT (K)... THIS IS AFTER ADVECTION AND TURBULAN
C  QO IS MIXING RATIO AT T+DT (KG/KG)..Q1
C
      DO K = 1, KMAX
        DO I = 1, IM
          QESO(I,K) = 10. * FPVS(T1(I,K))
          QESO(I,K) = EPS * QESO(I,K) / (P(I,K) + EPSM1 * QESO(I,K))
          QESO(I,K) = MAX(QESO(I,K),1.E-8)
C         QO(I,K) = MIN(QO(I,K),QESO(I,K))
          TVO(I,K) = TO(I,K) + DELTA * TO(I,K) * QO(I,K)
        ENDDO
      ENDDO
C
C  HYDROSTATIC HEIGHT ASSUME ZERO TERR
C
      DLNSIG = ALOG(SL(1))
      DO I = 1, IM
        ZO(I,1) = TERR - DLNSIG * RD / G * TVO(I,1)
      ENDDO
      DO K = 2, KMAX
        DLNSIG = ALOG(SL(K) / SL(K-1))
        DO I = 1, IM
          ZO(I,K) = ZO(I,K-1) - DLNSIG * RD / G
     &              * .5 * (TVO(I,K) + TVO(I,K-1))
        ENDDO
      ENDDO
C  COMPUTE MOIST STATIC ENERGY
      DO K = 1, KMAX
        DO I = 1, IM
          HEO(I,K) = G * ZO(I,K) + CP * TO(I,K) + HVAP * QO(I,K)
          HESO(I,K) = G * ZO(I,K) + CP * TO(I,K) + HVAP * QESO(I,K)
C         HEO(I,K) = MIN(HEO(I,K),HESO(I,K))
        ENDDO
      ENDDO
C     DO K = 1, KMAX - 1
C       DO I = 1, IM
C         TOL(I,K) = .5 * (TO(I,K) + TO(I,K+1))
C         QOL(I,K) = .5 * (QO(I,K) + QO(I,K+1))
C         QESOL(I,K) = .5 * (QESO(I,K) + QESO(I,K+1))
C         HEOL(I,K) = .5 * (HEO(I,K) + HEO(I,K+1))
C         HESOL(I,K) = .5 * (HESO(I,K) + HESO(I,K+1))
C       ENDDO
C     ENDDO
      DO K = 1, KMAX - 1
        DO I = 1, IM
          DZ = .5 * (ZO(I,K+1) - ZO(I,K))
          DP = .5 * (P(I,K+1) - P(I,K))
          ES = 10. * FPVS(TO(I,K+1))
          PPRIME = P(I,K+1) + EPSM1 * ES
          QS = EPS * ES / PPRIME
          DQSDP = - QS / PPRIME
          DESDT = ES * (FACT1 / TO(I,K+1) + FACT2 / (TO(I,K+1)**2))
          DQSDT = QS * P(I,K+1) * DESDT / (ES * PPRIME)
          GAMMA = EL2ORC * QESO(I,K+1) / (TO(I,K+1)**2)
          DT = (G * DZ + HVAP * DQSDP * DP) / (CP * (1. + GAMMA))
          DQ = DQSDT * DT + DQSDP * DP
          TOL(I,K) = TO(I,K+1) + DT
          QOL(I,K) = QO(I,K+1) + DQ
          PO = .5 * (P(I,K) + P(I,K+1))
          QESOL(I,K) = 10. * FPVS(TOL(I,K))
          QESOL(I,K) = EPS * QESOL(I,K) / (PO + EPSM1 * QESOL(I,K))
          QESOL(I,K) = MAX(QESOL(I,K),1.E-8)
C         QOL(I,K) = MIN(QOL(I,K),QESOL(I,K))
          HEOL(I,K) = .5 * G * (ZO(I,K) + ZO(I,K+1)) +
     &                CP * TOL(I,K) + HVAP * QOL(I,K)
          HESOL(I,K) = .5 * G * (ZO(I,K) + ZO(I,K+1)) +
     &                CP * TOL(I,K) + HVAP * QESOL(I,K)
        ENDDO
      ENDDO
      k = kmax
      do i = 1, im
        heol(i,k) = heo(i,k)
        hesol(i,k) = heso(i,k)
      enddo
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND)) THEN
CCCCC   PRINT *, '   HEO ='
CCCCC   PRINT 6001, (HEO(LOND,K),K=1,KMAX)
CCCCC   PRINT *, '   HESO ='
CCCCC   PRINT 6001, (HESO(LOND,K),K=1,KMAX)
CCCCC   PRINT *, '   TO ='
CCCCC   PRINT 6002, (TO(LOND,K)-273.16,K=1,KMAX)
CCCCC   PRINT *, '   QO ='
CCCCC   PRINT 6003, (QO(LOND,K),K=1,KMAX)
CCCCC   PRINT *, '   QSO ='
CCCCC   PRINT 6003, (QESO(LOND,K),K=1,KMAX)
CCCCC   PRINT *, '   HEOL ='
CCCCC   PRINT 6001, (HEOL(LOND,K),K=1,KMAX-1)
CCCCC   PRINT *, '   HESOL ='
CCCCC   PRINT 6001, (HESOL(LOND,K),K=1,KMAX-1)
CCCCC   PRINT *, '   TOL ='
CCCCC   PRINT 6002, (TOL(LOND,K)-273.16,K=1,KMAX-1)
CCCCC   PRINT *, '   QOL ='
CCCCC   PRINT 6003, (QOL(LOND,K),K=1,KMAX-1)
CCCCC   PRINT *, '   QSOL ='
CCCCC   PRINT 6003, (QESOL(LOND,K),K=1,KMAX-1)
CCCCC ENDIF
C
C  DETERMINE LEVEL WITH LARGEST MOIST STATIC ENERGY
C  THIS IS THE LEVEL WHERE UPDRAFT STARTS
C
      DO I = 1, IM
        HMAX(I) = HEO(I,1)
        KB(I) = 1
      ENDDO
      DO K = 2, KBM
        DO I = 1, IM
          IF(HEO(I,K).GT.HMAX(I).AND.CNVFLG(I)) THEN
            KB(I) = K
            HMAX(I) = HEO(I,K)
          ENDIF
        ENDDO
      ENDDO
C
C  LOOK FOR CONVECTIVE CLOUD BASE AS THE LEVEL OF FREE CONVECTION
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KB(I)
C         HKBO(I) = .5 * (HEO(I,INDX) + HEO(I,INDX+1))
C         QKBO(I) = .5 * (QO(I,INDX) + QO(I,INDX+1))
          HKBO(I) = HEOL(I,INDX)
          QKBO(I) = QOL(I,INDX)
        ENDIF
      ENDDO
      DO I = 1, IM
        FLG(I) = CNVFLG(I)
        KBCON(I) = KMAX
      ENDDO
      DO K = 1, KBMAX
        DO I = 1, IM
          IF(FLG(I).AND.K.GT.KB(I)) THEN
C           HSBAR(I) = .5 * (HESO(I,K) + HESO(I,K+1))
            HSBAR(I) = HESOL(I,K)
            IF(HKBO(I).GT.HSBAR(I)) THEN
              FLG(I) = .FALSE.
              KBCON(I) = K
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          PBCDIF(I) = -P(I,KBCON(I)) + P(I,KB(I))
          PDOT(I) = 10.* DOT(I,KBCON(I))
          IF(PBCDIF(I).GT.150.) CNVFLG(I) = .FALSE.
          IF(KBCON(I).EQ.KMAX) CNVFLG(I) = .FALSE.
        ENDIF
      ENDDO
      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN
C  FOUND LFC, CAN DEFINE REST OF VARIABLES
 6001 FORMAT(2X,-2P10F12.2)
 6002 FORMAT(2X,10F12.2)
 6003 FORMAT(2X,3P10F12.2)
C
C  DETERMINE ENTRAINMENT RATE BETWEEN KB AND KBCON
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          IF(KB(I).EQ.1) THEN
            DZ = .5 * (ZO(I,KBCON(I)) + ZO(I,KBCON(I)-1)) - ZO(I,1)
          ELSE
            DZ = .5 * (ZO(I,KBCON(I)) + ZO(I,KBCON(I)-1))
     &         - .5 * (ZO(I,KB(I)) + ZO(I,KB(I)-1))
          ENDIF
          IF(KBCON(I).NE.KB(I)) THEN
            XLAMB(I) = -ALOG(ALPHA) / DZ
          ELSE
            XLAMB(I) = 0.
          ENDIF
        ENDIF
      ENDDO
C  DETERMINE UPDRAFT MASS FLUX
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            ETA(I,K) = 1.
          ENDIF
        ENDDO
      ENDDO
      DO K = KBMAX, 2, -1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LT.KBCON(I).AND.K.GE.KB(I)) THEN
            DZ = .5 * (ZO(I,K+1) - ZO(I,K-1))
            ETA(I,K) = ETA(I,K+1) * EXP(-XLAMB(I) * DZ)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.KB(I).EQ.1.AND.KBCON(I).GT.1) THEN
          DZ = .5 * (ZO(I,2) - ZO(I,1))
          ETA(I,1) = ETA(I,2) * EXP(-XLAMB(I) * DZ)
        ENDIF
      ENDDO
C
C  WORK UP UPDRAFT CLOUD PROPERTIES
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KB(I)
          HCKO(I,INDX) = HKBO(I)
          QCKO(I,INDX) = QKBO(I)
          PWAVO(I) = 0.
        ENDIF
      ENDDO
C
C  CLOUD PROPERTY BELOW CLOUD BASE IS MODIFIED BY THE ENTRAINMENT PROCES
C
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LE.KBCON(I)) THEN
            FACTOR = ETA(I,K-1) / ETA(I,K)
            ONEMF = 1. - FACTOR
C           HCKO(I,K) = FACTOR * HCKO(I,K-1) + ONEMF * HEO(I,K)
            HCKO(I,K) = FACTOR * HCKO(I,K-1) + ONEMF *
     &                  .5 * (HEOL(I,K) + HEOL(I,K+1))
C           DBYO(I,K) = HCKO(I,K) - .5 * (HESO(I,K) + HESO(I,K+1))
            DBYO(I,K) = HCKO(I,K) - HESOL(I,K)
          ENDIF
          IF(CNVFLG(I).AND.K.GT.KBCON(I)) THEN
            HCKO(I,K) = HCKO(I,K-1)
            DBYO(I,K) = HCKO(I,K) - HESOL(I,K)
          ENDIF
        ENDDO
      ENDDO
C  DETERMINE CLOUD TOP
      DO I = 1, IM
        FLG(I) = CNVFLG(I)
        KTCON(I) = 1
      ENDDO
C     DO K = 2, KMAX
C       KK = KMAX - K + 1
C       DO I = 1, IM
C         IF(DBYO(I,KK).GE.0..AND.FLG(I).AND.KK.GT.KBCON(I)) THEN
C           KTCON(I) = KK + 1
C           FLG(I) = .FALSE.
C         ENDIF
C       ENDDO
C     ENDDO
      DO K = 2, KMAX
        DO I = 1, IM
          IF(DBYO(I,K).LT.0..AND.FLG(I).AND.K.GT.KBCON(I)) THEN
            KTCON(I) = K
            FLG(I) = .FALSE.
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.(P(I,KBCON(I)) - P(I,KTCON(I))).LT.150.)
     &  CNVFLG(I) = .FALSE.
      ENDDO
      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN
C
C  DETRAINING CLOUD
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          DZ = .5 * (ZO(I,KTCON(I))+ZO(I,KTCON(I)-1))
     &       - .5 * (ZO(I,KBCON(I))+ZO(I,KBCON(I)-1))
          XLAMB(I) = -1. / DZ
        ENDIF
        DWNFLG(I) = CNVFLG(I)
        IF(CNVFLG(I).AND.(P(I,KBCON(I))-P(I,KTCON(I))).GT.PDETRN)
     &     DWNFLG(I)=.FALSE.
      ENDDO
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(DWNFLG(I).AND.K.GT.KBCON(I).AND.K.LE.KTCON(I)) THEN
            DZ = .5 * (ZO(I,K+1) - ZO(I,K-1))
            ETA(I,K) = ETA(I,K-1) * EXP( XLAMB(I) * DZ)
          ENDIF
        ENDDO
      ENDDO
C
C  CLOUD PROPERTY ABOVE CLOUD TOP IS MODIFIED BY THE DETRAINMENT PROCESS
C
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(DWNFLG(I).AND.K.GT.KBCON(I).AND.K.LE.KTCON(I)) THEN
            FACTOR = ETA(I,K-1) / ETA(I,K)
            ONEMF = 1. - FACTOR
            HCKO(I,K) = FACTOR * HCKO(I,K-1) + ONEMF *
     &                  .5 * (HEOL(I,K) + HEOL(I,K+1))
            DBYO(I,K) = HCKO(I,K) - HESOL(I,K)
          ENDIF
        ENDDO
      ENDDO
C  SEARCH FOR DOWNDRAFT ORIGINATING LEVEL ABOVE THETA-E MINIMUM
      DO I = 1, IM
        HMIN(I) = HESO(I,1)
        LMIN(I) = KBMAX
        JMIN(I) = KBMAX
      ENDDO
      DO K = 2, KBMAX
        DO I = 1, IM
          IF(HESO(I,K).LT.HMIN(I).AND.CNVFLG(I)) THEN
            LMIN(I) = K + 1
            HMIN(I) = HESO(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          JMIN(I) = MIN(LMIN(I),KTCON(I)-1)
          XMBMAX(I) = .1
          JMIN(I) = MAX(JMIN(I),KBCON(I)+1)
        ENDIF
      ENDDO
      DO I = 1, IM
        IF(DWNFLG(I).AND.JMIN(I).LE.KBCON(I)) CNVFLG(I) = .FALSE.
        IF(DWNFLG(I).AND.JMIN(I).LE.KBCON(I)) DWNFLG(I) = .FALSE.
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.DWNFLG(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, ' LMIN, PDOT=', LMIN(I), PDOT(I)
CCCCC   PRINT *, ' KBOT, KTOP, JMIN =', KBCON(I), KTCON(I), JMIN(I)
CCCCC ENDIF
      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN
C
C  COMPUTE CLOUD MOISTURE PROPERTY AND PRECIPITATION
C
      DO I = 1, IM
          AA1(I) = 0.
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LT.KTCON(I)) THEN
            DZ = .5 * (ZO(I,K+1) - ZO(I,K-1))
            DZ1 = (ZO(I,K) - ZO(I,K-1))
            GAMMA = EL2ORC * QESOL(I,K) / (TOL(I,K)**2)
C           QRCH = .5 * (QESO(I,K) + QESO(I,K+1))
            QRCH = QESOL(I,K)
     &           + GAMMA * DBYO(I,K) / (HVAP * (1. + GAMMA))
            FACTOR = ETA(I,K-1) / ETA(I,K)
            ONEMF = 1. - FACTOR
C           QCKO(I,K) = FACTOR * QCKO(I,K-1) + ONEMF * QO(I,K)
            QCKO(I,K) = FACTOR * QCKO(I,K-1) + ONEMF *
     &                  .5 * (QOL(I,K) + QOL(I,K+1))
            DQ = ETA(I,K) * QCKO(I,K) - ETA(I,K) * QRCH
C
C  BELOW LFC CHECK IF THERE IS EXCESS MOISTURE TO RELEASE LATENT HEAT
C
            IF(DQ.GT.0.) THEN
              ETAH = .5 * (ETA(I,K) + ETA(I,K-1))
              QLK = DQ / (ETA(I,K) + ETAH * C0 * DZ)
              AA1(I) = AA1(I) - DZ1 * G * QLK
              QC = QLK + QRCH
              PWO(I,K) = ETAH * C0 * DZ * QLK
              QCKO(I,K) = QC
              PWAVO(I) = PWAVO(I) + PWO(I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C  CALCULATE CLOUD WORK FUNCTION AT T+DT
C
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.GT.KBCON(I).AND.K.LT.KTCON(I)) THEN
            DZ1 = ZO(I,K) - ZO(I,K-1)
C           GAMMA = EL2ORC * QESO(I,K) / (TO(I,K)**2)
C           GAMMAO = EL2ORC * QESO(I,K-1) / (TO(I,K-1)**2)
            GAMMA = EL2ORC * QESOL(I,K-1) / (TOL(I,K-1)**2)
C           RFACT =  1. + .25 * DELTA * CP * (GAMMA + GAMMAO)
C    &               * (TO(I,K) + TO(I,K-1)) / HVAP
            RFACT =  1. + DELTA * CP * GAMMA
     &               * TOL(I,K-1) / HVAP
            AA1(I) = AA1(I) +
C    &               DZ1 * (G / (.5 * CP * (TO(I,K)+TO(I,K-1))))
C    &               * DBYO(I,K-1) / (1. + .5 * (GAMMA + GAMMAO))
     &               DZ1 * (G / (CP * TOL(I,K-1)))
     &               * DBYO(I,K-1) / (1. + GAMMA)
     &               * RFACT
            AA1(I)=AA1(I)+
C    &               DZ1 * G * DELTA * .5 *
C    &               (QESO(I,K) + QESO(I,K-1) - QO(I,K) - QO(I,K-1))
     &               DZ1 * G * DELTA *
     &               MAX(0.,(QESOL(I,K-1) - QOL(I,K-1)))
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I).AND.AA1(I).LE.0.) DWNFLG(I) = .FALSE.
        IF(CNVFLG(I).AND.AA1(I).LE.0.) CNVFLG(I) = .FALSE.
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, ' AA1 BEFORE DWNDRFT =', AA1(I)
CCCCC ENDIF
C
C------- DOWNDRAFT CALCULATIONS
C
C  DETERMINE LEVEL WITH LARGEST WIND SPEED
      DO I = 1, IM
          VMAX(I) = SPD(I,KB(I))
          KDS(I) = KB(I)
      ENDDO
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(K.GE.KB(I).AND.SPD(I,K).GT.VMAX(I).AND.CNVFLG(I)
     &       .AND.K.LE.KTCON(I)) THEN
            VMAX(I) = SPD(I,K)
            KDS(I) = K
          ENDIF
        ENDDO
      ENDDO
C
C--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          VSHEAR(I) = 0.
          SHRMAX(I) = 0.
          KSHMAX(I) = 1
        ENDIF
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          IF(K.GE.KB(I).AND.K.LE.KDS(I).AND.CNVFLG(I)) THEN
            SHEAR = ABS((SPD(I,K+1)-SPD(I,K))/(ZO(I,K+1)-ZO(I,K)))
            VSHEAR(I) = VSHEAR(I) + SHEAR
            SHRMAX(I) = MAX(SHRMAX(I),SHEAR)
            IF(SHRMAX(I).EQ.SHEAR) KSHMAX(I) = K
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          KNUMB = KDS(I) - KB(I) + 1
          KNUMB = MAX(KNUMB,1)
          VSHEAR(I) = 1.E3 * VSHEAR(I) / FLOAT(KNUMB)
          E1=1.591-.639*VSHEAR(I)
     &       +.0953*(VSHEAR(I)**2)-.00496*(VSHEAR(I)**3)
          EDT(I)=1.-E1
          EDT(I) = MIN(EDT(I),.9)
          EDT(I) = MAX(EDT(I),.1)
          EDTO(I)=EDT(I)
          EDTX(I)=EDT(I)
        ENDIF
      ENDDO
C  DETERMINE DETRAINMENT RATE BETWEEN 1 AND KBDTR
      DO I = 1, IM
        KBDTR(I) = KBCON(I)
        IF(CNVFLG(I)) THEN
          KBDTR(I) = KBCON(I)
          KBDTR(I) = MAX(KBDTR(I),1)
          XLAMD(I) = 0.
          IF(KBDTR(I).GT.1) THEN
            DZ = .5 * ZO(I,KBDTR(I)) + .5 * ZO(I,KBDTR(I)-1)
     &         - ZO(I,1)
            XLAMD(I) = ALOG(BETA) / DZ
          ENDIF
        ENDIF
      ENDDO
C  DETERMINE DOWNDRAFT MASS FLUX
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            ETAD(I,K) = 1.
          ENDIF
          QRCDO(I,K) = 0.
        ENDDO
      ENDDO
      DO K = KBMAX, 2, -1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LT.KBDTR(I)) THEN
            DZ = .5 * (ZO(I,K+1) - ZO(I,K-1))
            ETAD(I,K) = ETAD(I,K+1) * EXP(XLAMD(I) * DZ)
          ENDIF
        ENDDO
      ENDDO
      K = 1
      DO I = 1, IM
        IF(CNVFLG(I).AND.KBDTR(I).GT.1) THEN
          DZ = .5 * (ZO(I,2) - ZO(I,1))
          ETAD(I,K) = ETAD(I,K+1) * EXP(XLAMD(I) * DZ)
        ENDIF
      ENDDO
C
C--- DOWNDRAFT MOISTURE PROPERTIES
C
      DO I = 1, IM
        PWEVO(I) = 0.
        FLG(I) = CNVFLG(I)
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          JMN = JMIN(I)
C         HCDO(I) = .5 * (HEO(I,JMN) + HEO(I,JMN+1))
C         QCDO(I) = .5 * (QO(I,JMN) + QO(I,JMN+1))
C         QRCDO(I,JMN) = .5 * (QESO(I,JMN) + QESO(I,JMN+1))
          HCDO(I) = HEOL(I,JMN)
          QCDO(I) = QOL(I,JMN)
          QRCDO(I,JMN) = QESOL(I,JMN)
        ENDIF
      ENDDO
      DO K = KMAX-1, 1, -1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LT.JMIN(I)) THEN
C           DQ = .5 * (QESO(I,K) + QESO(I,K+1))
C           DT = .5 * (TO(I,K) + TO(I,K+1))
C           GAMMA = EL2ORC * DQ / DT**2
            DQ = QESOL(I,K)
            DT = TOL(I,K)
            GAMMA = EL2ORC * DQ / DT**2
C           DH = HCDO(I) - .5*(HESO(I,K)+HESO(I,K+1))
            DH = HCDO(I) - HESOL(I,K)
            QRCDO(I,K)=DQ+(1./HVAP)*(GAMMA/(1.+GAMMA))*DH
            DETAD = ETAD(I,K+1) - ETAD(I,K)
            PWDO(I,K) = ETAD(I,K+1) * QCDO(I) -
     &                 ETAD(I,K) * QRCDO(I,K)
              PWDO(I,K) = PWDO(I,K) - DETAD *
     &                   .5 * (QRCDO(I,K) + QRCDO(I,K+1))
            QCDO(I) = QRCDO(I,K)
            PWEVO(I) = PWEVO(I) + PWDO(I,K)
          ENDIF
        ENDDO
      ENDDO
C     IF(LAT.EQ.LATD.AND.DWNFLG(LOND)) THEN
C       I = LOND
C       PRINT *, ' PWAVO, PWEVO =', PWAVO(I), PWEVO(I)
C     ENDIF
C
C--- FINAL DOWNDRAFT STRENGTH DEPENDENT ON PRECIP
C--- EFFICIENCY (EDT), NORMALIZED CONDENSATE (PWAV), AND
C--- EVAPORATE (PWEV)
C
      DO I = 1, IM
        DWNFLG2(I) = CNVFLG(I)
        IF(CNVFLG(I).AND.(P(I,KBCON(I))-P(I,KTCON(I))).LT.PDPDWN)
     &     DWNFLG2(I)=.FALSE.
        IF(DWNFLG2(I)) THEN
          IF(PWEVO(I).LT.0.) THEN
            EDTO(I) = -EDTO(I) * PWAVO(I) / PWEVO(I)
            EDTO(I) = MIN(EDTO(I),EDTMAX)
          ELSE
            EDTO(I) = 0.
          ENDIF
        ELSE
          EDTO(I) = 0.
        ENDIF
      ENDDO
C
C
C--- DOWNDRAFT CLOUDWORK FUNCTIONS
C
C
      DO K = KMAX-1, 1, -1
        DO I = 1, IM
          IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
C           GAMMA1 = EL2ORC * QESO(I,K) / TO(I,K)**2
C           GAMMA2 = EL2ORC * QESO(I,K+1) / TO(I,K+1)**2
            GAMMA = EL2ORC * QESOL(I,K) / TOL(I,K)**2
            DHH=HCDO(I)
C           DT=.5*(TO(I,K)+TO(I,K+1))
C           DG=.5*(GAMMA1+GAMMA2)
C           DH=.5*(HESO(I,K)+HESO(I,K+1))
            DT=TOL(I,K)
            DG=GAMMA
            DH=HESOL(I,K)
            DZ=-1.*(ZO(I,K+1)-ZO(I,K))
            AA1(I)=AA1(I)+EDTO(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))
     &             *(1.+DELTA*CP*DG*DT/HVAP)
            AA1(I)=AA1(I)+EDTO(I)*
C    &      DZ*G*DELTA*.5*(QESO(I,K)+QESO(I,K+1)-QO(I,K)-QO(I,K+1))
     &      DZ*G*DELTA*MAX(0.,(QESOL(I,K)-QOL(I,K)))
          ENDIF
        ENDDO
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.DWNFLG2(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, '  AA1 AFTER DWNDRFT =', AA1(I)
CCCCC ENDIF
      DO I = 1, IM
        IF(AA1(I).LE.0.) CNVFLG(I) = .FALSE.
        IF(AA1(I).LE.0.) DWNFLG(I) = .FALSE.
        IF(AA1(I).LE.0.) DWNFLG2(I) = .FALSE.
      ENDDO
C
C
C--- WHAT WOULD THE CHANGE BE, THAT A CLOUD WITH UNIT MASS
C--- WILL DO TO THE ENVIRONMENT?
C
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            DELLAH(I,K) = 0.
            DELLAQ(I,K) = 0.
            DELLAT(I,K) = 0.
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
C         DP = 100. * (PSFC(I) - .5 * (P(I,1) + P(I,2)))
          DP = 100. * PSFC(I) * DEL(1)
          DELLAH(I,1) = EDTO(I) * ETAD(I,1) * (HCDO(I)
C    &                - .5 * (HEO(I,1) + HEO(I,2))) * G / DP
     &                - HEOL(I,1)) * G / DP
          DELLAQ(I,1) = EDTO(I) * ETAD(I,1) * (QCDO(I)
C    &                - .5 * (QO(I,1) + QO(I,2))) * G / DP
     &                - QOL(I,1)) * G / DP
        ENDIF
      ENDDO
C
C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT
C
      DO K = 2, KMAX-1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LT.KTCON(I)) THEN
            AUP = 1.
            IF(K.LE.KB(I)) AUP = 0.
            ADW = 1.
            IF(K.GT.JMIN(I)) ADW = 0.
C           DV1=.5*(HEO(I,K)+HEO(I,K+1))
C           DV2 = HEO(I,K)
C           DV3=.5*(HEO(I,K)+HEO(I,K-1))
C           DV1Q=.5*(QO(I,K)+QO(I,K+1))
C           DV2Q = QO(I,K)
C           DV3Q=.5*(QO(I,K)+QO(I,K-1))
            DV1= HEOL(I,K)
C           DV2 = HEO(I,K)
            DV2 = .5 * (HEOL(I,K) + HEOL(I,K+1))
            DV3= HEOL(I,K-1)
            DV1Q= QOL(I,K)
C           DV2Q = QO(I,K)
            DV2Q = .5 * (QOL(I,K) + QOL(I,K+1))
            DV3Q= QOL(I,K-1)
C           DP=+50.*(P(I,K-1)-P(I,K+1))
            DP = 100. * PSFC(I) * DEL(K)
            DZ = .5 * (ZO(I,K+1) - ZO(I,K-1))
            DETA = ETA(I,K) - ETA(I,K-1)
            DETAD = ETAD(I,K) - ETAD(I,K-1)
            DELLAH(I,K) = DELLAH(I,K) +
     &          ((AUP * ETA(I,K) - ADW * EDTO(I) * ETAD(I,K)) * DV1
     &      - (AUP * ETA(I,K-1) - ADW * EDTO(I) * ETAD(I,K-1))* DV3
     &                   - AUP * DETA * DV2
     &                  + ADW * EDTO(I) * DETAD * HCDO(I)) * G / DP
            DELLAQ(I,K) = DELLAQ(I,K) +
     &          ((AUP * ETA(I,K) - ADW * EDTO(I) * ETAD(I,K)) * DV1Q
     &      - (AUP * ETA(I,K-1) - ADW * EDTO(I) * ETAD(I,K-1))* DV3Q
     &                  - AUP * DETA * DV2Q
     &     +ADW*EDTO(I)*DETAD*.5*(QRCDO(I,K)+QRCDO(I,K-1))) * G / DP
          ENDIF
        ENDDO
      ENDDO
C
C------- CLOUD TOP
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KTCON(I)
C         IF(INDX.LT.KMAX) THEN
C           DP = 50. * (P(I,INDX-1) - P(I,INDX+1))
C         ELSE
C           DP = 100. * (P(I,INDX-1) - P(I,INDX))
C         ENDIF
          DP = 100. * PSFC(I) * DEL(INDX)
C         DV1 = .5 * (HEO(I,INDX-1) + HEO(I,INDX))
          DV1 = HEOL(I,INDX-1)
          DELLAH(I,INDX) = ETA(I,INDX-1) *
     &                     (HCKO(I,INDX-1) - DV1) * G / DP
C         DVQ1 = .5 * (QO(I,INDX) + QO(I,INDX-1))
          DVQ1 = QOL(I,INDX-1)
          DELLAQ(I,INDX) = ETA(I,INDX-1) *
     &                     (QCKO(I,INDX-1) - DVQ1) * G / DP
        ENDIF
      ENDDO
C
C------- FINAL CHANGED VARIABLE PER UNIT MASS FLUX
C
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            XQ(I,K) = QO(I,K)
            XT(I,K) = TO(I,K)
          ENDIF
          IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
            XQ(I,K) = DELLAQ(I,K) * MBDT + QO(I,K)
            DELLAT(I,K) = (DELLAH(I,K) - HVAP * DELLAQ(I,K)) / CP
            XT(I,K) = DELLAT(I,K) * MBDT + TO(I,K)
            XQ(I,K) = MAX(XQ(I,K),1.E-8)
          ENDIF
        ENDDO
      ENDDO
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C--- THE ABOVE CHANGED ENVIRONMENT IS NOW USED TO CALULATE THE
C--- EFFECT THE ARBITRARY CLOUD (WITH UNIT MASS FLUX)
C--- WOULD HAVE ON THE STABILITY,
C--- WHICH THEN IS USED TO CALCULATE THE REAL MASS FLUX,
C--- NECESSARY TO KEEP THIS CHANGE IN BALANCE WITH THE LARGE-SCALE
C--- DESTABILIZATION.
C
C--- ENVIRONMENTAL CONDITIONS AGAIN, FIRST HEIGHTS
C
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            XQES(I,K) = 10. * FPVS(XT(I,K))
            XQES(I,K) = EPS * XQES(I,K) / (P(I,K) + EPSM1 * XQES(I,K))
            XQES(I,K) = MAX(XQES(I,K),1.E-8)
C           XQ(I,K) = MIN(XQ(I,K),XQES(I,K))
            XTV(I,K) = XT(I,K) + DELTA * XT(I,K) * XQ(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          XAA0(I) = 0.
          XPWAV(I) = 0.
        ENDIF
      ENDDO
C
C  HYDROSTATIC HEIGHT ASSUME ZERO TERR
C
      DLNSIG = ALOG(SL(1))
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          XZ(I,1) = TERR - DLNSIG * RD / G * XTV(I,1)
        ENDIF
      ENDDO
      DO K = 2, KMAX
        DLNSIG = ALOG(SL(K) / SL(K-1))
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            XZ(I,K) = XZ(I,K-1) - DLNSIG * RD / G
     &             * .5 * (XTV(I,K) + XTV(I,K-1))
          ENDIF
        ENDDO
      ENDDO
C
C--- MOIST STATIC ENERGY
C
C     DO K = 1, KMAX
C       DO I = 1, IM
C         IF(CNVFLG(I)) THEN
C           XHE(I,K) = G * XZ(I,K) + CP * XT(I,K) + HVAP * XQ(I,K)
C           XHES(I,K) = G * XZ(I,K) + CP * XT(I,K) + HVAP * XQES(I,K)
C           XHE(I,K) = MIN(XHE(I,K),XHES(I,K))
C         ENDIF
C       ENDDO
C     ENDDO
C     DO K = 1, KMAX - 1
C       DO I = 1, IM
C         IF(CNVFLG(I)) THEN
C           XTL(I,K) = .5 * (XT(I,K) + XT(I,K+1))
C           XQL(I,K) = .5 * (XQ(I,K) + XQ(I,K+1))
C           XQESL(I,K) = .5 * (XQES(I,K) + XQES(I,K+1))
C           XHEL(I,K) = .5 * (XHE(I,K) + XHE(I,K+1))
C           XHESL(I,K) = .5 * (XHES(I,K) + XHES(I,K+1))
C         ENDIF
C       ENDDO
C     ENDDO
      DO K = 1, KMAX - 1
        DO I = 1, IM
          IF(CNVFLG(I)) THEN
            DZ = .5 * (XZ(I,K+1) - XZ(I,K))
            DP = .5 * (P(I,K+1) - P(I,K))
            ES = 10. * FPVS(XT(I,K+1))
            PPRIME = P(I,K+1) + EPSM1 * ES
            QS = EPS * ES / PPRIME
            DQSDP = - QS / PPRIME
            DESDT = ES * (FACT1 / XT(I,K+1) + FACT2 / (XT(I,K+1)**2))
            DQSDT = QS * P(I,K+1) * DESDT / (ES * PPRIME)
            GAMMA = EL2ORC * XQES(I,K+1) / (XT(I,K+1)**2)
            DT = (G * DZ + HVAP * DQSDP * DP) / (CP * (1. + GAMMA))
            DQ = DQSDT * DT + DQSDP * DP
            XTL(I,K) = XT(I,K+1) + DT
            XQL(I,K) = XQ(I,K+1) + DQ
            PO = .5 * (P(I,K) + P(I,K+1))
            XQESL(I,K) = 10. * FPVS(XTL(I,K))
            XQESL(I,K) = EPS * XQESL(I,K) / (PO + EPSM1 * XQESL(I,K))
            XQESL(I,K) = MAX(XQESL(I,K),1.E-8)
C           XQL(I,K) = MIN(XQL(I,K),XQESL(I,K))
            XHEL(I,K) = .5 * G * (XZ(I,K) + XZ(I,K+1)) +
     &                  CP * XTL(I,K) + HVAP * XQL(I,K)
            XHESL(I,K) = .5 * G * (XZ(I,K) + XZ(I,K+1)) +
     &                  CP * XTL(I,K) + HVAP * XQESL(I,K)
          ENDIF
        ENDDO
      ENDDO
      k = kmax
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          XHEL(I,K) = G * XZ(I,K) + CP * XT(I,K) + HVAP * XQ(I,K)
          XHESL(I,K) = G * XZ(I,K) + CP * XT(I,K) + HVAP * XQES(I,K)
C         XHEL(I,K) = MIN(XHEL(I,K),XHESL(I,K))
        ENDIF
      ENDDO
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
          INDX = KB(I)
C         XHKB(I) = .5 * (XHE(I,INDX) + XHE(I,INDX+1))
C         XQKB(I) = .5 * (XQ(I,INDX) + XQ(I,INDX+1))
          XHKB(I) = XHEL(I,INDX)
          XQKB(I) = XQL(I,INDX)
          XHCK(I,INDX) = XHKB(I)
          XQCK(I,INDX) = XQKB(I)
        ENDIF
      ENDDO
C
C
C**************************** STATIC CONTROL
C
C
C------- MOISTURE AND CLOUD WORK FUNCTIONS
C
      DO K = 2, KMAX - 1
        DO I = 1, IM
C         IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LE.KBCON(I)) THEN
          IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LE.KTCON(I)) THEN
            FACTOR = ETA(I,K-1) / ETA(I,K)
            ONEMF = 1. - FACTOR
C           XHCK(I,K) = FACTOR * XHCK(I,K-1) + ONEMF * XHE(I,K)
            XHCK(I,K) = FACTOR * XHCK(I,K-1) + ONEMF *
     &                  .5 * (XHEL(I,K) + XHEL(I,K+1))
          ENDIF
C         IF(CNVFLG(I).AND.K.GT.KBCON(I)) THEN
C           XHCK(I,K) = XHCK(I,K-1)
C         ENDIF
        ENDDO
      ENDDO
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.GT.KB(I).AND.K.LT.KTCON(I)) THEN
            DZ = .5 * (XZ(I,K+1) - XZ(I,K-1))
C           GAMMA = EL2ORC * XQES(I,K) / (XT(I,K)**2)
            GAMMA = EL2ORC * XQESL(I,K) / (XTL(I,K)**2)
C           XDBY = XHCK(I,K) - .5 * (XHES(I,K) + XHES(I,K+1))
            XDBY = XHCK(I,K) - XHESL(I,K)
            XDBY = MAX(XDBY,0.)
C           XQRCH = .5*(XQES(I,K)+XQES(I,K+1))
            XQRCH = XQESL(I,K)
     &           + GAMMA * XDBY / (HVAP * (1. + GAMMA))
            FACTOR = ETA(I,K-1) / ETA(I,K)
            ONEMF = 1. - FACTOR
C           XQCK(I,K) = FACTOR * XQCK(I,K-1) + ONEMF * XQ(I,K)
            XQCK(I,K) = FACTOR * XQCK(I,K-1) + ONEMF *
     &                  .5 * (XQL(I,K) + XQL(I,K+1))
            DQ = ETA(I,K) * XQCK(I,K) - ETA(I,K) * XQRCH
            IF(DQ.GT.0.) THEN
              ETAH = .5 * (ETA(I,K) + ETA(I,K-1))
              QLK = DQ / (ETA(I,K) + ETAH * C0 * DZ)
              XAA0(I) = XAA0(I) - (XZ(I,K) - XZ(I,K-1)) * G * QLK
              XQC = QLK + XQRCH
              XPW = ETAH * C0 * DZ * QLK
              XQCK(I,K) = XQC
              XPWAV(I) = XPWAV(I) + XPW
            ENDIF
          ENDIF
          IF(CNVFLG(I).AND.K.GT.KBCON(I).AND.K.LT.KTCON(I)) THEN
            DZ1 = XZ(I,K) - XZ(I,K-1)
C           GAMMA = EL2ORC * XQES(I,K) / (XT(I,K)**2)
C           GAMMAO = EL2ORC *  XQES(I,K-1) / (XT(I,K-1)**2)
            GAMMA = EL2ORC * XQESL(I,K-1) / (XTL(I,K-1)**2)
C           RFACT =  1. + .25 * DELTA * CP * (GAMMA + GAMMAO)
C    &               * (XT(I,K) + XT(I,K-1)) / HVAP
            RFACT =  1. + DELTA * CP * GAMMA
     &               * XTL(I,K-1) / HVAP
C           XDBY = XHCK(I,K-1) - .5 * (XHES(I,K) + XHES(I,K-1))
            XDBY = XHCK(I,K-1) - XHESL(I,K-1)
            XAA0(I) = XAA0(I)
C    &              + DZ1 * (G / (.5 * CP * (XT(I,K)+XT(I,K-1))))
C    &              * XDBY / (1. + .5 * (GAMMA + GAMMAO))
     &              + DZ1 * (G / (CP * XTL(I,K-1)))
     &              * XDBY / (1. + GAMMA)
     &              * RFACT
            XAA0(I)=XAA0(I)+
C    &               DZ1 * G * DELTA * .5 *
C    &               (XQES(I,K) + XQES(I,K-1) - XQ(I,K) - XQ(I,K-1))
     &               DZ1 * G * DELTA *
     &               MAX(0.,(XQESL(I,K-1) - XQL(I,K-1)))
          ENDIF
        ENDDO
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, ' XAA BEFORE DWNDRFT =', XAA0(I)
CCCCC ENDIF
C
C------- DOWNDRAFT CALCULATIONS
C
C
C--- DOWNDRAFT MOISTURE PROPERTIES
C
      DO I = 1, IM
        XPWEV(I) = 0.
      ENDDO
      DO I = 1, IM
        IF(DWNFLG2(I)) THEN
          JMN = JMIN(I)
C         XHCD(I) = .5 * (XHE(I,JMN) + XHE(I,JMN+1))
C         XQCD(I) = .5 * (XQ(I,JMN) + XQ(I,JMN+1))
C         QRCD(I,JMN) = .5 * (XQES(I,JMN) + XQES(I,JMN+1))
          XHCD(I) = XHEL(I,JMN)
          XQCD(I) = XQL(I,JMN)
          QRCD(I,JMN) = XQESL(I,JMN)
        ENDIF
      ENDDO
      DO K = KMAX-1, 1, -1
        DO I = 1, IM
          IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
C           DQ = .5 * (XQES(I,K) + XQES(I,K+1))
C           DT = .5 * (XT(I,K) + XT(I,K+1))
            DQ = XQESL(I,K)
            DT = XTL(I,K)
            GAMMA = EL2ORC * DQ / DT**2
C           DH = XHCD(I) - .5*(XHES(I,K)+XHES(I,K+1))
            DH = XHCD(I) - XHESL(I,K)
            QRCD(I,K)=DQ+(1./HVAP)*(GAMMA/(1.+GAMMA))*DH
            DETAD = ETAD(I,K+1) - ETAD(I,K)
            XPWD = ETAD(I,K+1) * QRCD(I,K+1) -
     &                 ETAD(I,K) * QRCD(I,K)
            XPWD = XPWD - DETAD *
     &             .5 * (QRCD(I,K) + QRCD(I,K+1))
            XPWEV(I) = XPWEV(I) + XPWD
          ENDIF
        ENDDO
      ENDDO
C
      DO I = 1, IM
        IF(DWNFLG2(I)) THEN
          IF(XPWEV(I).GE.0.) THEN
            EDTX(I) = 0.
          ELSE
            EDTX(I) = -EDTX(I) * XPWAV(I) / XPWEV(I)
            EDTX(I) = MIN(EDTX(I),EDTMAX)
          ENDIF
        ELSE
          EDTX(I) = 0.
        ENDIF
      ENDDO
C
C
C
C--- DOWNDRAFT CLOUDWORK FUNCTIONS
C
C
      DO K = KMAX-1, 1, -1
        DO I = 1, IM
          IF(DWNFLG2(I).AND.K.LT.JMIN(I)) THEN
C           GAMMA1 = EL2ORC * XQES(I,K) / XT(I,K)**2
C           GAMMA2 = EL2ORC * XQES(I,K+1) / XT(I,K+1)**2
            GAMMA = EL2ORC * XQESL(I,K+1) / XTL(I,K+1)**2
            DHH=XHCD(I)
C           DT=.5*(XT(I,K)+XT(I,K+1))
C           DG=.5*(GAMMA1+GAMMA2)
C           DH=.5*(XHES(I,K)+XHES(I,K+1))
            DT= XTL(I,K)
            DG= GAMMA
            DH= XHESL(I,K)
            DZ=-1.*(XZ(I,K+1)-XZ(I,K))
            XAA0(I)=XAA0(I)+EDTX(I)*DZ*(G/(CP*DT))*((DHH-DH)/(1.+DG))
     &              *(1.+DELTA*CP*DG*DT/HVAP)
            XAA0(I)=XAA0(I)+EDTX(I)*
C    &      DZ*G*DELTA*.5*(XQES(I,K)+XQES(I,K+1)-XQ(I,K)-XQ(I,K+1))
     &      DZ*G*DELTA*MAX(0.,(XQESL(I,K)-XQL(I,K)))
          ENDIF
        ENDDO
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.DWNFLG2(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, '  XAA AFTER DWNDRFT =', XAA0(I)
CCCCC ENDIF
C
C  CALCULATE CRITICAL CLOUD WORK FUNCTION
C
      DO I = 1, IM
        ACRT(I) = 0.
        IF(CNVFLG(I)) THEN
C       IF(CNVFLG(I).AND.SLIMSK(I).NE.1.) THEN
          IF(P(I,KTCON(I)).LT.PCRIT(15))THEN
            ACRT(I)=ACRIT(15)*(975.-P(I,KTCON(I)))
     &              /(975.-PCRIT(15))
          ELSE IF(P(I,KTCON(I)).GT.PCRIT(1))THEN
            ACRT(I)=ACRIT(1)
          ELSE
            K = IFIX((850. - P(I,KTCON(I)))/50.) + 2
            K = MIN(K,15)
            K = MAX(K,2)
            ACRT(I)=ACRIT(K)+(ACRIT(K-1)-ACRIT(K))*
     *           (P(I,KTCON(I))-PCRIT(K))/(PCRIT(K-1)-PCRIT(K))
           ENDIF
C        ELSE
C          ACRT(I) = .5 * (P(I,KBCON(I)) - P(I,KTCON(I)))
         ENDIF
       ENDDO
      DO I = 1, IM
        ACRTFCT(I) = 1.
        IF(CNVFLG(I)) THEN
C       IF(CNVFLG(I).AND.SLIMSK(I).EQ.1.) THEN
C         ACRTFCT(I) = PDOT(I) / W3
          IF(PDOT(I).LE.W4) THEN
            ACRTFCT(I) = (PDOT(I) - W4) / (W3 - W4)
          ELSEIF(PDOT(I).GE.-W4) THEN
            ACRTFCT(I) = (PDOT(I) + W4) / (W4 - W3)
          ELSE
            ACRTFCT(I) = 0.
          ENDIF
          ACRTFCT(I) = MAX(ACRTFCT(I),-1.)
          ACRTFCT(I) = MIN(ACRTFCT(I),1.)
          ACRTFCT(I) = 1. - ACRTFCT(I)
          DTCONV(I) = DT2 + (3600. - DT2) *
     &                (PDOT(I) - W2) / (W1 - W2)
          DTCONV(I) = MAX(DTCONV(I), DT2)
        ENDIF
C       FLG(I) = CNVFLG(I)
C       DTCONV(I) = 3.6E4
C       IF(FLG(I).AND.PDOT(I).LT.0.) THEN
C         DTCONV(I) = .01*(P(I,KTCON(I)) - P(I,KBCON(I))) / PDOT(I)
C       ELSEIF(FLG(I)) THEN
C         CNVFLG(I) = .FALSE.
C       ENDIF
      ENDDO
C     DO I = 1, IM
C       IF(CNVFLG(I)) THEN
C         DTCONV(I) = MIN(DTCONV(I),3.6E4)
C         DTCONV(I) = MAX(DTCONV(I),DT2)
C       ENDIF
C     ENDDO
C
C--- LARGE SCALE FORCING
C
      DO I= 1, IM
        FLG(I) = CNVFLG(I)
        IF(CNVFLG(I)) THEN
C         AATMP(I) = MAX(AA0(I),ACRT(I))
C         AATMP(I) = AA0(I)
C         F(I) = (AA1(I) - AATMP(I)) / DT2
C         F(I) = (AA1(I) - AA0(I)) / DT2
C         IF(AA0(I).GT.ACRT(I))
C         IF(SLIMSK(I).EQ.1.)
C    &      F(I) = F(I) + (AATMP(I)-ACRT(I)) / MAX(DT2,DTCONV(I))
C         F(I) = (AA1(I) - ACRT(I)) / DTCONV(I)
C         F(I) = AA1(I) / DTCONV(I)
          F(I) = (AA1(I) - ACRT(I) * ACRTFCT(I)) / DTCONV(I)
          IF(F(I).LE.0.) FLG(I) = .FALSE.
        ENDIF
        CNVFLG(I) = FLG(I)
        IF(CNVFLG(I)) THEN
C         XAA0(I) = MAX(XAA0(I),0.)
          XK(I) = (XAA0(I) - AA1(I)) / MBDT
          IF(XK(I).GE.0.) FLG(I) = .FALSE.
        ENDIF
C
C--- KERNEL, CLOUD BASE MASS FLUX
C
        CNVFLG(I) = FLG(I)
        IF(CNVFLG(I)) THEN
          XMB(I) = -F(I) / XK(I)
          XMB(I) = MIN(XMB(I),XMBMAX(I))
        ENDIF
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND)) THEN
CCCCC   I = LOND
CCCCC   PRINT *, '  A1, XA =', AA1(I), XAA0(I)
CCCCC   PRINT *, ' XMB, ACRT =', XMB(I), ACRT(I)
CCCCC ENDIF
      TOTFLG = .TRUE.
      DO I = 1, IM
        TOTFLG = TOTFLG .AND. (.NOT. CNVFLG(I))
      ENDDO
      IF(TOTFLG) RETURN
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C--- FEEDBACK: SIMPLY THE CHANGES FROM THE CLOUD WITH UNIT MASS FLUX
C---           MULTIPLIED BY  THE MASS FLUX NECESSARY TO KEEP THE
C---           EQUILIBRIUM WITH THE LARGER-SCALE.
C
      DO I = 1, IM
        DELHBAR(I) = 0.
        DELQBAR(I) = 0.
        DELTBAR(I) = 0.
        QCOND(I) = 0.
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
            AUP = 1.
            IF(K.LE.KB(I)) AUP = 0.
            ADW = 1.
            IF(K.GT.JMIN(I)) ADW = 0.
            T1(I,K) = T1(I,K) + DELLAT(I,K) * XMB(I) * DT2
            Q1(I,K) = Q1(I,K) + DELLAQ(I,K) * XMB(I) * DT2
C           IF(K.EQ.1) THEN
C             DP = 100. * (PSFC(I) - .5 * (P(I,1) + P(I,2)))
C           ELSEIF(K.LT.KMAX) THEN
C             DP = 50. * (P(I,K-1) - P(I,K+1))
C           ELSE
C             DP = 100. * (P(I,K-1) - P(I,K))
C           ENDIF
            DP = 100. * PSFC(I) * DEL(K)
            DELHBAR(I) = DELHBAR(I) + DELLAH(I,K)*XMB(I)*DP/G
            DELQBAR(I) = DELQBAR(I) + DELLAQ(I,K)*XMB(I)*DP/G
            DELTBAR(I) = DELTBAR(I) + DELLAT(I,K)*XMB(I)*DP/G
          ENDIF
        ENDDO
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND) ) THEN
CCCCC   I = LOND
CCCCC   PRINT *, ' DELHBAR, DELQBAR, DELTBAR ='
CCCCC   PRINT *, DELHBAR(I), HVAP*DELQBAR(I), CP*DELTBAR(I)
CCCCC ENDIF
      DO I = 1, IM
        DELQBAR(I) = 0.
        DELTBAR(I) = 0.
      ENDDO
      DO K = KMAX, 1, -1
        DO I = 1, IM
          IF(CNVFLG(I).AND.K.LE.KTCON(I)) THEN
            AUP = 1.
            IF(K.LE.KB(I)) AUP = 0.
            ADW = 1.
            IF(K.GT.JMIN(I)) ADW = 0.
            RN(I) = RN(I)
     &            + (AUP * PWO(I,K) + ADW * EDTO(I) * PWDO(I,K))
     &            * XMB(I) * .001 * DT2
            QCOND(I) = EVEF * (QO(I,K) - QESO(I,K)) / (1. + EL2ORC *
     &               QESO(I,K) / TO(I,K)**2)
C           IF(K.EQ.1) THEN
C             DP = 100. * (PSFC(I) - .5 * (P(I,1) + P(I,2)))
C           ELSEIF(K.LT.KMAX) THEN
C             DP = 50. * (P(I,K-1) - P(I,K+1))
C           ELSE
C             DP = 100. * (P(I,K-1) - P(I,K))
C           ENDIF
            DP = 100. * PSFC(I) * DEL(K)
            IF(RN(I).GT.0..AND.QCOND(I).LE.0.) THEN
              QEVAP = -QCOND(I) * (1. - EXP(-.32 * SQRT(DT2 * RN(I))))
              QEVAP = MIN(QEVAP, RN(I)*1000.*G/DP)
              Q1(I,K) = Q1(I,K) + QEVAP
              T1(I,K) = T1(I,K) - ELOCP * QEVAP
              RN(I) = RN(I) - .001 * QEVAP * DP / G
              DELLAT(I,K) = DELLAT(I,K) - ELOCP*QEVAP/XMB(I)/DT2
              DELLAQ(I,K) = DELLAQ(I,K) + QEVAP/XMB(I)/DT2
            ENDIF
            DELQBAR(I) = DELQBAR(I) + DELLAQ(I,K)*XMB(I)*DP/G
            DELTBAR(I) = DELTBAR(I) + DELLAT(I,K)*XMB(I)*DP/G
          ENDIF
        ENDDO
      ENDDO
CCCCC IF(LAT.EQ.LATD.AND.CNVFLG(LOND) ) THEN
CCCCC   I = LOND
CCCCC   PRINT *, '   DELLAH ='
CCCCC   PRINT 6003, (DELLAH(I,K)*XMB(I),K=1,KMAX)
CCCCC   PRINT *, '   DELLAQ ='
CCCCC   PRINT 6003, (HVAP*DELLAQ(I,K)*XMB(I),K=1,KMAX)
CCCCC   PRINT *, '   DELLAT ='
CCCCC   PRINT 6003, (CP*DELLAT(I,K)*XMB(I),K=1,KMAX)
CCCCC   PRINT *, ' DELHBAR, DELQBAR, DELTBAR ='
CCCCC   PRINT *, DELHBAR(I), HVAP*DELQBAR(I), CP*DELTBAR(I)
CCCCC   PRINT *, ' PRECIP =', HVAP*RN(I)*1000./DT2
CCCCC ENDIF
C
C  PRECIPITATION RATE CONVERTED TO ACTUAL PRECIP
C  IN UNIT OF M INSTEAD OF KG
C
      DO I = 1, IM
        IF(CNVFLG(I)) THEN
C
C  IN THE EVENT OF UPPER LEVEL RAIN EVAPORATION AND LOWER LEVEL DOWNDRAF
C    MOISTENING, RN CAN BECOME NEGATIVE, IN THIS CASE, WE BACK OUT OF TH
C    HEATING AND THE MOISTENING
C
          IF(RN(I).LE.0.) THEN
            RN(I) = 0.
          ELSE
            KTOP(I) = KTCON(I)
            KBOT(I) = KBCON(I)
            KUO(I) = 1
            CLDWRK(I) = AA1(I)
          ENDIF
        ENDIF
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          IF(CNVFLG(I).AND.RN(I).LE.0.) THEN
            T1(I,K) = TO(I,K)
            Q1(I,K) = QO(I,K)
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
