CFPP$ EXPAND(FPVS)
      SUBROUTINE CLDJMS(IDIMT,IDIMS,KDIM,NBIN,MCLD,
     1           PS,Q,T,VVEL,CV,CVT,CVB,SI,SL,
     1           SLMSK,CLD,MTOP,MBOT,CLDARY,IVVA,INVR,RHMAX,
     2           XLATRD,RHCLD,ISTRAT)
CTUNE
CFPP$ NOCONCUR R
C....    FROM YH.RAD.MDL93(CLDNEW28).......
C....     LATER UPDATED FROM YH.RAD.MDL94(CLDMUL28)...22JAN94
C....     LATER UPDATED FROM YH.RAD.MDL94(CLDML28A)... 1FEB94
C....     LATER UPDATED FROM YH.RAD.MDL94(CLDML28B)... 5FEB94
C.               SUBR CLDPRP REPLACED
C.               ADDED VERTICAL INTERP OF CLD-RH RELATIONS(ISTRAT GT 1)
C....     LATER UPDATED FROM YH.RAD.MDL94(CLDML28E)... 11MAR94
C.               SUBR CLDPRP REPLACED,GCL ADJUSTED
C....     LATER UPDATED FROM CLOUD6................... 24MAR94
C.               SUBR CLDPRP , LOW ENHANCED TO OLD VALUE..0.14..
C.               SUBR GCLNEW , LLYR CALCULATION ADJ TO OLD VALU(KL-1)
C.                             LLYRL WAS OK.. IVE REMOVED IT AND
C.                             REPLACED IT BY ITS EQUIVALENT, KLOWB
C....     LATER UPDATED FROM CLOUD6................... 30MAR94
C.               SUBR CLDPRP , LOW AND MIDDLE (NOT CV) ENHANCED=0.10
C---------------------------------------------------------------------
C     NOV., 1992 - Y.H., K.A.C., AND A.K.
C        CLOUD PARAMETERIZATION PATTERNED AFTER SLINGO AND SLINGO'S
C        WORK (JGR, 1991).
C     STRATIFORM CLOUDS ARE ALLOWED IN ANY LAYER EXCEPT THE SURFACE
C        AND UPPER STRATOSPHERE.  THE RELATIVE HUMIDITY CRITERION MAY
C        VARY IN DIFFERENT MODEL LAYERS.
CYH94
C     OUTPUT CLOUD AMOUNTS ARE IN CLDARY(I,K), K=1 IS THE LOWEST
C        MODEL LAYER, STRATIFORM (STR) AND CONVECTIVE (CNV) TYPES OF
C        CLOUD ARE COMPRESSED INTO ONE WORD: CAMT = STR + 1.0E4*CNV
C        LOW MARINE STRATUS AMT'S ARE FLAGED BY ADDING 2.
CYH94
CTUNE
C..   FOR ISTRAT = 0, THERE IS RH-CLD RELATION FOR EACH LAYER..
C                      CRIT RH COMPUTED WITHIN..
C..   FOR ISTRAT = 1, RH-CLD RELATION FROM TABLES CREATED USING
C                     MITCHELL-HAHN TUNING TECHNIQUE (A.F. RTNEPH OBS)
C                  ...STRATUS COMPUTED SIMILAR TO OLD OPNL CLDJMS.....
C                      EXCEPT NO CLOUD BELOW LAYER=KLOWB..APPROX 955MB
CTUNE
C     CONVECTIVE CLOUDS ARE FROM MODEL CONVECTIVE SCHEME AND ARE
C        NO LONGER BROKEN INTO .75,.25,.25..RATHER CC ITSELF IS USED..
C        CONVECTIVE STILL TAKES PRECEDENCE OVER STRATIFORM IN RADFS
C         BUT HERE CV+ST MERGE EXITS IN CLDARY...(IN RADIATION USE OF
C        CC GIVES IMPROVEMENT TO TROPICAL MIDDLE CLD (AS DID ST+CV))
C
C     CLOUDS ARE ALSO DIVIDED INTO 3 ATMOSPHERIC DOMAINS (L,M,H) FOR
C        DIAGNOSTIC PURPOSES.  THEY ARE COMPUTED FROM RANDOM OVERLAP
C        ASSUMPTION FOR SEPARATED CLOUD LAYERS AND MAXIMUM OVERLAP
C        FOR ADJACENT CLOUD LAYERS.  A TOTAL CLOUD FRACTION IS ALSO
C        COMPUTED.
C
C     H,M,L DOMAIN PRESSURE TOPS 'PTOP1(K)' VARY LINEARLY FROM
C        'PTOPC(K,1)' AT 45DEG TO 'PTOPC(K,2)' AT THE POLE
C
C--------------------------------------------------------------------
C     INPUT VARIABLES:
C        PS (CB)       - SURFACE PRESSURE
C        Q  (KG/KG)    - SPECIFIC HUMIDITY
C        T  (DEG K)    - ABSOLUTE TEMPERATURE
C        VVEL(CB/SEC)  - VERTICAL VELOCITY
C        CV,CVT,CVB    - CONV CLD FRACTION, TOP, BOTTOM LAYER FROM
C                        KUO SCHEME
C        SI,SL         - MDL SIGMA INTERFACE AND LAYER MEAN
C        SLMSK         - SEA/LAND MASK ARRAY(SEA:0.,LAND:1.,SNOW:2.)
C        IVVA          - FLAG TO CONTROL VERTICAL VELOCITY ADJ.
C                        =1: WITH, =0: WITHOUT
C        INVR          - FLAG TO CONTROL LAPSE RATE INVERSION CLD
C                        =1: WITH, =0: WITHOUT
C        RHMAX         - UPPER LIMIT OF RELATIVE HUMIDITY TO
C                        FORM OVERCAST CLOUD (CLD FRACTN = 1.)
CTUNE
C --------------- MODIFY TO AS AN ARRAY (H.-M. H. JUANG)
C********XLATRD        - CURRENT LATITUDE IN RADIANS (1ST DATA PT)
C********                 FOR MODELS WITH DIFF LAT AT EACH PT, NEED TO
C********                 USE THE LAT OF ALL POINTS....CAREFUL.....
C        RHCLD         - CLOUD-RH RELATIONS FROM MITCHELL+HAHN,
C                        USING A.F. RTNEPH ANALYSES
C        ISTRAT        - 0 OR 1:FOR DEFAULT OR 'RHCLD' TABLES
C                        IN THE STRATIFORM CLOUD CALCULATION
CTUNE
C    OUTPUT VARIABLES:
C       CLDARY         - VERTICAL COLUMN ARRAY OF CLOUD FRACTION
C                        PROFILE
C       CLD            - CLD FRACTION IN 3 TYPES OF DOMAINS (L,M,H)
C                          AND TOTAL IN 4TH LAYER
C       MTOP,MBOT      - TOP, BOTTOM LAYERS OF CLOUDS (L,M,H)
C
C--------------------------------------------------------------------
C
                      P A R A M E T E R
     1 ( RD= 2.8705E+2 , RV= 4.6150E+2 , EPS=RD/RV, EPSM1=RD/RV-1.0, PI=
     1 3.141593E+0  )
                      D I M E N S I O N
     1  PS(IDIMS), CV(IDIMT),  CVT(IDIMT),  CVB(IDIMT)
     2, SLMSK(IDIMT),SI(KDIM+1),  SL (KDIM)
     3, T(IDIMS,KDIM), VVEL(IDIMT,KDIM),   Q(IDIMS,KDIM)
CTOT 4, CLD(IDIMT,3),    MTOP(IDIMT,3),      MBOT(IDIMT,3)
     4, CLD(IDIMT,4),    MTOP(IDIMT,3),      MBOT(IDIMT,3)
CYH945, CLDARY(IDIMT,KDIM), CLSTR(IDIMT)
     5, CLDARY(IDIMT,KDIM),XLATRD(IDIMT)
CTUNE
C...    RH-CLD RELATIONSHIPS FOR EACH POINT
      DIMENSION RHCLD(IDIMT,NBIN,MCLD)
CTUNE
C
C --- PTOPC(K,L): TOP PRESURE OF EACH CLD DOMAIN (K=1-4 ARE SFC,L,M,H;
C       L=1,2 ARE LOW-LAT (<45 DEGREE) AND POLE REGIONS)
      COMMON /COMCD1/ ROCP,PTOPC(4,2),CVTOP,VVCLD(2),CLAPSE
     1,               CRHRH,KLOWT,KLOWB,PSTRT
     2,               LLYR,CLAPKC,DCLPS,CLPSE
C ---  WORKSPACE ---
                        L O G I C A L
CYH941  BITX(IDIMT), BITY(IDIMT), BITZ(IDIMT), BITW(IDIMT), BIT1, BIT2
     1  BITX(IDIMT), BITY(IDIMT), BITZ(IDIMT),              BIT1, BIT2
     2, BITM(IDIMT)
                      D I M E N S I O N
     1  RHRH (IDIMT,KDIM), PRSLY(IDIMT,KDIM),  DTHDP(IDIMT,KDIM)
     2, THETA(IDIMT,KDIM), KCUT (IDIMT),       KBASE(IDIMT)
     3, KBT1 (IDIMT),      KTH1 (IDIMT),       CL1  (IDIMT)
     4, KBT2 (IDIMT),      KTH2 (IDIMT),       CL2  (IDIMT)
     5, KCVB (IDIMT),      KCVT (IDIMT),       OMEG (IDIMT)
CNOT 6, FACV (IDIMT,3),    KSAVE(IDIMT)
     6,                    KSAVE(IDIMT)
     7, PTOP1(IDIMT,4)
CC                    E Q U I V A L E N C E
CC   1  (KBT1, KCVB, THETA(1,1)),   (KBT2, KCUT, THETA(1,2))
CC   2, (KTH1, KCVT, THETA(1,3)),   (KTH2,       THETA(1,4))
CC   3, (CL1 ,       THETA(1,5)),   (CL2 ,       THETA(1,6))
CC   4, (CR1 ,       THETA(1,7)),   (CR2 ,       THETA(1,8))
CC   5, (OMEG,       THETA(1,9)),   (KBASE,      THETA(1,10))
CC   6, (XCRH1,      THETA(1,11)),  (XCRH2,FACV, THETA(1,12))
C===>    BEGIN HERE ................................................
      KDIMP=KDIM+1
      LEVM1=KDIM-1
      LEVM2=KDIM-2
C...  FIND TOP PRESSURE FOR EACH CLOUD DOMAIN
      DO 4 K=1,4
      DO 4 I=1,IDIMT
       FAC = AMAX1(0.0 E 0, 4.0 E 0*ABS(XLATRD(I))/PI-1.0 E 0)
       PTOP1(I,K) = PTOPC(K,1) + (PTOPC(K,2)-PTOPC(K,1)) * FAC
    4 CONTINUE
C --- LOW CLOUD TOP SIGMA LEVEL, COMPUTED FOR EACH LAT CAUSE
C       DOMAIN DEFINITION CHANGES WITH LATITUDE...
      KLOW=KDIM
CBBK  DO 10 I=1,IDIMT
CBBK  SILOW = PTOP1(I,2) * 1.0 E -3
CBBK  DO 6 K=1,KDIM
CBBK    KK=K
CBBK    IF (SI(KK) .LT. SILOW) GO TO 8
C  6  CONTINUE
C  8  KLOW = MIN(KLOW,KK)
C 10  CONTINUE
      DO 10 K=KDIM,1,-1
      DO 10 I=1,IDIMT
        IF (SI(K) .LT. PTOP1(I,2) * 1.0 E -3) KLOW = MIN(KLOW,K)
  10  CONTINUE
C --- POTENTIAL TEMP AND LAYER RELATIVE HUMIDITY
      DO 40 K=1,KDIM
      DO 40 I=1,IDIMT
        CLDARY(I,K) = 0.0 E 0
        PRSLY(I,K) = PS(I) * SL(K) * 10.0 E 0
        EXNR = (PRSLY(I,K)*0.001 E 0) ** (-ROCP)
        THETA(I,K) = EXNR * T(I,K)
        ES = FPVS(T(I,K))
        QS = EPS * ES / (SL(K)*PS(I) + EPSM1*ES)
        RHRH(I,K) = AMAX1(0.0 E 0, AMIN1(1.0 E 0, Q(I,K)/QS))
  40  CONTINUE
C --- POTENTIAL TEMP LAPSE RATE
      DO 50 K=1,LEVM1
      DO 50 I=1,IDIMT
        DTHDP(I,K) = (THETA(I,K+1) - THETA(I,K)) /
     1               (PRSLY(I,K+1) - PRSLY(I,K))
  50  CONTINUE
C ------------------------------------------------------------------
C     FIND THE STRATOSPHERE CUT OFF LAYER FOR HIGH CLOUD. IT
C      IS ASSUMED TO BE ABOVE THE LAYER WITH DTHDP LESS THAN
C      -0.25 IN THE HIGH CLOUD DOMAIN (FROM LOOKING AT 1 CASE).
C ------------------------------------------------------------------
      DO 60 I=1,IDIMT
        KCUT(I) = LEVM2
  60  CONTINUE
      DO 80 K=KLOW+1,LEVM2
        BIT1 = .FALSE.
        DO 70 I=1,IDIMT
          IF (KCUT(I).EQ.LEVM2 .AND. PRSLY(I,K).LE.PTOP1(I,3) .AND.
     1        DTHDP(I,K).LT.-0.25 E 0) THEN
            KCUT(I) = K
          END IF
          BIT1    = BIT1 .OR. KCUT(I).EQ.LEVM2
  70    CONTINUE
        IF (.NOT. BIT1) GO TO 85
  80  CONTINUE
  85  CONTINUE
C ------------------------------------------------------------------
      IF (ISTRAT.LE.0) THEN
C ------------------------------------------------------------------
C       ....DEFAULT SCHEME ....TUNED FOR 28 LYRS BY Y-T HOU.
CYH   CALCULATE STRATIFORM CLOUD AND PUT INTO ARRAY 'CLDARY'
CYH     THE RELATIVE HUMIDITY CRITERIA ARE PRESET FOR EACH MODEL
CYH     SIGMA LEVEL, (1) FOR OCEAN POINTS, AND (2) FOR LAND POINTS.
C ------------------------------------------------------------------
CKAC  DO 130 K=3,LEVM2
      DO 130 K=KLOWB,LEVM2
        BIT1 = .FALSE.
        DO 90 I=1,IDIMT
CYH..     BITX(I) = PRSLY(I,K).LE.PLOW .AND. K.LE.KCUT(I)
          BITX(I) = K.LE.KCUT(I)
          BIT1 = BIT1 .OR. BITX(I)
  90    CONTINUE
        IF (.NOT. BIT1) GO TO 130
        SPNT = AMAX1(0.6 E 0, AMIN1(0.85 E 0, 0.96 E 0-0.6 E 0*SL(K)))
        CR1SE1 = (0.41 E 0*SL(K) - 0.71 E 0)**2 + 0.52 E 0
        CR1SL1 = 0.8 E 0 - 0.167 E 0*SL(K)
        DO 100 I=1,IDIMT
CYH..     CR1 = CRH(K,1)
CYH..     IF (SLMSK(I).EQ.1.0 E 0) CR1 = CRH(K,2)
CYH       XCRH1 = 0.67 * (RHMAX - CR1)
          IF (SLMSK(I).EQ.1.0 E 0) THEN
            CR1 = CR1SE1
          ELSE
            CR1 = CR1SL1
          END IF
          XCRH1 = SPNT * (RHMAX - CR1)
          CR2 = CR1 + XCRH1
          XCRH2 = RHMAX - CR2
          CL1(I) = AMAX1(0. E 0, (RHRH(I,K)-CR1)/XCRH1) ** 3
CYH       CL1(I) = CL1(I)**4
          IF (CL1(I).GT.1.0 E 0)
     1      CL1(I) = 1. E 0 + SQRT((RHRH(I,K)-CR2)/XCRH2)
 100    CONTINUE
        DO 120 I=1,IDIMT
          IF (BITX(I)) THEN
            CLDARY(I,K) = AMIN1(1.0 E 0, 0.5 E 0*CL1(I))
          END IF
 120    CONTINUE
 130  CONTINUE
C ------------------------------------------------------------------
C     SPECIAL TREATMENT ON LOW CLOUDS
C ------------------------------------------------------------------
      DVVCLD = VVCLD(1) - VVCLD(2)
      RCLAP = 1.0 E 0 / (0.8 E 0 - CRHRH)
      DO 180 I=1,IDIMT
        KBASE(I) = 0
 180  CONTINUE
C
      DO 350 K=KLOWB,KLOWT
C
        DO 190 I=1,IDIMT
          OMEG(I) = 10.0 E 0 * VVEL(I,K)
          CL1 (I) = 0.0 E 0
 190    CONTINUE
        IF (IVVA .LE. 0) GO TO 250
C --- VERTICAL VELOCITY ADJUSTMENT ON LOW CLOUDS
        BIT1 = .FALSE.
        DO 210 I=1,IDIMT
          BITX(I) = PRSLY(I,K).GE.PTOP1(I,2) .AND. CLDARY(I,K).GT.0.0 E 
     10
          BIT1 = BIT1 .OR. BITX(I)
 210    CONTINUE
        IF (.NOT. BIT1) GO TO 250
        DO 220 I=1,IDIMT
          IF (BITX(I)) THEN
            IF (OMEG(I).GE.VVCLD(1)) THEN
              CLDARY(I,K) = 0.0 E 0
            ELSE IF(OMEG(I).GT.VVCLD(2)) THEN
              CR1 = (VVCLD(1) - OMEG(I)) / DVVCLD
C             CLDARY(I,K) = CLDARY(I,K) * CR1
              CLDARY(I,K) = CLDARY(I,K) * SQRT(CR1)
            ENDIF
          ENDIF
 220    CONTINUE
C --- T INVERSION RELATED STRATUS CLOUDS
 250    IF (INVR .LT. 1) GO TO 350
        BIT1 = .FALSE.
        DO 260 I=1,IDIMT
          BITX(I) = PRSLY(I,K).GE.PSTRT .AND. SLMSK(I).LE.0.0
     1              .AND. DTHDP(I,K).LE.CLAPSE
CYH  2              .AND. OMEG (I).GT.0.0
          BIT1 = BIT1 .OR. BITX(I)
 260    CONTINUE
        IF (.NOT. BIT1) GO TO 350
        DO 270 I=1,IDIMT
          IF ( KBASE(I).EQ.0  .AND. RHRH(I,K).GT.CRHRH .AND. BITX(I) )
     1      KBASE(I) = K
 270    CONTINUE
        DO 300 I=1,IDIMT
          IF (KBASE(I).GT.0 .AND. BITX(I) .AND. CLDARY(I,K+1).LE.0.1 E -
     11
     1        .AND. CLDARY(I,K+2).LE.0.1 E -1) THEN
            CR1 = AMIN1(1.0 E 0,
     1            AMAX1(0.0 E 0,  16.67 E 0*(CLAPSE-DTHDP(I,K)) ))
            IF(RHRH(I,KBASE(I)).LT.0.8 E 0) THEN
              CR1 = CR1 * (RHRH(I,KBASE(I))-CRHRH) * RCLAP
            ENDIF
C --- FOR T INVERSION TYPE CLOUD, ADD FLAG VALUE OF 2.0
            CLDARY(I,K) = AMAX1(CLDARY(I,K), CR1) + 2.0 E 0
          ENDIF
 300    CONTINUE
 350  CONTINUE
C ------------------------------------------------------------------
      END IF
C ------------------------------------------------------------------
      IF (ISTRAT.GT.0) THEN
CTUNE
C ------------------------------------------------------------------
C     CALCULATE STRATIFORM CLOUD AND PUT INTO ARRAY 'CLDARY' USING
C       THE CLOUD-REL.HUMIDITY RELATIONSHIP FROM TABLE LOOK-UP..WHERE
C       TABLES OBTAINED USING K.MITCHELL FREQUENCY DISTRIBUTION TUNING
C        (OBSERVATIONS ARE DAILY MEANS FROM US AF RTNEPH).....K.A.C.
C       TABLES CREATED WITHOUT LOWEST 10 PERCENT OF ATMOS.....K.A.C.
C ------------------------------------------------------------------
C  THIS LOOP TO RETRIEVE CLOUD FROM RH REWRITTEN 950113 -MI
      DO KLEV=KLOWB,LEVM2
        DO I=1,IDIMT
          KBASE(I)=0
          BITX(I)=.FALSE.
        ENDDO
        DO KC=MCLD,1,-1
          DO I=1,IDIMT
            IF(PRSLY(I,KLEV).GE.PTOP1(I,KC+1)) KBASE(I)=KC
          ENDDO
        ENDDO
        NX=0
        NHALF=(NBIN+1)/2
        DO I=1,IDIMT
          IF(KBASE(I).LE.0.OR.KLEV.GT.KCUT(I)) THEN
            CLDARY(I,KLEV)=0.
          ELSEIF(RHRH(I,KLEV).LE.RHCLD(I,1,KBASE(I))) THEN
            CLDARY(I,KLEV)=0.
          ELSEIF(RHRH(I,KLEV).GE.RHCLD(I,NBIN,KBASE(I))) THEN
            CLDARY(I,KLEV)=1.
          ELSE
            BITX(I)=.TRUE.
            KSAVE(I)=NHALF
            NX=NX+1
          ENDIF
        ENDDO
        DOWHILE(NX.GT.0)
          NHALF=(NHALF+1)/2
          DO I=1,IDIMT
            IF(BITX(I)) THEN
              CRK=RHRH(I,KLEV)
              CR1=RHCLD(I,KSAVE(I),KBASE(I))
              CR2=RHCLD(I,KSAVE(I)+1,KBASE(I))
              IF(CRK.LE.CR1) THEN
                KSAVE(I)=MAX(KSAVE(I)-NHALF,1)
              ELSEIF(CRK.GT.CR2) THEN
                KSAVE(I)=MIN(KSAVE(I)+NHALF,NBIN-1)
              ELSE
                CLDARY(I,KLEV)=0.01*(KSAVE(I)+(CRK-CR1)/(CR2-CR1))
                BITX(I)=.FALSE.
                NX=NX-1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C.... CLEAN OUT NOT-SUSPECTED MARINE STRATUS REGIONS...
C      CAUSE TUNING PROCEDURE NOT CARRIED OUT DOWN TO LYR3 AND WE
C      GET TOO MUCH LO CLOUD IF WE DON T CLEAN IT OUT..
      DO 831 I=1,IDIMT
       BITM(I) = .TRUE.
  831 CONTINUE
CKAC  DO 833 K=3,LLYR
      DO 833 K=KLOWB,LLYR
       DO 832 I=1,IDIMT
        IF(BITM(I)) THEN
          BITM(I) = PRSLY(I,K).LT.PSTRT
     1         .OR. SLMSK(I).GT.0.0 .OR. DTHDP(I,K).GT.CLAPKC.OR.
CKC  2        CLDARY(I,K+1).GT.0.005 E 0.OR.CLDARY(I,K+2).GT.0.005 E 0
CKAC 2        RHRH (I,K+1).GT.0.80 E 0.OR.RHRH (I,K+2).GT.0.80 E 0
     2        RHRH (I,K+1).GT.0.60 E 0.OR.RHRH (I,K+2).GT.0.60 E 0
          KBASE(I) = K
        ENDIF
  832  CONTINUE
  833 CONTINUE
      DO 835 K=1,LLYR
       DO 834 I=1,IDIMT
        IF(BITM(I)) CLDARY(I,K) = 0.0 E 0
  834  CONTINUE
  835 CONTINUE
C ------------------------------------------------------------------
C     SPECIAL TREATMENT ON LOW CLOUDS
C ------------------------------------------------------------------
      DVVCLD = VVCLD(1) - VVCLD(2)
C
CKAC  DO 950 K=3,KLOW
      DO 950 K=KLOWB,KLOW
C
        DO 904 I=1,IDIMT
          OMEG(I) = 10.0 E 0 * VVEL(I,K)
          CL1 (I) = 0.0 E 0
 904    CONTINUE
CYH94   IF (IVVA .LT. 1) GO TO 920
        IF (IVVA .LE. 0) GO TO 920
C --- VERTICAL VELOCITY ADJUSTMENT ON LOW CLOUDS
        BIT1 = .FALSE.
        DO 906 I=1,IDIMT
          BITX(I) = PRSLY(I,K).GE.PTOP1(I,2) .AND. CLDARY(I,K).GT.0.0 E 
     10
          BIT1 = BIT1 .OR. BITX(I)
 906    CONTINUE
        IF (.NOT. BIT1) GO TO 920
        IF(K.GT.LLYR) THEN
          DO 910 I=1,IDIMT
            IF (BITX(I)) THEN
              IF(OMEG(I).GE.VVCLD(1)) THEN
                CLDARY(I,K) = 0.0 E 0
              ELSE IF(OMEG(I).GT.VVCLD(2)) THEN
                CR1 = (VVCLD(1) - OMEG(I)) / DVVCLD
C               CLDARY(I,K) = CLDARY(I,K) * CR1
                CLDARY(I,K) = CLDARY(I,K) * SQRT(CR1)
              ENDIF
            ENDIF
 910      CONTINUE
        ELSE
          DO 915 I=1,IDIMT
C....    NO VVEL FILTER FOR MARINE STRATUS REGION
            IF (BITM(I)) THEN
              IF (BITX(I)) THEN
                IF(OMEG(I).GE.VVCLD(1)) THEN
                  CLDARY(I,K) = 0.0 E 0
                ELSE IF(OMEG(I).GT.VVCLD(2)) THEN
                  CR1 = (VVCLD(1) - OMEG(I)) / DVVCLD
C                 CLDARY(I,K) = CLDARY(I,K) * CR1
                  CLDARY(I,K) = CLDARY(I,K) * SQRT(CR1)
                ENDIF
              ENDIF
            ENDIF
 915      CONTINUE
        ENDIF
C --- T INVERSION RELATED STRATUS CLOUDS
 920    IF (INVR .LT. 1) GO TO 950
        IF (K.GT.LLYR) GO TO 950
        BIT1 = .TRUE.
        DO 930 I=1,IDIMT
          BIT1 = BIT1 .AND. BITM(I)
 930    CONTINUE
        IF (BIT1) GO TO 950
        DO 940 I=1,IDIMT
         IF (.NOT.BITM(I)) THEN
          IF (DTHDP(I,KBASE(I)).GT.CLPSE) THEN
C---   SMOOTH TRANSITION FOR CLOUD WHEN DTHDP BETWEEN
C           CLAPSE AND CLAPSE+DCLPS  (-0.05 AND -0.06)
           CFILTR = 1.0 E 0 - ((CLPSE - DTHDP(I,KBASE(I))) / DCLPS)
           CLDARY(I,K) = CLDARY(I,K)*CFILTR
          END IF
C --- FOR T INVERSION TYPE CLOUD, ADD FLAG VALUE OF 2.0
          CLDARY(I,K) = CLDARY(I,K)+2.0 E 0
         END IF
 940    CONTINUE
 950  CONTINUE
C ------------------------------------------------------------------
      END IF
CTUNE
C ------------------------------------------------------------------
C     ADD CONVECTIVE CLOUD INTO 'CLDARY', NO MERGE AT THIS POINT..
C     TWO TYPES OF CLOUDS ARE SEPARATED BY A FACTOR OF 1.0E+4
C ------------------------------------------------------------------
 360  BIT1 = .FALSE.
CNOT  DO 370 L=1,3
CNOT  DO 370 I=1,IDIMT
CNOT    FACV(I,L) = 1.0 E 0
C370  CONTINUE
      DO 380 I=1,IDIMT
        BITX(I) = CV(I).GT.0.0 E 0 .AND. CVT(I).GE.CVB(I)
        BIT1 = BIT1 .OR. BITX(I)
 380  CONTINUE
      IF (.NOT. BIT1) GO TO 550
      DO 390 I=1,IDIMT
        IF (BITX(I)) THEN
          KCVB(I) = NINT(CVB(I))
          KCVT(I) = MIN(LEVM2, NINT(CVT(I)))
        ELSE
          KCVB(I) = 1
          KCVT(I) = 1
        END IF
 390  CONTINUE
CKAC  DO 450 K=KLOWB,LEVM2
      DO 450 K=KLOWB,LEVM2
        BIT2 = .FALSE.
        DO 400 I=1,IDIMT
          BITY(I) = BITX(I) .AND. KCVB(I).LE.K .AND. KCVT(I).GE.K
          BIT2 = BIT2 .OR. BITY(I)
 400    CONTINUE
        IF (.NOT. BIT2) GO TO 450
        DO 420 I=1,IDIMT
          IF (BITY(I)) CLDARY(I,K) = CLDARY(I,K)
     1               + 10.0 E 0 * AINT(1.0 E 3 * CV(I))
 420    CONTINUE
 450  CONTINUE
C     IF MEAN CVT LAYER HIGHER THAN 400MB ADD ANVIL CIRRUS
      BIT2 = .FALSE.
      DO 460 I=1,IDIMT
        BITZ(I) = BITX(I) .AND. PRSLY(I,KCVT(I)).LE.CVTOP
        BIT2 = BIT2 .OR. BITZ(I)
 460  CONTINUE
      IF (.NOT. BIT2) GO TO 500
      DO 480 I=1,IDIMT
        IF (BITZ(I)) THEN
CKAC      KK = KCVT(I) + 1
          KK = KCVT(I)
          CR1 = AMAX1(0.0 E 0, AMIN1(1.0 E 0, 2.0 E 0*(CV(I)-0.3 E 0)))
C....    GET STRATUS BACK BEFORE DOING ANVIL CALCULATION
          CR2 = AMOD(CLDARY(I,KK),10. E 0)
          CLDARY(I,KK) = CR2 + 10.0 E 0*AINT(1.0 E 3*CR1)
CKAC      CLDARY(I,KK) = CLDARY(I,KK) + 10.0 E 0*AINT(1.0 E 3*CR1)
        END IF
 480  CONTINUE
C -------------------------------------------------------------------
C     SEPARATE CLOUDS INTO 3 PRESSURE DOMAINS (L,M,H).  WITHIN EACH
C     OF THE DOMAINS, ASSUME SEPARATED CLOUD LAYERS ARE RANDOMLY
C     OVERLAPPED AND ADJACENT CLOUD LAYERS ARE MAXIMUM OVERLAPPED.
C     VERTICAL LOCATION OF EACH TYPE OF CLOUD IS DETERMINED BY
C     THE THICKEST CONTINUING CLOUD LAYERS IN THE DOMAIN.
CNOT  DEEP CONVECTIVE CLOUD SPAN MORE THAN 1 DOMAIN WILL BE MULTIPLIED
CNOT  BY A FACTOR FOR EACH OF THE DOMAINS.
C -------------------------------------------------------------------
 500  CONTINUE
CNOT  DO 520 L=1,2
CNOT  DO 520 I=1,IDIMT
CNOT    IF (BITX(I) .AND. PRSLY(I,KCVB(I)).GE.PTOP1(I,L+1)
CNOT 1              .AND. PRSLY(I,KCVT(I)).LT.PTOP1(I,L+1)) THEN
CYH94     FACV(I,L)   = AMAX1(0.30 E 0, 0.80 E 0*FACV(I,L))
CNOT      FACV(I,L)   = AMAX1(0.35 E 0, 0.80 E 0*FACV(I,L))
CYH94     FACV(I,L+1) = 0.30 E 0
CNOT      FACV(I,L+1) = 0.35 E 0
CNOT    END IF
C520  CONTINUE
 550  CONTINUE
C     DO 552 I=1,64,10
C     WRITE(6,551) I,(CLDARY(I,K),K=1,KDIM)
C551  FORMAT(' IN CLDNEW: I=',I3,' CLDARY(I,K)=',8E10.2/10X,10E10.2)
C552  CONTINUE
CYH94 FOVP = 1.0 E 0 / ANINT(FLOAT(KDIM) / 9.0 E 0)
C --- LOOP OVER 3 CLOUD DOMAINS (L,M,H)
      DO 750 L=1,3
C
        DO 580 I=1,IDIMT
          CLD (I,L) = 0.0 E 0
          MTOP(I,L) = 1
          MBOT(I,L) = 1
          CL1 (I) = 0.0 E 0
          CL2 (I) = 0.0 E 0
          KBT1(I) = 1
          KBT2(I) = 1
          KTH1(I) = 0
          KTH2(I) = 0
 580    CONTINUE
C
        DO 700 K=2,LEVM2
          BIT1 = .FALSE.
          DO 600 I=1,IDIMT
            BITX(I) = (PRSLY(I,K).GE.PTOP1(I,L+1)) .AND.
     1        (PRSLY(I,K).LT.PTOP1(I,L)) .AND. (CLDARY(I,K).GT.0.0 E 0)
            BIT1 = BIT1 .OR. BITX(I)
 600      CONTINUE
          IF (.NOT. BIT1) GO TO 700
          DO 630 I=1,IDIMT
            CR1  = AMOD(CLDARY(I,K), 2.0 E 0)
            CR2  = FLOAT(INT(CLDARY(I,K)) / 10) * 1.0 E -3
CNOT        CR3  = (CR1 + CR2 - CR1*CR2) * FACV(I,L)
CNOT        CR3  = (CR1 + CR2 - CR1*CR2)
            IF (BITX(I)) THEN
              IF(KTH2(I).LE.0) THEN
C --- KTH2 LE 0 : 1ST CLD LAYER.
                KBT2(I) = K
                KTH2(I) = 1
              ELSE
C --- KTH2 GT 0 : CONSECUTIVE CLD LAYER.
                KTH2(I) = KTH2(I) + 1
              ENDIF
CNOT          CL2 (I) = AMAX1(CL2(I), CR3)
C ---  PHYSICAL CLOUD AS SEEN BY RADIATION..CONV TAKES PRECEDENCE
C ---    EXCEPT ANVIL CIRRUS NOT RANDOM OVERLAPPED WITH CV TOWER AS
C ...      IN RADIATION CODE(SO HI MAY BE SLIGHT UNDERESTIMATE)....
              IF (CR2.GT.0.0 E 0) THEN
               CL2 (I) = AMAX1(CL2(I), CR2)
              ELSE
               CL2 (I) = AMAX1(CL2(I), CR1)
              END IF
            ENDIF
 630      CONTINUE
          BIT2 = .FALSE.
C....  BITY=TRUE IF NEXT LYR=CLEAR OR WE CHANGE CLOUD DOMAINS..
          DO 640 I=1,IDIMT
            BITY(I) = BITX(I) .AND. (CLDARY(I,K+1).LE.0.0 E 0
     1                         .OR.  PRSLY(I,K+1).LT.PTOP1(I,L+1) )
            BIT2 = BIT2 .OR. BITY(I)
 640      CONTINUE
          IF (.NOT. BIT2) GO TO 700
C --- AT THE DOMAIN BOUNDARY OR SEPARATED CLD LYRS, RANDOM OVERLAP.
C     CHOOSE THE THICKEST OR THE LARGEST FRACTION AMT AS THE CLD
C     LAYER IN THAT DOMAIN
          DO 650 I=1,IDIMT
            IF (BITY(I)) THEN
              IF (CL1(I).GT.0.0 E 0) THEN
                KBT1(I) = INT( (CL1(I)*KBT1(I) + CL2(I)*KBT2(I))
     1                       / (CL1(I) + CL2(I)) )
                KTH1(I) = NINT( (CL1(I)*KTH1(I) + CL2(I)*KTH2(I))
     1                        / (CL1(I) + CL2(I)) ) + 1
                CL1 (I) = CL1(I) + CL2(I) - CL1(I)*CL2(I)
              ELSE
                KBT1(I) = KBT2(I)
                KTH1(I) = KTH2(I)
                CL1 (I) = CL2 (I)
              ENDIF
              KBT2(I) = 1
              KTH2(I) = 0
              CL2 (I) = 0.0 E 0
            ENDIF
 650      CONTINUE
 700    CONTINUE
C --- FINISH ONE DOMAIN, SAVE EFFECTIVE CLOUDS
        DO 720 I=1,IDIMT
          CLD(I,L) =  CL1(I)
          MTOP(I,L) = MAX(KBT1(I), KBT1(I)+KTH1(I)-1)
          MBOT(I,L) = KBT1(I)
 720    CONTINUE
 750  CONTINUE
C....  CALCULATE TOTAL CLOUD FROM THE MULTI-LYR CLOUD ARRAY
C  .......IN A MANNER AS SEEN BY THE RADIATION CODE........
C      WHERE, MAX OVERLAP IS USED FOR VERTICALLY ADJACENT CLOUD LAYERS
C      .. A CLEAR LAYER SEPARATES TWO CONTIGUOUSLY LAYERED CLOUD TYPES.
C      WHERE, FOR CONVECTION ANY ANVIL IS CONSIDERED A SEPARATE
C         RANDOMLY OVERLAPPED CLOUD..
C      ILOW=0,1 IF NO,YES PRECEEDING MODEL LAYER WAS CLOUDY..
C      CLOW CONTAINS THE CLOUDINESS OF PRECEEDING SEPARATE LAYERED CLD
      DO 780 I=1,IDIMT
       CLD(I,4) = 0. E 0
       ICVEC = 0
       ILOW = 0
       CLOW = 0. E 0
       DO 780 K=1,KDIM
        CCLDY = AMOD(CLDARY(I,K), 2.0 E 0)
        CCVEC = FLOAT(INT(CLDARY(I,K)) / 10) * 1.0 E -3
        IF (CCVEC.GT.0. E 0) THEN
         CCLDY = CCVEC
         ICVEC = 1
        END IF
        IF (CCLDY.GT.0. E 0) THEN
         IF (ILOW.EQ.0) THEN
          CLOW = CCLDY
          ILOW = 1
         ELSE
          IF (ICVEC.GT.0) THEN
           IF (CCLDY.NE.CLOW) THEN
C...  IF CONVECTIVE AND AN ADJACENT LYR=STRATIFORM (IE CCLDY CHANGES),
C.      THEN RANDOM OVERLAP THE PRECEEDING CLOUD TOWER...
            CLD(I,4) = CLD(I,4) + (1.-CLD(I,4))*CLOW
            CLOW = CCLDY
           END IF
          ELSE
C...  MAX OVERLAP FOR NON CONVECTIVE ADJACENT CLD LAYERS...
           CLOW = MAX(CCLDY,CLOW)
          END IF
         END IF
        ELSE
         IF (ILOW.EQ.1) THEN
C...  IF THIS IS FIRST CLEAR LAYER IN A GAP BETWIXT CLDLYRS, THEN
C.      RANDOM OVERLAP THE PRECEEDING CLOUDS WITH THE ONES BELOW..
          CLD(I,4) = CLD(I,4) + (1.-CLD(I,4))*CLOW
          ILOW = 0
         END IF
        END IF
 780  CONTINUE
      RETURN
      END
