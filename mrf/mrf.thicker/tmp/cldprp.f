      SUBROUTINE CLDPRP(IDIMT,IDIMS,PS,Q,T,SI,CLDARY,IBEG,IPTS,XLATRD,
     1                  CAMT,KTOP,KBTM,NCLDS,EMIS,RRCL,TTCL)
CFPP$ NOCONCUR R
C---------------------------------------------------------------------
C     FEB., 1993 - Y.H.
C        CLOUD RADIATIVE PROPERTIES CALCULATIONS AFTER DAVIS (1982)
C        AND HARSHVARDHAN ET AL. (1987).
C--------------------------------------------------------------------
C     INPUT VARIABLES:
C        PS(I)         - SURFACE PRESSURE (CB)
C        Q (I,K)       - SPECIFIC HUMIDITY, K=1 IS TOP LAYER (KG/KG)
C        T (I,K)       - ABSOLUTE TEMPERATURE, K=1 IS TOP LAYER (K)
C        SI(K)         - MDL SIGMA INTERFACES, K=1 IS THE SURFACE
C        CLDARY(I,K)   - CLOUD ARRAY CONTAINS COMPRESSED CLOUD
C                        FRACTIONS OF 3 TYPES (STRATIFORM, CONV
C                        AND STRATUS), K=1 IS THE MDL SFC LAYER
C        IBEG,IPTS     - INDICES FOR THE BEGINNIG NO. AND THE
C                        TOTAL NO. OF ARRAY ELEMENTS TO BE PROCESSED
C ----- MODIFY XLATRD TO GENERAL FOR REGIONAL AND GLOBAL (H.-M.H. JUANGK
C********XLATRD        - CURRENT LATITUDE IN RADIANS (1ST DATA PT)
C********                 FOR MODELS WITH DIFF LAT AT EACH PT, NEED TO
C********                 USE THE LAT OF EACH POINT....CAREFUL.....
C    OUTPUT VARIABLES:
C    OUTPUT VARIABLES:
C        CAMT(I,K)     - CLOUD FRACTIONS, K=1 IS THE SFC, K=2 IS THE
C                        LOWEST CLOUD LAYER, AND SO ON
C        KTOP,KBTM(I,K)- CLOUD TOP AND BOTTOM INDECES, KTOP AND
C                        KBTM VALUES FROM 1 TO L MODEL LAYERS,
C                        WITH VALUE OF 1 BEING THE TOP MDL LAYER
C        NCLDS(I)      - NO. OF SEPARATED CLOUD LAYERS IN A COLUMN
C        EMIS(I,K)     - CLOUD EMISSIVITY
C  ***   ITYP(I,K)     - TYPE OF CLOUDS, ITYP=1, 2 AND 3 ARE FOR
C                        THE RH, STRATUS, AND CONV TYPES
C        RRCL,TTCL(I,NB,K)
C                      - CLOUD REFLECTTANCES, AND TRANSMITANCES FOR
C                        SW SPECTRAL BANDS
C
C--------------------------------------------------------------------
C
C     PARAMETER SETTINGS FOR THE LONGWAVE AND SHORTWAVE RADIATION CODE:
C          IMAX   =  NO. POINTS ALONG THE LAT. CIRCLE USED IN CALCS.
C          L      =  NO. VERTICAL LEVELS (ALSO LAYERS) IN MODEL
C***NOTE: THE USER NORMALLY WILL MODIFY ONLY THE IMAX AND L PARAMETERS
C          NBLW   =  NO. FREQ. BANDS FOR APPROX COMPUTATIONS. SEE
C                      BANDTA FOR DEFINITION
C          NBLX   =  NO. FREQ BANDS FOR APPROX CTS COMPUTATIONS
C          NBLY   =  NO. FREQ. BANDS FOR EXACT CTS COMPUTATIONS. SEE
C                      BDCOMB FOR DEFINITION
C          INLTE  =  NO. LEVELS USED FOR NLTE CALCS.
C          NNLTE  =  INDEX NO. OF FREQ. BAND IN NLTE CALCS.
C          NB,KO2 ARE SHORTWAVE PARAMETERS; OTHER QUANTITIES ARE DERIVED
C                    FROM THE ABOVE PARAMETERS.
      PARAMETER (L= 28 )
      PARAMETER (IMAX= 64 )
      PARAMETER (IMBX=IMAX+1)
      PARAMETER (NBLW=163,NBLX=47,NBLY=15)
      PARAMETER (NBLM=NBLY-1)
      PARAMETER (LP1=L+1,LP2=L+2,LP3=L+3)
      PARAMETER (LM1=L-1,LM2=L-2,LM3=L-3)
      PARAMETER (LL=2*L,LLP1=LL+1,LLP2=LL+2,LLP3=LL+3)
      PARAMETER (LLM1=LL-1,LLM2=LL-2,LLM3=LL-3)
      PARAMETER (LP1M=LP1*LP1,LP1M1=LP1M-1)
      PARAMETER (LP1V=LP1*(1+2*L/2))
      PARAMETER (LP121=LP1*NBLY)
      PARAMETER (LL3P=3*L+2)
      PARAMETER (NB=12)
      PARAMETER (INLTE=3,INLTEP=INLTE+1,NNLTE=56)
      PARAMETER (NB1=NB-1)
      PARAMETER (KO2=12)
      PARAMETER (KO21=KO2+1,KO2M=KO2-1)
                      D I M E N S I O N
     1  PS  (IDIMS),     SI  (LP1),       CLDARY(IDIMT,L)
     2, Q   (IMBX,L),      T   (IMBX,LP1),    CAMT(IMBX,LP1)
     3, KTOP(IMBX,LP1),    KBTM(IMBX,LP1),    EMIS(IMBX,LP1)
     4, ITYP(IMBX,LP1),    RRCL(IMBX,NB,LP1), TTCL(IMBX,NB,LP1)
     5, NCLDS(IMAX),     XLATRD(IDIMT)
C
      COMMON /COMCD1/ ROCP,PTOPC(4,2),CVTOP,VVCLD(2),CLAPSE
     1,               CRHRH,KLOWT,KLOWB,PSTRT
     2,               LLYR,CLAPKC,DCLPS,CLPSE
C
C --- ABCFF(NB) : NB BANDS ABSORPTION COEFF FOR WATER VAPOR
C
      COMMON /SWRSAV/ ABCFF(NB),PWTS(NB),CFCO2,CFO3,REFLO3,RRAYAV
C
C ---  WORKSPACE ---
                      D I M E N S I O N
     1  PRSLV(IMBX,LP1),MTYP (IMAX),      XAMT (IMAX)
     2, KCLD (IMAX),    MBTM (IMAX),      CL1  (IMAX)
     3, CL2  (IMAX),    QSUM (IMAX)
     4, MIDK (IMAX),    DELP (IMAX),      TCLD (IMAX)
     5, TAUC (IMAX),    PTOPD(IMAX)
                    E Q U I V A L E N C E
     1  (TCLD, CL1), (DELP, CL2), (TAUC, MBTM), (XAMT, PTOPD)
                        L O G I C A L
     1  BITX(IMAX),   BITW(IMAX),  BIT1,   BIT2
C===>    BEGIN HERE ................................................
      DO 10 K=2,LP1
      DO 10 I=1,IPTS
        IR = I + IBEG - 1
        PRSLV(I,K) = PS(IR) * SI(LP1-K+1) * 10.0 E 0
  10  CONTINUE
      DO 20 I=1,IMAX
        PRSLV(I,1) = 0.001 E 0
        KCLD(I) = 2
        MBTM(I) = 1
        MTYP(I) = 0
        XAMT(I) = 0.0 E 0
        ITYP(I,1) = 0
        CAMT(I,1) = 1.0 E 0
        KTOP(I,1) = LP1
        KBTM(I,1) = LP1
        EMIS(I,1) = 1.0 E 0
  20  CONTINUE
      DO 30 K=2,LP1
      DO 30 I=1,IMAX
        ITYP(I,K) = 0
        CAMT(I,K) = 0.0 E 0
        KTOP(I,K) = 1
        KBTM(I,K) = 1
        EMIS(I,K) = 0.0 E 0
  30  CONTINUE
      DO 40 K=1,LP1
      DO 40 J=1,NB
      DO 40 I=1,IMAX
        RRCL(I,J,K) = 0.0 E 0
        TTCL(I,J,K) = 1.0 E 0
  40  CONTINUE
C
C --- LOOP OVER MDL LAYERS
C
      DO 200 K=2,L
C
        BIT1 = .FALSE.
        DO 60 I=1,IPTS
          IR = I + IBEG - 1
          BITX(I) = CLDARY(IR,K).GT.0.0 E 0
          BIT1 = BIT1 .OR. BITX(I)
  60    CONTINUE
        IF (.NOT. BIT1) GO TO 200
C --- DECOMPRESS CLOUD ARRAY
        DO 70 I=1,IPTS
          CL1(I) = 0.0 E 0
          CL2(I) = 0.0 E 0
          BITW(I) = BITX(I)
  70    CONTINUE
        DO 80 I=1,IPTS
          IF (BITX(I)) THEN
            IR = I + IBEG - 1
            CL1(I) = AMOD(CLDARY(IR,K), 2.0 E 0)
            CLTEMP = AMOD(CLDARY(IR,K), 10.0 E 0)
            CL2(I) = 1.0 E -4 * (CLDARY(IR,K) - CLTEMP)
C --- MTYP=1,2 FOR RH+STRATUS, AND CONV CLOUD TYPES
            IF (CL2(I) .GT. 0.0 E 0) THEN
              MTYP(I) = 2
            ELSE
              MTYP(I) = 1
            END IF
          END IF
  80    CONTINUE
        IF(K.LT.L) THEN
          DO 100 I=1,IPTS
            IR = I + IBEG - 1
            IF(BITW(I)) THEN
              BITW(I) = CLDARY(IR,K+1).LE.0.0 E 0
            ENDIF
 100      CONTINUE
        ENDIF
        BIT2 = .FALSE.
        DO 110 I=1,IPTS
          BIT2 = BIT2 .OR. BITW(I)
          IF (BITX(I)) THEN
            IF(ITYP(I,KCLD(I)).EQ.0) THEN
              ITYP(I,KCLD(I)) = MTYP(I)
              XAMT(I) = CL1(I)
              IF (MTYP(I) .EQ. 2) XAMT(I) = CL2(I)
              MBTM(I) = K
            ELSE IF(ITYP(I,KCLD(I)).NE.MTYP(I)) THEN
              CAMT(I,KCLD(I)) = XAMT(I)
              KTOP(I,KCLD(I)) = LP1 - (K - 1)
              KBTM(I,KCLD(I)) = LP1 - MBTM(I)
              ITYP(I,KCLD(I)+1) = MTYP(I)
              MBTM(I) = K
              XAMT(I) = CL1(I)
              IF (MTYP(I).EQ.2) XAMT(I) = CL2(I)
              KCLD(I) = KCLD(I) + 1
            ELSE IF(MTYP(I).EQ.1) THEN
              XAMT(I) = AMAX1(XAMT(I), CL1(I))
            ELSE IF(MTYP(I).EQ.2) THEN
              XAMT(I) = AMAX1(XAMT(I), CL2(I))
            ENDIF
          END IF
 110    CONTINUE
        IF (.NOT. BIT2) GO TO 200
        DO 160 I=1,IPTS
          IF (BITW(I)) THEN
            CAMT(I,KCLD(I)) = XAMT(I)
            KTOP(I,KCLD(I)) = LP1 - K
            KBTM(I,KCLD(I)) = LP1 - MBTM(I)
            KCLD(I) = KCLD(I) + 1
            MTYP(I) = 0
            MBTM(I) = 1
            XAMT(I) = 0.0 E 0
          END IF
 160    CONTINUE
C
 200  CONTINUE
C --- RECORD NUM OF CLD LYRS AND FIND MAX NUM OF CLD LYRS
      MCLDS = 0
      DO 220 I=1,IPTS
        NCLDS(I) = KCLD(I) - 2
        MCLDS = MAX(MCLDS, NCLDS(I))
 220  CONTINUE
C     WRITE(6,231) MCLDS
C231  FORMAT(' IN CLDPRP: MAXCLDS =',I4)
C     IF (MCLDS .EQ. 0) RETURN
C
C --- ESTIMATE CLOUD OPTICAL PROPERTIES FROM T AND Q
C ---  THE 240-LOOP FLAGS MID/HI CLD USING PRESSURE DOMAIN BDRY, PTOP
C
C
      DO 500 NC=2,MCLDS+1
C
        DO 230 I=1,IPTS
          TAUC(I) = 0.0 E 0
          QSUM(I) = 0.0 E 0
          BITX(I) = CAMT(I,NC) .GT. 0.0 E 0
 230    CONTINUE
C...  FIND TOP PRESSURE FOR MID CLOUD (3) DOMAIN=FUNCTION OF LATITUDE
        MINKTP=LP1
        MAXKBT=0
        DO 240 I=1,IPTS
          IR = I + IBEG - 1
          FAC = AMAX1(0.0 E 0, 4.0 E 0*ABS(XLATRD(IR))/ 3.141593E+0 -1.0
     1 E 0)
          PTOPD(I) = PTOPC(3,1) + (PTOPC(3,2)-PTOPC(3,1)) * FAC
          MINKTP = MIN(MINKTP,KTOP(I,NC))
          MAXKBT = MAX(MAXKBT,KBTM(I,NC))
 240    CONTINUE
C --- CALC CLD THICKNESS DELP, TOTAL H2O PASS, AND MEAN TEMP (CELSIUS)
        DO 260 KK=MINKTP,MAXKBT
        DO 260 I=1,IPTS
          IF (KK.GE.KTOP(I,NC) .AND. KK.LE.KBTM(I,NC) .AND.BITX(I)) THEN
            DELP(I) = PRSLV(I,KK+1) - PRSLV(I,KK)
            TCLD(I) = T(I,KK) - 273.16 E 0
            QSUM(I) = QSUM(I) + Q(I,KK) * DELP(I)
     1              * (PRSLV(I,KK+1) + PRSLV(I,KK))
     2              / (120.1612 E 0 * SQRT(T(I,KK)))
            IF(PRSLV(I,KTOP(I,NC)+1).LE.PTOPD(I) .OR.
     1         ITYP(I,NC).EQ.2) THEN
C....     TOPS ARE HIGH OR CONVEC CLOUDS
              IF (TCLD(I) .LE. -10.0 E 0) THEN
                TAUC(I) = TAUC(I) + DELP(I)
     1                  * AMAX1(0.1 E -3, 2.00 E -6*(TCLD(I)+82.5 E 0)**
     12)
              ELSE
                TAUC(I) = TAUC(I) + DELP(I)
     1                  * AMIN1(0.08 E 0, 6.949 E -3*TCLD(I)+0.10 E 0)
              END IF
            ELSE
C....     TOPS ARE MID AND LOW NON-CONVEC CLOUDS
              IF (TCLD(I) .LE. -20.0 E 0) THEN
                TAUC(I) = TAUC(I) + DELP(I)
     1                  * AMAX1(0.1 E -3,2.56 E -5*(TCLD(I)+82.5 E 0)**2
     1)
              ELSE
                TAUC(I) = TAUC(I) + DELP(I) * 0.10 E 0
              END IF
            END IF
          END IF
 260    CONTINUE
C --- CALC CLD EMIS
        DO 320 I=1,IPTS
          IF (BITX(I))
     1      EMIS(I,NC) = 1.0 E 0 - EXP(-0.75 E 0*TAUC(I))
CYH       IF (BITX(I) .AND. ITYP(I,NC).EQ.2) EMIS(I,NC) = 1.0 E 0
 320    CONTINUE
        DO 420 NBAND=1,NB
          DO 400 I=1,IPTS
            IF (BITX(I)) THEN
              DD = TAUC(I) / (TAUC(I)+ABCFF(NBAND)*QSUM(I))
              EE = 1.0 E 0 - DD
              FF = 1.0 E 0 - DD*0.85 E 0
              AA = AMIN1( 50.0 E 0,SQRT(3.0 E 0*EE*FF)*TAUC(I) )
              AA = EXP(-AA)
              BB = FF / EE
              CC = SQRT(BB)
              DD = (CC + 1.0 E 0)*(CC + 1.0 E 0) -
     1             (CC - 1.0 E 0)*(CC - 1.0 E 0) * AA*AA
              RRCL(I,NBAND,NC) = AMAX1(0.1 E -5,
     1          (BB-1.0 E 0)*(1.0 E 0-AA*AA) / DD )
              TTCL(I,NBAND,NC) = AMAX1(0.1 E -5,4.0 E 0 * CC * AA / DD )
            END IF
 400      CONTINUE
 420    CONTINUE
C
 500  CONTINUE
      IF (IPTS .EQ. IMAX) GO TO 565
      IPTS1 = IPTS + 1
      DO 520 I=IPTS1,IMAX
        NCLDS(I) = NCLDS(IPTS)
 520  CONTINUE
      DO 540 K=1,LP1
      DO 540 I=IPTS1,IMAX
        CAMT(I,K) = CAMT(IPTS,K)
        KTOP(I,K) = KTOP(IPTS,K)
        KBTM(I,K) = KBTM(IPTS,K)
        EMIS(I,K) = EMIS(IPTS,K)
 540  CONTINUE
      DO 560 K=1,LP1
      DO 560 N=1,NB
      DO 560 I=IPTS1,IMAX
        RRCL(I,N,K) = RRCL(IPTS,N,K)
        TTCL(I,N,K) = TTCL(IPTS,N,K)
 560  CONTINUE
 565  CONTINUE
C     DO 586 K=2,MCLDS+1
C       WRITE(6,581) K-1
C581    FORMAT(' IN CLDPRP: K=',I3,' CLD,EMIS,TOP,BOT:')
C       WRITE(6,582) (CAMT(I,K),EMIS(I,K),KTOP(I,K),KBTM(I,K),I=1,IMAX)
C582    FORMAT(6(2F6.3,2I4))
C       DO 585 N=1,NB
C         WRITE(6,583) N
C583      FORMAT(' NBAND =',I3,' TTCL,RRCL:')
C         WRITE(6,584) (TTCL(I,N,K),RRCL(I,N,K),I=1,IMAX)
C584      FORMAT(6(2E10.3))
C585    CONTINUE
C586  CONTINUE
C
      RETURN
      END
