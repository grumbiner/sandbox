      SUBROUTINE EKMAH(FH, T, LRHS, KG, LWDN, SWDN, QS)
C=======================================================================
C  PROGRAMMED BY:
C     -C.KOCH                     UNI, BONN                         1986
C  MODIFIED EXTENSIVELY BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  Extension:
C      R. Grumbine                NMC, Camp Springs                 1994
C  PURPOSE:
C     -CALCULATES GROWTH RATES FOR THE ICE COVERED PART OF A GRID CELL
C       WITH ABL-MODEL ACC.TO KOCH(88)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT AND TURNING
C       ANGLE TO BE USED IN SUBROUTINE RELCON (NOTE THAT BY THIS
C       CONFIGURATION THE RESULTANT STRESS IS STAGGERED IN TIME,
C       I.E. THOSE RESULTS OF THE PRESENT ROUTINE WHICH ARE RELEVANT
C       FOR THE DYNAMICS WILL FIRST BE USED AT THE NEXT TIME STEP)
C  METHOD:
C     -HEAT BUDGET EQUATION OVER ICE (OPTIONALLY FOR SEVEN THICKNESS
C       CATEGORIES)
C     -MONIN-OBUKHOV THEORY FOR SURFACE LAYER (DERIVED QUANTITIES ONLY)
C     -ROSSBY NUMBER SIMILARITY THEORY FOR EKMAN LAYER (ABL)
C     -THE STABILITY PARAMETER (WMUE), THE FRICTION VELOCITY (UST) AND
C       THE ICE OR SNOW SURFACE TEMPERATURE ARE SOLVED PER ITERATION
C       (REGULA FALSI)
C     -IN ORDER TO ACCELERATE THE INTEGRATION, ONLY THOSE GRID CELLS
C       WHICH DID NOT PASS THE SOLUTION CRITERIA ARE SELECTED FOR
C       FURTHER ITERATIONS; THIS METHOD REQUIRES AN INTERMEDIATE
C       STORAGE OF THE VARIABLES INTO A ONE-DIMENSIONAL ARRAY
C  OPTIONS:
C     -THE FRICTION VELOCITY CAN BE CALCULATED VIA THE RESISTANCE LAWS
C       OF THE EKMAN-LAYER (SURFWIN=0) OR VIA THE MONIN-OBUKHOV THEORY
C       (SURFWIN=1)
C  INTERFACE:
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE IN CELSIUS
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C     -LWDN: Downwelling longwave radiation
C     -SWDN: Downwelling shortwave radiation
C     -QS:   Mixed layer salinity
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -STAB:   CALCULATION OF STABILITY FUNCTIONS FOR SURFACE LAYER
C     -RESIST: CALCULATION OF STABILITY FUNCTIONS FOR EKMAN LAYER
C     -tfreez: Compute freezing point as a function of salinity
C     -albedo: Compute sea ice albedo.
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/GEO/PI,RAD
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/STP/TX,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M), 
     1  BETA(0:L,0:M), TAUX (L,M), TAUY (L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD (0:L,0:M), ACL(0:L,0:M), 
     1  PA(0:L,0:M), UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M), WRK(0:L,0:M,2), ALB(0:L,0:M), 
     1  TRK(0:L,0:M), A2(0:L,0:M), FLAI(LMDP), FAKTS(LMDP), QG(LMDP),
     2  THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -HICE:  EFFECTIVE ICE THICKNESS (OPTIONALLY FOR SEVEN CATEGORIES)
C     -WRK:   DUMMY ARRAYS
C     -ALB:   ALBEDO
C     -TRK:   DUMMY ARRAY
C     -A2:    FLAG FOR SNOW CONDITIONS
C     -FLAI:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    DUMMY ARRAY
C     -THETG: POTENTIAL AIR TEMPERATURE
C=======================================================================
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP), DELT(LMDP),
     1  SUM1(LMDP), SUM2(LMDP), FAKT1(LMDP), QAL(LMDP)
C=======================================================================
C     -FH:    GROWTH RATE IN METERS OF ICE
C     -T:     SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA:  SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI:  SATURATION VAPOR PRESSURE OVER ICE
C     -DELT:  VERTICAL TEMPERATURE GRADIENT
C     -SUM1:  LOG OF MODIFIED SURFACE ROSSBY NUMBER
C     -SUM2:  VERTICAL SPECIFIC HUMIDITY GRADIENT FOR VIRTUAL TEMP.GRAD.
C     -FAKT1: FACTOR FOR CALCULATION OF STABILITY PARAMETER
C     -QAL:   FACTOR FOR RESISTANCE LAWS OF BAROTROPIC EKMAN LAYER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP), 
     2  ALB1(LMDP), A21(LMDP), HICE1(LMDP), UST(LMDP), CD1(LMDP), 
     3  WMUE(LMDP), FM1(LMDP), SINBET1(LMDP), COSBET1(LMDP), 
     4  BETA1(LMDP), TMPL(LMDP), HSNOW1(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL QS(0:L, 0:M), TB1(LMDP), albedo, tfreez
C=======================================================================
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), UPAST(LMDP),
     1TMYUS1(0:L,0:M), TMYUS(LMDP), TMUE(LMDP), TUST(LMDP), PAST(LMDP)
     2,TT(LMDP), TT1(0:L,0:M), TMUE1(0:L,0:M), TUST1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
C  FOR SINGLE ITERATION LOOPS:
      IMAX=10
C  FOR OVERALL ITERATION LOOP:
      IWMAX=50
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
      ITERW=0
C-----------------------------------------------------------------------
C  START OF OVERALL ITERATION PROCEDURE
C-----------------------------------------------------------------------
   88 CONTINUE
      K=0
C-----------------------------------------------------------------------
C  SELECT GRID CELLS FOR FURTHER ITERATIONS AND BUILD UP ONE-DIM. ARRAY
C-----------------------------------------------------------------------
      DO 1000 I = 1, LMDP
        HSNOW1(I) = 0.
 1000 CONTINUE
      DO 82 J=1,MM
      DO 82 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 82
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       TB1(K) = tfreez(QS(I,J)) + TMELT
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
CD       SWDN1(K) = SWDN(I,J) * (1. - albedo(T(I,J), TAIR(I,J), 
CD     1                           HSN(I,J,LRHS), H(I,J, LRHS) ) )
       SWDN1(K) = SWDN(I,J) * (1. - ALB1(K) )
       FM1(K)=FM(I,J)
       IF (ITERW.EQ.0) GOTO 82
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   82 CONTINUE
      IF (K.EQ.0) GOTO 91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      DO 31 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
       DELT(N)=THETG(N)-T1(N)
       IF (ITERW.GT.0) GOTO 31
       UST(N)=0.1
       WMUE(N)=SIGN(50.,DELT(N))
       TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/UST(N)
   31 CONTINUE
C-----------------------------------------------------------------------
C  REPEAT THE SINGLE ITERATION PROCEDURES TWICE
C-----------------------------------------------------------------------
      DO 7 ITERM=1,3
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE SURFACE TEMPERATURE
C-----------------------------------------------------------------------
       CALL RESIST(WMUE,K, AF, BF, CF)
       CALL STAB(TMPL,ZOI,K, PH1, PH2)
       CALL VAPOR(T1,ESTI,2,K)
       CALL VAPOR(T1,ESTA,1,K)
       DO 33 N=1,K
        FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
        FAKT1(N)=FAKTH*UST(N)*FAKT
        QAL(N)=PH1(N)*FAKT
        STP(N)=T1(N)
        EA=MAX(0.,ESTA(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/EPSI)
        TA1(N)=MAX(200.,STP(N)*(1.-QAL(N))+QAL(N)*THETG(N))
        FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1        -FAKT1(N)*(THETG(N)-STP(N)+SUBL/CPAIR*(QG(N)-EPSI
     2        /PA1(N)*ESTI(N)))+(STP(N)-TB1(N))/HICE1(N)*CON
        T1(N)=T1(N)+1.
   33  CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION FOR THE SURFACE TEMPERATURE
C-----------------------------------------------------------------------
       DO 3 ITER=1,IMAX
        CALL VAPOR(T1,ESTI,2,K)
        CALL VAPOR(T1,ESTA,1,K)
        DO 34 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=T1(N)
         EA=MAX(0.,ESTA(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
         TA1(N)=MAX(200.,STP(N)*(1.-QAL(N))+QAL(N)*THETG(N))
         FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1         -FAKT1(N)*(THETG(N)-STP(N)+SUBL/CPAIR*(QG(N)-0.623
     2         /PA1(N)*ESTI(N)))+(STP(N)-TB1(N))/HICE1(N)*CON
         FDIFF=FP(N)-FPP(N)
         T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1         MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=T1(N)-STP(N)
         TT(N)=SIGN(1.,.01-ABS(DIFF))
   34   CONTINUE
    3  CONTINUE
C-----------------------------------------------------------------------
C  MAKE SURE THAT THE SURFACE TEMP. DOES NOT EXCEED THE MELTING POINT
C-----------------------------------------------------------------------
       DO 83 N=1,K
        FLAG=.5*(1.+SIGN(1.,T1(N)-TMELT))
        T1(N)=T1(N)*(1.-FLAG)+TMELT*FLAG
   83  CONTINUE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       CALL VAPOR(T1,ESTI,2,K)
       DO 42 N=1,K
        PAST(N)=WMUE(N)
        DELT(N)=THETG(N)-T1(N)
        SUM1(N)=LOG( UST(N)/ABS(FM1(N))/ZOI)
        SUM2(N)=0.61*(QG(N)-0.623/PA1(N)*ESTI(N))
        FAKT1(N)=GRAV*0.064/ABS(FM1(N))/UST(N)
        STP(N)=WMUE(N)
        TMPL(N)=5.*ABS(FM1(N))*STP(N)/UST(N)
   42  CONTINUE
       CALL STAB(TMPL,ZOI,K, PH1, PH2)
       CALL RESIST(STP,K, AF, BF, CF)
       DO 43 N=1,K
        FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
        SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
        FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1        /(T1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
        WMUE(N)=WMUE(N)*1.5
   43  CONTINUE
C-----------------------------------------------------------------------
C  START THE ITERATION FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       DO 4 ITER=1,IMAX
        DO 44 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=WMUE(N)
         TMPL(N)=5.*ABS(FM1(N))*STP(N)/ UST(N)
   44   CONTINUE
        CALL STAB(TMPL,ZOI,K, PH1, PH2)
        CALL RESIST(STP,K, AF, BF, CF)
        DO 45 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(T1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TMUE(N)=SIGN(1.,10.-ABS(DIFF))
   45   CONTINUE
    4  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF (SURFWIN.EQ.1)THEN
        DO 22 N=1,K
   22   UST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K, AF, BF, CF)
        DO 52 N=1,K
         UPAST(N)=UST(N)
         STP(N)=UST(N)
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
         FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
         STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOI)
         UST(N)=UST(N)*0.8
   52   CONTINUE
C-----------------------------------------------------------------------
C  START ITERATION FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        DO 5 ITER=1,IMAX+1
         DO 53 N=1,K
          FPP(N)=FP(N)
          STPP(N)=STP(N)
          STP(N)=UST(N)
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
          FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
          STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOI)
          FDIFF=FP(N)-FPP(N)
          FLAG=.5*(1.+SIGN(1.,ABS(FDIFF)-1.E-10))
          FDIFF=FDIFF*FLAG+.1*(1.-FLAG)
          UST(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
          DIFF= UST(N)-STP(N)
          TUST(N)=SIGN(1.,.0001-ABS(DIFF))
   53    CONTINUE
    5   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 54 N=1,K
        UST(N)=MAX(UST(N), 0.014)
        UDIFF=ABS(UST(N)-UPAST(N))*(1.-SURFWIN)
        DIFF=ABS(WMUE(N)-PAST(N))
        TMYUS(N)=SIGN(1.,.01-UDIFF)
        TMYUS(N)=SIGN(1.,10.-DIFF)+TMYUS(N)
        TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/ UST(N)
   54  CONTINUE
    7 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE THE QUANTITIES NEEDED FOR THE NEXT OVERALL ITERATION STEP
C-----------------------------------------------------------------------
      K=0
      DO 92 J=1,MM
      DO 92 I=0,L
       IF (TMYUS1(I,J).EQ.2.) GOTO 92
       K=K+1
       T(I,J)=T1(K)-TMELT
       TMPL1(I,J)=TMPL(K)
       TMYUS1(I,J)=TMYUS(K)
       TT1(I,J)=TT(K)
       TUST1(I,J)=TUST(K)
       TMUE1(I,J)=TMUE(K)
       UST1(I,J)=UST(K)
       WMUE1(I,J)=WMUE(K)
   92 CONTINUE
C-----------------------------------------------------------------------
C  FINISH OVERALL ITERATION AFTER EXCEEDING THE SPECIFIED MAXIMUM
C-----------------------------------------------------------------------
      ITERW=ITERW+1
C**ITERATION-INFO CANCELLED:
      IF (ITERW.GT.IWMAX) GOTO 91
      GOTO 88
C  90 WRITE(16,700) IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED T, WMUE AND UST
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       IF (A(I,J,LRHS).EQ.0.) GOTO 93
       K=K+1
       HICE1(K)=HICE(I,J)
       ALB1(K)=ALB(I,J)
       A21(K)=A2(I,J)
       T1(K)=T(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(T(I,J), TAIR(I,J), 
     1                           HSN(I,J,LRHS), H(I,J,LRHS) ) )
       FM1(K)=FM(I,J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       UST(K)=UST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
  112 CONTINUE
      CALL STAB(TMPL,ZOI,K, PH1, PH2)
      CALL RESIST(WMUE,K, AF, BF, CF)
      DO 55 N=1,K
       FAKT=1./(LOG(UST(N)/ABS(FM1(N))/ZOI)-CF(N))
       FAKT1(N)=FAKTH*UST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=T1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTI(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/.623)
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
CD       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       ALB1(N) = albedo(t1(N), ta1(N), hice1(N), hsnow1(N) )

       ALPHA=FAKTH*UST(N)/PH1(N)
       STRE=UST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-T1(N))
       FLLA1(N)=ALPHA*(EA-ESTI(N))/CPAIR*VAPL*0.623/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/UST(N)
       Q1=D3*T1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       SINBET1(N)=-BF(N)/VONKAR*UST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
       COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
       BETA1(N)=ACOS(COSBET1(N))/RAD
       CD1(N)=(UST(N)/UG1(N))**2
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB1(N)-T1(N))/HICE1(N)*CON)
     1      /CLO
       FHB=((TB1(N) -T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   55 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED T, WMUE AND UST
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
        IF (OM(I,J).EQ.0.) GOTO 84
        IF (A(I,J,LRHS).EQ.0.) GOTO 84
        K=K+1
        FH(I,J)=FH1(K)
        IF (KG.NE.4)GOTO84
        CD(I,J)=CD1(K)
        SINBET(I,J)=SINBET1(K)
        COSBET(I,J)=COSBET1(K)
   84 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
