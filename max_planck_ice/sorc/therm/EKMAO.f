      SUBROUTINE EKMAO(FH,QT, LWDN, SWDN, QS)
C=======================================================================
C  PROGRAMMED BY:
C     -C.KOCH                     UNI, BONN                         1986
C  MODIFIED EXTENSIVELY BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  Extended by:
C      R. Grumbine                NMC, Camp Springs                 1994
C  LAST MODIFIED: 13 September 1994.
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH ABL-MODEL ACC.TO KOCH(88)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT AND TURNING
C       ANGLE TO BE USED IN SUBROUTINE RELCON (NOTE: SEE EKMAH)
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C     -MONIN-OBUKHOV THEORY FOR SURFACE LAYER (DERIVED QUANTITIES ONLY)
C     -ROSSBY NUMBER SIMILARITY THEORY FOR EKMAN LAYER (ABL)
C     -THE STABILITY PARAMETER (WMUE) AND THE FRICTION VELOCITY (WUST)
C       ARE SOLVED PER ITERATION (REGULA FALSI)
C     -IN ORDER TO ACCELERATE THE INTEGRATION, ONLY THOSE GRID CELLS
C       WHICH DID NOT PASS THE SOLUTION CRITERIA ARE SELECTED FOR
C       FURTHER ITERATIONS; THIS METHOD REQUIRES AN INTERMEDIATE
C       STORAGE OF THE VARIABLES INTO A ONE-DIMENSIONAL ARRAY
C  OPTIONS:
C     -THE FRICTION VELOCITY CAN BE CALCULATED VIA THE RESISTANCE LAWS
C       OF THE EKMAN-LAYER (SURFWIN=0) OR VIA THE MONIN-OBUKHOV THEORY
C       (SURFWIN=1)
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -LWDN: Downwelling Longwave radiation
C     -SWDN: Downwelling shortwave radiation
C     -QS  : Mixed layer salinity
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -STAB:   CALCULATION OF STABILITY FUNCTIONS FOR SURFACE LAYER
C     -RESIST: CALCULATION OF STABILITY FUNCTIONS FOR EKMAN LAYER
C     -tfreez: Compute freezing point of salt water
C     -albedo: Sea ice albedo
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1  ,WUST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/WRK(0:L,0:M,6), FLAW(LMDP), FAKTS(LMDP), QG(LMDP),
     1THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -QG:    SPECIFIC HUMIDITY AT 850 HPA
C     -THETG: POTENTIAL TEMPERATURE AT 850 HPA
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP), DELT(LMDP)
     1  ,SUM1(LMDP), SUM2(LMDP), FAKT1(LMDP), QAL(LMDP)
C=======================================================================
C     -FH:    GROWTH RATE IN METERS OF ICE
C     -QT:    SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA:  SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW:  SATURATION VAPOR PRESSURE OVER WATER
C     -DELT:  VERTICAL TEMPERATURE GRADIENT
C     -SUM1:  LOG OF MODIFIED SURFACE ROSSBY NUMBER
C     -SUM2:  VERTICAL SPECIFIC HUMIDITY GRADIENT FOR VIRTUAL TEMP.GRAD.
C     -FAKT1: FACTOR FOR CALCULATION OF STABILITY PARAMETER
C     -QAL:   FACTOR FOR RESISTANCE LAWS OF BAROTROPIC EKMAN LAYER
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1  FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP), 
     2  ZOW1(LMDP), WUST(LMDP), CD1(LMDP), WMUE(LMDP), FM1(LMDP), 
     3  SINBET1(LMDP), COSBET1(LMDP), TMPL(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL QS(0:L, 0:M), TB1(LMDP), tfreez, albedo
C=======================================================================
C  REMARK: THESE VARIABLES ARE NECESSARY FOR THE OPTIMIZATION OF THE
C    ITERATION PROCEDURES
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), UPAST(LMDP),
     1  TWMYUS1(0:L,0:M), TWMYUS(LMDP), TWMUE(LMDP), TWUST(LMDP), 
     2  PAST(LMDP), TWMUE1(0:L,0:M), TWUST1(0:L,0:M)
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
   79 TWMYUS1(I,J)=OM(I,J)+2.
      ITERW=0
C-----------------------------------------------------------------------
C  START OF OVERALL ITERATION PROCEDURE
C-----------------------------------------------------------------------
   88 CONTINUE
      K=0
C-----------------------------------------------------------------------
C  SELECT GRID CELLS FOR FURTHER ITERATIONS AND BUILD UP ONE-DIM. ARRAY
C-----------------------------------------------------------------------
      DO 80 J=1,MM
      DO 80 I=0,L
       IF (TWMYUS1(I,J).EQ.2.) GOTO 80
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       TB1(K) = tfreez(QS(I,J)) + TMELT
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       FM1(K)=FM(I,J)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(0., 0., 0., 0.) )
       IF (ITERW.EQ.0) GOTO 80
       WUST(K)=WUST1(I,J)
       WMUE(K)=WMUE1(I,J)
   80 CONTINUE
      IF (K.EQ.0) GOTO 91
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATIONS
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 11 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
       DELT(N)=THETG(N)-QT1(N)
       IF (ITERW.GT.0) GOTO 11
       WUST(N)=.1
       WMUE(N)=SIGN(50.,DELT(N))
   11 CONTINUE
C-----------------------------------------------------------------------
C  REPEAT THE SINGLE ITERATION PRODECURES TWICE
C-----------------------------------------------------------------------
      DO 6 ITERM=1,3
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR THE STABILITY PARAMETER
C-----------------------------------------------------------------------
       DO 12 N=1,K
        PAST(N)=WMUE(N)
        SUM1(N)=LOG(WUST(N)/ABS(FM1(N))/ZOW1(N))
        SUM2(N)=0.61*(QG(N)-EPSI/PA1(N)*ESTW(N))
        FAKT1(N)=GRAV*0.064/ABS(FM1(N))/WUST(N)
        STP(N)=WMUE(N)
   12  CONTINUE
       CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
       CALL RESIST(STP,K, AF, BF, CF)
       DO 13 N=1,K
        FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
        SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
        FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1        /(QT1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
        WMUE(N)=WMUE(N)*1.5
   13  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE STABILITY PARAMETER (START OF ITERATION)
C-----------------------------------------------------------------------
       DO 1 ITER=1,IMAX
        DO 14 N=1,K
         STPP(N)=STP(N)
         FPP(N)=FP(N)
         STP(N)=WMUE(N)
         TMPL(N)=5.*ABS(FM1(N))*STP(N)/WUST(N)
   14   CONTINUE
        CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
        CALL RESIST(STP,K, AF, BF, CF)
        DO 15 N=1,K
         FLAG=.5*(1.+SIGN(1.,(SUM1(N)-CF(N)-1.E-6)))
         SUM1(N)=SUM1(N)*FLAG+(CF(N)+.1)*(1.-FLAG)
         FP(N)=STP(N)*(SUM1(N)-CF(N))-FAKT1(N)*(DELT(N)
     1         /(QT1(N)+PH1(N)*DELT(N)/(SUM1(N)-CF(N)))+SUM2(N))
         FDIFF=FP(N)-FPP(N)
         WMUE(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1           MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
         DIFF=WMUE(N)-STP(N)
         TWMUE(N)=SIGN(1.,10.-ABS(DIFF))
   15   CONTINUE
    1  CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE FRICTION VELOCITY
C-----------------------------------------------------------------------
C**FOR CYCLE 7 SKIP UST-ITERATION:
       IF (SURFWIN.EQ.1)THEN
        DO 66 N=1,K
   66   WUST(N)=.4*UG1(N)/PH2(N)
       ELSE
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR FRICTION VELOCITY
C-----------------------------------------------------------------------
        CALL RESIST(WMUE,K, AF, BF, CF)
        DO 22 N=1,K
         UPAST(N)=WUST(N)
         STP(N)=WUST(N)
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
         FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
         STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
         ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
         FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOW1(N))
         WUST(N)=WUST(N)*0.8
   22   CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE FRICTION VELOCITY PER ITERATION
C-----------------------------------------------------------------------
        DO 2 ITER=1,IMAX+1
         DO 23 N=1,K
          FPP(N)=FP(N)
          STPP(N)=STP(N)
          STP(N)=WUST(N)
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FLAG1=.5*(1.+(SIGN(1.,(ZWP-1.E-10))))
          FLAG2=.5*(1.+(SIGN(1.,(STP(N)-1.E-10))))
          STP(N)=UG1(N)/BF(N)*.39*(1.-FLAG1*FLAG2)+STP(N)*FLAG1*FLAG2
          ZWP=0.16*UG1(N)**2/STP(N)**2-BF(N)**2
          FP(N)=AF(N)+SQRT(ZWP)-LOG(STP(N)/ABS(FM1(N))/ZOW1(N))
          FDIFF=FP(N)-FPP(N)
          WUST(N)=STP(N)-(STP(N)-STPP(N))*FP(N)/
     1            MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
          DIFF=WUST(N)-STP(N)
          TWUST(N)=SIGN(1.,.0001-ABS(DIFF))
   23    CONTINUE
    2   CONTINUE
       END IF
C-----------------------------------------------------------------------
C  DETERMINE WHETHER WE SUCCEEDED IN FINDING ANY SOLUTION
C-----------------------------------------------------------------------
       DO 24 N=1,K
        WUST(N)=MAX(WUST(N), 0.014)
        UDIFF=ABS(WUST(N)-UPAST(N))*(1.-SURFWIN)
        DIFF=ABS(WMUE(N)-PAST(N))
        TWMYUS(N)=SIGN(1.,.01-UDIFF)
        TWMYUS(N)=SIGN(1.,10.-DIFF)+TWMYUS(N)
        TMPL(N)=5.*ABS(FM1(N))*WMUE(N)/WUST(N)
   24  CONTINUE
    6 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE THE QUANTITIES NEEDED FOR THE NEXT OVERALL ITERATION STEP
C-----------------------------------------------------------------------
      K=0
      DO 92 J=1,MM
      DO 92 I=0,L
       IF (TWMYUS1(I,J).EQ.2.) GOTO 92
       K=K+1
       TWMUE1(I,J)=TWMUE(K)
       TWUST1(I,J)=TWUST(K)
       TMPL1(I,J)=TMPL(K)
       TWMYUS1(I,J)=TWMYUS(K)
       WUST1(I,J)=WUST(K)
       WMUE1(I,J)=WMUE(K)
   92 CONTINUE
C-----------------------------------------------------------------------
C  FINISH OVERALL ITERATION AFTER EXCEEDING THE SPECIFIED MAXIMUM
C-----------------------------------------------------------------------
      ITERW=ITERW+1
C**ITERATION-INFO CANCELLED:
      IF (ITERW.GT.IWMAX) GOTO 91
      GOTO 88
C  90 WRITE(16,701) IIC
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED STABILITY AND FRICTION VELOCITY
C-----------------------------------------------------------------------
   91 CONTINUE
      K=0
       DO 93 J=1,MM
       DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       K=K+1
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(0., 0., 0., 0.) )
       FM1(K)=FM(I,J)
       TMPL(K)=TMPL1(I,J)
       WMUE(K)=WMUE1(I,J)
       WUST(K)=WUST1(I,J)
   93 CONTINUE
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       FTHET=(PA1(N)/ATMLEV)**KAPPA
       THETG(N)=FTHET*TA1(N)
       QG(N)=EPSI/(ATMLEV/(TD1(N)*ESTA(N))-(1.-EPSI))
  112 CONTINUE
      CALL STAB(TMPL,ZOW(0,0), K, PH1, PH2)
      CALL RESIST(WMUE,K, AF, BF, CF)
      DO 25 N=1,K
       FAKT=1./(LOG(WUST(N)/ABS(FM1(N))/ZOW1(N))-CF(N))
       FAKT1(N)=FAKTH*WUST(N)*FAKT
       QAL(N)=PH1(N)*FAKT
       TA1(N)=QT1(N)*(1.-QAL(N))+QAL(N)*THETG(N)
       EA=MAX(0.,ESTW(N)*(1.-QAL(N))+QAL(N)*QG(N)*PA1(N)/EPSI)
       ALPHE=FAKTH*WUST(N)/(SUM1(N)-CF(N))
       ALPHA=FAKTH*WUST(N)/PH1(N)
       STRE=WUST(N)**2*RHOAIR
       FLSE1(N)=ALPHA*(TA1(N)-QT1(N))
       FLLA1(N)=ALPHA*(EA-ESTW(N))/CPAIR*VAPL*EPSI/PA1(N)
       ZL=5.*ABS(FM1(N))*WMUE(N)/WUST(N)
       Q1=D3*QT1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       SINBET1(N)=-BF(N)/VONKAR*WUST(N)/UG1(N)
       FLAG=SIGN(1.,SINBET1(N))
       SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
       COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
       CD1(N)=(WUST(N)/UG1(N))**2
       FH1(N)=(Q1-Q2-Q3-FLSE1(N)-FLLA1(N))/CLB
   25 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 81 J=1,MM
      DO 81 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 81
       K=K+1
       FH(I,J)=FH1(K)
       CD(I,J)=CD1(K)
       SINBET(I,J)=SINBET1(K)
       COSBET(I,J)=COSBET1(K)
C**ITERATION-INFO CANCELLED:
C      IF (TWMUE1(I,J).LE.0..OR.TWUST1(I,J).LE.0.) WRITE(16,701) IIC
   81 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
