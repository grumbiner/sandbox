      SUBROUTINE ECMBDI(FH, T, LRHS, KG, LWDN, SWDN, QS)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C     -R. Grumbine                NMC, Camp Springs                 1994
C  PURPOSE:
C     -CALCULATES GROWTH RATES FOR ICE COVERED PART OF A GRID CELL WITH
C       ASL-PARAMETERIZATION ACC.TO LOUIS(79)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT (NOTE: SEE EKMAH)
C  METHOD:
C     -ICE OR SNOW SURFACE TEMPERATURES ARE CALCULATED BY ITERATION
C       (REGULA FALSI)
C     -MONIN-OBUKHOV THEORY WITH MONIN-OBUKHOV LENGTH REPLACED BY THE
C       RICHARDSON NUMBER
C  OPTIONS:
C     -STATEMENTS FOR ADDITIONAL WIND TURNING (ACC.TO STOESSEL (1990))
C  INTERFACE:
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    ICE OR SNOW SURFACE TEMPERATURE IN CELSIUS
C     -LRHS: RUNNING INDEX VALUE FOR OLD TIME STEP
C     -KG:   INDEX FOR ICE THICKNESS CATEGORIES
C     -LWDN: Downwelling longwave radiation
C     -SWDN: Downwelling shortwave radiation
C     -QS  : Mixed layer Salinity.
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -RISTAB: CALC.THE STAB.FUNCTIONS WITH FIXED ROUGHNESS LENGTH
C     -RESIST: CALC.STAB.FUNCTIONS FOR ADDITIONAL WIND TURNING(OPTIONAL)
C     -tfreez: Freezing point of salt water
C     -albedo: Sea ice albedo
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/CORR/FM(0:L,0:M), F(L,M), COSPHI(0:L,0:M), SINPHI(0:L,0:M)
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1  BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THCK/H(0:L,0:M,2), A(0:L,0:M,2), HSN(0:L,0:M,2)
      COMMON/STP/TX,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1 ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1 ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/WORK/HICE(0:L,0:M), WRK(0:L,0:M,2), ALB(0:L,0:M), 
     1 TRK(0:L,0:M), A2(0:L,0:M), FLAI(LMDP), FAKTS(LMDP), QG(LMDP), 
     2 THETG(LMDP), SPACE(LMDP,2)
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
      REAL FH(0:L,0:M), T(0:L,0:M), ESTA(LMDP), ESTI(LMDP), EA1(LMDP),
     1 ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -T:    SURFACE=ICE OR SNOW TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTI: SATURATION VAPOR PRESSURE OVER ICE
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), T1(LMDP), 
     2 ALB1(LMDP), A21(LMDP), HICE1(LMDP), UST(LMDP), CD1(LMDP), 
     3 FM1(LMDP), TMPL(LMDP), HSNOW1(LMDP)
C=======================================================================
C    Remark: The following are introduced to use externally-computed
C      downwelling radiation parameters.  BG
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL QS(0:L, 0:M), TB1(LMDP), tfreez, albedo
C======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
      REAL STP(LMDP), STPP(LMDP), FP(LMDP), FPP(LMDP), TT(LMDP),
     1TMYUS1(0:L,0:M)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE ITERATION PROCEDURE
C=======================================================================
      REAL SINBET1(LMDP), COSBET1(LMDP), BETA1(LMDP), WMUE(LMDP)
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED FOR THE OPTIONAL ADDITIONAL
C    WIND TURNING
C=======================================================================
C-----------------------------------------------------------------------
C  DETERMINE MAXIMUM NUMBER OF ITERATION STEPS
C-----------------------------------------------------------------------
      IMAX=30
C-----------------------------------------------------------------------
C  SELECT GRID CELLS TO BE INVOLVED
C-----------------------------------------------------------------------
      DO 79 J=1,MM
      DO 79 I=0,L
       TMYUS1(I,J)=OM(I,J)+2.
       IF (A(I,J,LRHS).EQ.0.)TMYUS1(I,J)=2.
   79 CONTINUE
C-----------------------------------------------------------------------
C  STORE EXTERNAL VARIABLES INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 1000 I = 1, LMDP
        HSNOW1(I) = 0.0
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
       TB1(K) = tfreez(QS(I,J)) +TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
CD       SWDN1(K) = SWDN(I,J) * (1. - albedo(T(I,J), TAIR(I,J), 
CD     1                            HSN(I,J,LRHS), H(I,J,LRHS) )  )
       SWDN1(K) = SWDN(I,J) * (1. - ALB1(K) )
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      FM1(K)=FM(I,J)
   82 CONTINUE
      IF (K.EQ.0) GOTO 87
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(T1,ESTI,2,K)
      DO 31 N=1,K
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*RGAS
     1          *TA1(N)*LOG(1.E5/PA1(N))/GRAV)*(PA1(N)/1.E5)**KAPPA
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=GRAV*ZA(N)*(THETG(N)- T1(N)+.61* T1(N)*(EA1(N)-ESTI(N))
     1         *EPSI/PA1(N))/ T1(N)/UG1(N)**2
   31 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
C-----------------------------------------------------------------------
C  MAKE FIRST GUESS FOR SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 33 N=1,K
       STP(N)=T1(N)
       FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*CPAIR
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FP(N)=D3*STP(N)**4- SWDN1(N) - LWDN1(N)
     1       -FLSE1(N)-FLLA1(N)+(STP(N)-TB1(N))/HICE1(N)*CON
       T1(N)=T1(N)+1.
       TT(N)=0.
   33 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE THE SURFACE TEMPERATURE (START OF ITERATION PROCEDURE)
C-----------------------------------------------------------------------
      DO 3 ITER=1,IMAX
       CALL VAPOR(T1,ESTI,2,K)
       DO 32 N=1,K
        TMPL(N)=GRAV*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1          *EPSI/PA1(N))/T1(N)/UG1(N)**2
   32  CONTINUE
       CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
       DO 34 N=1,K
        STPP(N)=STP(N)
        FPP(N)=FP(N)
        STP(N)=T1(N)
        FLSE1(N)=UG1(N)*(THETG(N)-STP(N))*RHOAIR*CPAIR
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1           *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
        FP(N)=D3*STP(N)**4- SWDN1(N)- LWDN1(N)
     1        -FLSE1(N)-FLLA1(N)+(STP(N)-TB1(N))/HICE1(N)*CON
        FDIFF=FP(N)-FPP(N)
        T1(N)=STP(N)-(STP(N)-STPP(N))*FP(N)
     1        /MAX(ABS(FDIFF), 1.E-10)*SIGN(1.,FDIFF)
        DIFF=T1(N)-STP(N)
        TT(N)=SIGN(1.,.01-ABS(DIFF))
   34  CONTINUE
    3 CONTINUE
C-----------------------------------------------------------------------
C  CALCULATE GROWTH RATES WITH UPDATED SURFACE TEMPERATURE
C-----------------------------------------------------------------------
      DO 83 N=1,K
       FLAG=.5*(1.+SIGN(1.,T1(N)-TMELT))
       T1(N)=T1(N)*(1.-FLAG)+TMELT*FLAG
   83 CONTINUE
      CALL VAPOR(T1,ESTI,2,K)
      DO 52 N=1,K
       TMPL(N)=GRAV*ZA(N)*(THETG(N)-T1(N)+.61*T1(N)*(EA1(N)-ESTI(N))
     1         *EPSI/PA1(N))/T1(N)/UG1(N)**2
   52 CONTINUE
      CALL RISTAB(TMPL,ZOI,K,PA1, PH1, PH2)
      DO 85 N=1,K
       A1=0.5*(1.+SIGN(1.,T1(N)-TMELT))
CD       ALB1(N)=A21(N)*ALBSNM+(1.-A21(N))*ALBM
       ALB1(N) = albedo(t1(N), ta1(N), HICE1(N), HSNOW1(N) )
       FLSE1(N)=UG1(N)*(THETG(N)-T1(N))*RHOAIR*CPAIR
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTI(N))*EPSI/PA1(N)*RHOAIR*VAPL
     1          *(.4/LOG(ZA(N)/ZOI))**2*PH1(N)/.74
       Q1=D3*T1(N)**4
       Q2= SWDN1(N)
       Q3= LWDN1(N)
       UST(N)=(.4/LOG(ZA(N)/ZOI))*UG1(N)*SQRT(PH2(N))
       CD1(N)=(UST(N)/UG1(N))**2
C**NEXT STATEMENT FOR ADDITIONAL WIND TURNING:
C      WMUE(N)=.4*UST(N)/ABS(FM1(N))/ZA(N)/PH1(N)*PH2(N)**2*TMPL(N)
       FHI=A1*(Q1-Q2-Q3-FLSE1(N)-FLLA1(N)-(TB1(N)-T1(N))/HICE1(N)*CON)
     1     /CLO
       FHB=((TB1(N) -T1(N))/HICE1(N)*CON)/CLB
       FH1(N)=FHI+FHB
   85 CONTINUE
C**NEXT CALL AND LOOP FOR ADDITIONAL WIND TURNING:
C     CALL RESIST(WMUE,K, AF, BF, CF)
C     DO 86 N=1,K
C      SINBET1(N)=-BF(N)/VONKAR*UST(N)/UG1(N)
C      FLAG=SIGN(1.,SINBET1(N))
C      SINBET1(N)=MIN(ABS(SINBET1(N)), 1.)*FLAG
C      COSBET1(N)=SQRT(1.-SINBET1(N)*SINBET1(N))
C      BETA1(N)=ACOS(COSBET1(N))/RAD
C  86 CONTINUE
C-----------------------------------------------------------------------
C  UNSCRAMBLE FOR TWO-DIMENSIONAL FIELD
C-----------------------------------------------------------------------
      K=0
      DO 84 J=1,MM
      DO 84 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 84
       IF (A(I,J,LRHS).EQ.0.) GOTO 84
       K=K+1
       FH(I,J)=FH1(K)
       T(I,J)=T1(K)-TMELT
C      IF (TT(K).LE.0.) WRITE(16,701) IIC
       IF (KG.NE.4) GOTO 84
       CD(I,J)=CD1(K)
C**NEXT FOUR STATEMENTS FOR ADDITIONAL WIND TURNING:
C      WMUE1(I,J)=WMUE(K)
C      SINBET(I,J)=SINBET1(K)
C      COSBET(I,J)=COSBET1(K)
C      BETA(I,J)=BETA1(K)
   84 CONTINUE

   87 CONTINUE

      RETURN
  701 FORMAT (1X,I4,'ITERATION EXCEEDED')
      END
