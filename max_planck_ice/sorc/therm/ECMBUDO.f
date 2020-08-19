      SUBROUTINE ECMBDO(FH, QT, LWDN, SWDN)
C=======================================================================
C  PROGRAMMED BY:
C     -A.STOESSEL                 MPI, HAMBURG                      1990
C  LAST MODIFIED: 13 September 1994.
C  PURPOSE:
C     -CALCULATES GROWTH RATES OF NEW ICE IN THE ICE FREE PART OF A GRID
C       CELL WITH ASL-PARAMETERIZATION ACC.TO LOUIS(79)
C     -CALCULATES STABILITY DEPENDENT DRAG COEFFICIENT (NOTE: SEE EKMAH)
C  METHOD:
C     -HEAT BUDGET EQUATION FOR OPEN WATER
C     -MONIN-OBUKHOV THEORY WITH MONIN-OBUKHOV LENGTH REPLACED BY THE
C       RICHARDSON NUMBER
C  INTERFACE:
C     -FH: GROWTH RATE IN METERS OF ICE
C     -QT: SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -LWDN: Downwelling longwave in W/M**2
C     -SWDN: Downwelling shortwave in W/M**2
C  EXTERNALS:
C     -VAPOR:  CALCULATES VAPOR PRESSURE
C     -RWSTAB: CALC.THE STAB.FUNCTIONS WITH VARIABLE ROUGHNESS LENGTH
C     -tfreez: Freezing point of salt water
C     -albedo: Sea ice albedo
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/ABLM/ZOW(0:L,0:M), FAKTH, SURTYP, SURFWIN
      COMMON/TAU/CD(0:L,0:M), SINBET(0:L,0:M), COSBET(0:L,0:M),
     1 BETA(0:L,0:M), TAUX(L,M), TAUY(L,M)
      COMMON/THFOR/TAIR(0:L,0:M), TD (0:L,0:M), ACL(0:L,0:M), 
     1 PA(0:L,0:M), UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M), VWIN(L,M)
      COMMON/MASK/VM(L,M), HM(0:L,0:M), OM(0:L,0:M), FLM(0:L,0:M,2)
      COMMON/FLUX/FLSE(0:L,0:M), FLLA(0:L,0:M), WMUE1(0:L,0:M)
     1 ,UST1(0:L,0:M), TMPL1(0:L,0:M)
      COMMON/RES/AF(LMDP), BF(LMDP), CF(LMDP), PH1(LMDP), PH2(LMDP)
      COMMON/WORK/WRK(0:L,0:M,6), FLAW(LMDP), FAKTS(LMDP), TRK(LMDP),
     1 THETG(LMDP), SPACE(LMDP,2)
C=======================================================================
C     -WRK:   DUMMY ARRAYS
C     -FLAW:  CLOUDINESS AND ALBEDO TERM FOR SHORT WAVE RADIATION
C     -FAKTS: CLOUDINESS TERM FOR LONG WAVE RADIATION
C     -TRK:   DUMMY ARRAY
C     -THETG: POTENTIAL TEMPERATURE
C=======================================================================
      REAL FH(0:L,0:M), QT(0:L,0:M), ESTA(LMDP), ESTW(LMDP), EA1(LMDP),
     1 ZA(LMDP)
C=======================================================================
C     -FH:   GROWTH RATE IN METERS OF ICE
C     -QT:   SEA SURFACE=OML TEMPERATURE IN CELSIUS
C     -ESTA: SATURATION VAPOR PRESSURE OF ATMOSPHERE
C     -ESTW: SATURATION VAPOR PRESSURE OVER WATER
C     -EA1:  RELATIVE HUMIDITY
C     -ZA:   HEIGHT OF FORCING LEVEL
C=======================================================================
      REAL TA1(LMDP), TD1(LMDP), PA1(LMDP), UG1(LMDP),
     1 FLSE1(LMDP), FLLA1(LMDP), FH1(LMDP), QT1(LMDP), 
     2 ZOW1(LMDP), WUST(LMDP), CD1(LMDP), TMPL(LMDP)
C=======================================================================
      REAL LWDN(0:L, 0:M), SWDN(0:L, 0:M), LWDN1(LMDP), SWDN1(LMDP)
      REAL tfreez, albedo
C=======================================================================
C  REMARK: THESE VARIABLES ARE INTRODUCED IN ORDER TO BE COMMENSURATE
C    WITH THE ABL ROUTINES (EKMAO,EKMAH), WHICH ARE OPTIMIZED WITH
C    REGARD TO THE ITERATION PROCEDURE(S)
C=======================================================================
C-----------------------------------------------------------------------
C  SELECT GRID CELLS AND STORE THEM INTO ONE-DIMENSIONAL ARRAY
C-----------------------------------------------------------------------
      K=0
      DO 93 J=1,MM
      DO 93 I=0,L
       IF (OM(I,J).EQ.0.) GOTO 93
       K=K+1
       ZOW1(K)=ZOW(I,J)
       QT1(K)=QT(I,J)+TMELT
       TA1(K)=TAIR(I,J)+TMELT
       TD1(K)=MAX(.1,TD(I,J)/100.)
       PA1(K)=PA(I,J)
       UG1(K)=MAX(UG(I,J), 2.)
       LWDN1(K) = LWDN(I,J)
       SWDN1(K) = SWDN(I,J) * (1. - albedo(0., 0., 0., 0.) )
   93 CONTINUE
C-----------------------------------------------------------------------
C  PREPARE MAIN COMPUTATION
C-----------------------------------------------------------------------
      CALL VAPOR(TA1,ESTA,1,K)
      CALL VAPOR(QT1,ESTW,3,K)
      DO 112 N=1,K
       EA1(N)=TD1(N)*ESTA(N)
       FLAG=.5*(1.-SIGN(1.,PA1(N)-1.E5))
       THETG(N)=FLAG*TA1(N)+(1.-FLAG)*(TA1(N)+6.5E-3*RGAS
     1          *TA1(N)*LOG(1.E5/PA1(N))/GRAV)*(PA1(N)/1.E5)**KAPPA
       ZA(N)=MAX(30.,((PA1(N)-100000.)*.08))
       TMPL(N)=GRAV*ZA(N)*(THETG(N)-QT1(N)+.61*QT1(N)*(EA1(N)-ESTW(N))
     1         *EPSI/PA1(N))/QT1(N)/UG1(N)**2
  112 CONTINUE
C-----------------------------------------------------------------------
C  GET THE STABILITY FUNCTIONS
C-----------------------------------------------------------------------
      CALL RWSTAB(TMPL,ZOW1,K,PA1, PH1, PH2)
C-----------------------------------------------------------------------
C  CALCULATE HEAT FLUXES AND GROWTH RATES
C-----------------------------------------------------------------------
      DO 25 N=1,K
       FLSE1(N)=UG1(N)*(THETG(N)-QT1(N))*RHOAIR*CPAIR*(.4/LOG(ZA(N)/
     1          ZOW1(N)))**2*PH1(N)/.74
       FLLA1(N)=UG1(N)*(EA1(N)-ESTW(N))*EPSI/PA1(N)*RHOAIR*VAPL*
     1          (.4/LOG(ZA(N)/ZOW1(N)))**2*PH1(N)/.74
       Q1=D3*QT1(N)**4
       Q2 = SWDN1(N)
       Q3 = LWDN1(N)
       WUST(N)=(.4/LOG(ZA(N)/ZOW1(N)))*UG1(N)*SQRT(PH2(N))
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
       ZOW(I,J)=MAX(1.5E-5,.032*WUST(K)**2/GRAV)
       CD(I,J)=CD1(K)
       FH(I,J)=FH1(K)
   81 CONTINUE

      RETURN
      END
