                        SUBROUTINE RADFS 
C     *****************************************************************
C     *                                                               *
C     *   THE INTERNAL DRIVE FOR GFDL RADIATION                       *
C     *   THIS SUBROUTINE WAS FROM Y.H AND K.A.C (1993)               *
C     *   AND MODIFIED BY Q. ZHAO FOR USE IN THE ETA MODEL            *
C     *                   NOV. 18,  1993                              *
C     *                                                               *
C     * UPDATE: THIS SUBROUTINE WAS MODIFIED TO USE CLOUD FRACTION    *
C     *         ON EACH MODEL LAYER.                                  *
C     *                                QINGYUN  ZHAO   95-3-22        *
C     *                                                               *
C     * UPDATE: R1 HAS BEEN ADDED TO THE INPUTS FROM RADTN TO         *
C     *         COMPUTE THE VARIATION OF SOLAR CONSTANT AT THE TOP    *
C     *         OF ATMOSPHERE WITH JULIAN DAY IN A YEAR.              *
C     *                                QINGYUN  ZHAO   96-7-23        *
C     *****************************************************************
C***
C***  REQUIRED INPUT:
C***
     1          (QS,PP,PPI,QQH2O,TT,O3QO3,TSFC,SLMSK,ALBEDO,XLAT
     2,          CAMT,ITYP,KTOP,KBTM,NCLDS,EMCLD,RRCL,TTCL
     3,          COSZRO,TAUDAR,IBEG
     4,          KO3,KALB
     5,          SLMRF,SLYMRF,ITIMSW,ITIMLW
C***************************************************************************
C*              IX IS THE LENGTH OF A ROW IN THE DOMAIN
C
C*   QS(IX):		THE SURFACE PRESSURE (PA)
C*   PP(IX,L):		THE MIDLAYER PRESSURES (PA)  (L IS THE VERT. DIMEN.)
C*   PPI(IX,LP1)	THE INTERFACE PRESSURES (PA)
C*   QQH2O(IX,L):	THE MIDLAYER WATER VAPOR MIXING RATIO (KG/KG)
C*   TT(IX,L):		THE MIDLAYER TEMPERATURE (K)
C*   O3QO3(IX,L):	THE MIDLAYER OZONE MIXING RATIO
C*   TSFC(IX):		THE SKIN TEMP. (K); NEGATIVE OVER WATER
C*   SLMSK(IX):		THE SEA MASK (LAND=0,SEA=1)
C*   ALBEDO(IX):	THE SURFACE ALBEDO (EXPRESSED AS A FRACTION)
C*   XLAT(IX):		THE GEODETIC LATITUDES OF EACH COLUMN IN DEGREES
C*				(N.H.> 0)
C* THE FOLLOWING ARE CLOUD INFORMATION FOR EACH CLOUD LAYER
C*                      LAYER=1:SURFACE
C*                      LAYER=2:FIRST LAYER ABOVE GROUND, AND SO ON
C*   CAMT(IX,LP1):      CLOUD FRACTION OF EACH CLOUD LAYER
C*   ITYP(IX,LP1):      CLOUD TYPE(=1: STRATIFORM, =2:CONVECTIVE)
C*   KTOP(IX,LP1):      HEIGHT OF CLOUD TOP OF EACH CLOUD LAYER (IN ETA LEVEL)
C*   KBTM(IX,LP1):      BOTTOM OF EACH CLOUD LAYER
C*   NCLDS(IX):         NUMBER OF CLOUD LAYERS
C*   EMCLD(IX,LP1):     CLOUD EMISSIVITY
C*   RRCL(IX,NB,LP1)    CLOUD REFLECTTANCES FOR SW SPECTRAL BANDS
C*   TTCL(IX,NB,LP1)    CLOUD TRANSMITANCES FOR SW SPECTRAL BANDS
C* THE ABOVE ARE CLOUD INFORMATION FOR EACH CLOUD LAYER
C*
C*   COSZRO(IX):	THE COSINE OF THE SOLAR ZENITH ANGLE
C*   TAUDAR:		=1.0
C*   IBEG:		=1
C*   KO3:		=1 ( READ IN THE QZONE DATA)
C*   KALB:		=0
C*   SLMRF(LP1):	THE INTERFACE'S ETA (LP1=L+1)
C*   SLYMRF(L):		THE MIDLAYER ETA 
C*   ITIMSW:		=1/0 (SHORTWAVE CALC. ARE DESIRED/NOT DESIRED)
C*   ITIMLW:		=1/0 (LONGWAVE CALC. ARE DESIRED/NOT DESIRED)
C************************************************************************
C***
C*** THE FOLLOWING ARE ADDITIONAL FOR ETA MODEL
C***
     6,          JD,R1,GMT
C**************************************************************************
C*   JD: JULIAN DAY IN A YEAR
C*   R1: THE NON-DIMENSIONAL SUN-EARTH DISTANCE
C*   GMT:HOUR
C**************************************************************************
C***
C*** GENERATED OUTPUT REQUIRED BY THE ETA MODEL
C***
     7,          SWH,HLW
     8,          FLWUP,FSWUP,FSWDN,FSWDNS,FSWUPS,FLWDNS,FLWUPS)
C************************************************************************
C*    SWH: ATMOSPHERIC SHORTWAVE HEATING RATES IN K/S.
C*         SWH IS A REAL ARRAY DIMENSIONED (NCOL X LM).
C*    HLW: ATMOSPHERIC LONGWAVE HEATING RATES IN K/S.
C*         HLW IS A REAL ARRAY DIMENSIONED (NCOL X LM).
C*  FLWUP: UPWARD LONGWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
C*         FLWUP IS A REAL ARRAY DIMENSIONED (NCOL).
C*  FSWUP: UPWARD SHORTWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
C*         FSWUP IS A REAL ARRAY DIMENSIONED (NCOL).
C*  FSWDN: DOWNWARD SHORTWAVE FLUX AT TOP OF THE ATMOSPHERE IN W/M**2.
C*         FSWDN IS A REAL ARRAY DIMENSIONED (NCOL).
C* FSWDNS: DOWNWARD SHORTWAVE FLUX AT THE SURFACE IN W/M**2.
C*         FSWDNS IS A REAL ARRAY DIMENSIONED (NCOL).
C* FSWUPS: UPWARD SHORTWAVE FLUX AT THE SURFACE IN W/M**2.
C*         FSWUPS IS A REAL ARRAY DIMENSIONED (NCOL).
C* FLWDNS: DOWNWARD LONGWAVE FLUX AT THE SURFACE IN W/M**2.
C*         FLWDNS IS A REAL ARRAY DIMENSIONED (NCOL).
C* FLWUPS: UPWARD LONGWAVE FLUX AT THE SURFACE IN W/M**2.
C*         FLWUPS IS A REAL ARRAY DIMENSIONED (NCOL).
C************************************************************************
C***
C*** THE FOLLOWING OUTPUTS ARE NOT REQUIRED BY THE ETA MODEL
C***
C----------------------------------------------------------------------
       INCLUDE "parmeta"
       INCLUDE "mpp.h"
#include "sp.h"
C----------------------------------------------------------------------
C      ****************************************************************
C      *  GENERALIZED FOR PLUG-COMPATIBILITY -                        *
C      *    ORIGINAL CODE WAS CLEANED-UP GFDL CODE...K.CAMPANA MAR89..*
C......*  EXAMPLE FOR MRF:                                            *
C      *    KO3  =0  AND O3QO3=DUMMY ARRAY.   (GFDL CLIMO O3 USED)    *
C      *    KEMIS=0  AND HI CLD EMIS COMPUTED HERE (CEMIS=DUMMY INPUT)*
C      *    KALB =0  AND SFC ALBEDO OVER OPEN WATER COMPUTED BELOW... *
C      *    KCCO2=0,CO2 OBTAINED FROM BLOCK DATA                      *
C      *         =1,CO2 COMPUTED IN HERE --- NOT AVAILABLE YET...     *
C      *    SLMRF = INTERFACE (LEVELS) SIGMA                          *
C      *    SLYMRF= LAYER SIGMA                                       *
C      *  UPDATED FOR YUTAI HOU SIB SW RADIATION....KAC 6 MAR 92      *
C      *    OCEAN ALBEDO FOR BEAM SET TO BULK SFCALB, SINCE           *
C      *       COSINE ZENITH ANGLE EFFECTS ALREADY THERE(REF:PAYNE)   *
C      *       SLMSK = 0.                                             *
C      *    SNOW ICE ALBEDO FOR BEAM NOT ENHANCED VIA COSINE ZENITH   *
C      *       ANGLE EITHER CAUSE VALU ALREADY HIGH (WE SEE POLAR     *
C      *       COOLING IF WE DO BEAM CALCULATION)....KAC 17MAR92      *
C      *       ALBEDO GE .5                                           *
C      *   UPDATED TO OBTAIN CLEAR SKY FLUXES "ON THE FLY" FOR        *
C      *       CLOUD FORCING DIAGNOSTICS ELSEWHERE...KAC 7AUG92       *
C      *       SEE ##CLR LINES...RADFS,LWR88,FST88,SPA88 .......      *
C      *  UPDATED FOR USE NEW CLD SCHEME      ......YH  DEC 92        *
C      *    INPUT CLD MAY BE AS ORIGINAL IN 3 DOMAIN (CLD,MTOP,MBOT)  *
C      *       OR IN A VERTICAL ARRAY OF 18 MDL LAYERS (CLDARY)       *
C      *    IEMIS=0  USE THE ORG. CLD EMIS SCHEME                     *
C      *         =1  USE TEMP DEP. CLD EMIS SCHEME                    *
C      *  UPDATED TO COMPUTE CLD LAYER REFLECTTANCE AND TRANSMITTANCE *
C      *    INPUT CLD EMISSIVITY AND OPTICAL THICKNESS 'EMIS0,TAUC0'  *
C      *                                      ......YH FEB 93         *
C      ****************************************************************
C
      INCLUDE "HCON.comm" 
      INCLUDE "rdparm" 
      PARAMETER (LNGTH=37*L)
      INCLUDE "RNDDTA.comm" 
      INCLUDE "CO2DTA.comm" 
      INCLUDE "TABCOM.comm" 
C
C......................................................................
C        *********************************************
C====>   *   INPUT FROM FROM CALLING PROGRAM         *
C        *********************************************
       DIMENSION
     1  QS(IDIM1:IDIM2),PP(IDIM1:IDIM2,L),QQH2O(IDIM1:IDIM2,L)
     2, TT(IDIM1:IDIM2,L),TSFC(IDIM1:IDIM2),SLMSK(IDIM1:IDIM2)
     3, ALBEDO(IDIM1:IDIM2),XLAT(IDIM1:IDIM2),ITYP(IDIM1:IDIM2,LP1)
     4, COSZRO(IDIM1:IDIM2),TAUDAR(IDIM1:IDIM2),PPI(IDIM1:IDIM2,LP1)
     5, NCLDS(IDIM1:IDIM2),CAMT(IDIM1:IDIM2,LP1)
     6, KTOP(IDIM1:IDIM2,LP1),KBTM(IDIM1:IDIM2,LP1)
     7, EMCLD(IDIM1:IDIM2,LP1)
     8, RRCL(IDIM1:IDIM2,NB,LP1),TTCL(IDIM1:IDIM2,NB,LP1)
C
      DIMENSION SLMRF(LP1),SLYMRF(L)
C
C        *********************************************
C====>   *   POSSIBLE INPUT FROM CALLING PROGRAM     *
C        *********************************************
      DIMENSION
     1  O3QO3(IDIM1:IDIM2,L)
      DIMENSION
     1  ALVBR(IDIM1:IDIM2),ALNBR(IDIM1:IDIM2)
     2, ALVDR(IDIM1:IDIM2),ALNDR(IDIM1:IDIM2)
C        *********************************************
C====>   *   OUTPUT TO CALLING PROGRAM               *
C        *********************************************
      DIMENSION SWH(IDIM1:IDIM2,L),HLW(IDIM1:IDIM2,L)
      DIMENSION FSWUP(IDIM1:IDIM2),FSWUPS(IDIM1:IDIM2)
     1,         FSWDN(IDIM1:IDIM2),FSWDNS(IDIM1:IDIM2)
     2,         FLWUP(IDIM1:IDIM2),FLWUPS(IDIM1:IDIM2)
     3,         FLWDNS(IDIM1:IDIM2)
C        *********************************************
C====>   *   POSSIBLE OUTPUT TO CALLING PROGRAM      *
C        *********************************************
C...... DOWNWARD SW FLUXES FOR THE SIB PARAMETERIZATION
      DIMENSION
     1  GDFVBR(IDIM1:IDIM2),GDFNBR(IDIM1:IDIM2)
     2, GDFVDR(IDIM1:IDIM2),GDFNDR(IDIM1:IDIM2)
C        ************************************************************
C====>   *   ARRAYS NEEDED BY SWR91SIB..FOR CLEAR SKY DATA(EG.FSWL) *
C        ************************************************************
      DIMENSION
     1  FSWL(IDIM1:IDIM2,LP1),HSWL(IDIM1:IDIM2,LP1)
     2, UFL(IDIM1:IDIM2,LP1),DFL(IDIM1:IDIM2,LP1)
C        ******************************************************
C====>   *   ARRAYS NEEDED BY CLO88, LWR88, SWR89 OR SWR91SIB *
C        ******************************************************
      DIMENSION EQCMT(IDIM1:IDIM2,LP1)
      DIMENSION CLDFAC(IDIM1:IDIM2,LP1,LP1)
      DIMENSION PRESS(IDIM1:IDIM2,LP1),TEMP(IDIM1:IDIM2,LP1)
     1,         RH2O(IDIM1:IDIM2,L),QO3(IDIM1:IDIM2,L)
      DIMENSION HEATRA(IDIM1:IDIM2,L),GRNFLX(IDIM1:IDIM2)
     1,         TOPFLX(IDIM1:IDIM2)
      DIMENSION FSW(IDIM1:IDIM2,LP1),HSW(IDIM1:IDIM2,LP1)
     1,         GRDFLX(IDIM1:IDIM2)
     2,         UF(IDIM1:IDIM2,LP1),DF(IDIM1:IDIM2,LP1)
      DIMENSION COSZEN(IDIM1:IDIM2),TAUDA(IDIM1:IDIM2)
C..... ADD PRESSURE INTERFACE
      COMMON /SWRSAV/ ABCFF(NB),PWTS(NB),CFCO2,CFO3,REFLO3,RRAYAV
C        *********************************************
C====>   *   VECTOR TEMPORARIES FOR CLOUD CALC.      *
C        *********************************************
      DIMENSION
     1  JJROW(IDIM1:IDIM2),DO3V(IDIM1:IDIM2)
     2, DO3VP(IDIM1:IDIM2),TTHAN(IDIM1:IDIM2)
C====>    **************************************************************
C--     SEASONAL CLIMATOLOGIES OF O3 (OBTAINED FROM A PREVIOUSLY RUN
C             CODE WHICH INTERPOLATES O3 TO USER VERTICAL COORDINATE).
C         DEFINED AS 5 DEG LAT MEANS N.P.->S.P.
      COMMON /SAVMEM/
C-       ...WINTER....  ...SPRING....  ...SUMMER....  ....FALL.....
     1   DDUO3N(37,L), DDO3N2(37,L), DDO3N3(37,L), DDO3N4(37,L)
      DIMENSION RAD1(LNGTH), RAD2(LNGTH), RAD3(LNGTH), RAD4(LNGTH)
      EQUIVALENCE (RAD1(1),DDUO3N(1,1)),(RAD2(1),DDO3N2(1,1))
      EQUIVALENCE (RAD3(1),DDO3N3(1,1)),(RAD4(1),DDO3N4(1,1))
C====>    **************************************************************
C
      COMMON / SSALB /
     2   ALBD(21,20),        ZA(20),             TRN(21),
     3   DZA(19)
C
C    ***********************************************************
C    ***********************************************************
C
      DIMENSION
     1  ALVB(IDIM1:IDIM2),ALNB(IDIM1:IDIM2)
     2, ALVD(IDIM1:IDIM2),ALND(IDIM1:IDIM2)
     3, GDFVB(IDIM1:IDIM2),GDFNB(IDIM1:IDIM2)
     4, GDFVD(IDIM1:IDIM2),GDFND(IDIM1:IDIM2)
      DIMENSION  SFCALB(IDIM1:IDIM2)
      DIMENSION XAMT(IDIM1:IDIM2,LP1),MTOPSW(IDIM1:IDIM2,LP1)
     1,         MBTMSW(IDIM1:IDIM2,LP1)
      COMMON /RDFSAV/ EMISP,EMIST,XLATT,XLATP,Q19001,HP98,H3M6,
     *     HP75,H6M2,HP537,H74E1,H15E1,Q14330,HP2,TWENTY,HNINE,
     *     DEGRAD,HSIGMA,DAYSEC,RCO2,
     *     CAO3SW(5),CAH2SW(5),CBSW(5)
      COMMON /ASTSAV/ SOLC,RSIN1,RCOS1,RCOS2
C====>    BEGIN HERE             .......................
C
C         SOLC,THE SOLAR CONSTANT IS SCALED TO A MORE CURRENT VALUE.
C          I.E. IF SOLC=2.0 LY/MIN THEN SSOLAR=1.96 LY/MIN.
C..     RE-COMPUTED CAUSE SSOLAR OVERWRITTEN AS PART OF SCRATCH COMMON
C
C******ZHAO
C  NOTE: XLAT IS IN DEGREE HERE
C*****ZHAO
      YEAR=365.25
      RLAG=14.8125
      TPI=6.283185308
      PI=3.1415927
      SC=2.
      SOLC=SC/(R1*R1)
C*****************************
C Special note: The solar constant is reduced extra 3 percent to account
C               for the lack of aerosols in the shortwave radiation
C               parameterization.       Q. Zhao    96-7-23
C****************************
      SSOLAR=SOLC*HP98
      SSOLAR=SSOLAR*0.97
      DATE=JD+GMT/24.0
      RANG=TPI*(DATE-RLAG)/YEAR
      RSIN1=SIN(RANG)
      RCOS1=COS(RANG)
      RCOS2=COS(2.0*RANG)
      DO 40 I=MYIS,MYIE
        IR = I + IBEG - 1
        TH2=HP2*XLAT(IR)
        JJROW(I)=Q19001-TH2
        TTHAN(I)=(19-JJROW(I))-TH2
C.....  NOTE THAT THE NMC VARIABLES ARE IN MKS (THUS PRESSURE IS IN
C          CENTIBARS)WHILE ALL GFDL VARIABLES ARE IN CGS UNITS
        SFCALB(I) = ALBEDO(IR)
C.....  NOW PUT SFC TEMP,PRESSURES, ZENITH ANGLE INTO SW COMMON BLOCK...
C***ZHAO
C  NOTE: ALL PRESSURES INPUT FROM THE ETA MODEL ARE IN PA
C        THE UNIT FOR PRESS IS MICRO BAR 
C        SURFACE TEMPERATURE ARE NEGATIVE OVER OCEANS IN THE ETA MODEL
C***ZHAO
        PRESS(I,LP1)=QS(IR)*10.0
        TEMP(I,LP1)=ABS(TSFC(IR))
c       if(mype.eq.13.and.i.eq.40) then
c       if(mype.eq.13) then
c         print*,'ir,i,lp1=',ir,i,lp1
c         print*,'tsfc(ir)=',tsfc(ir)
c       endif
        COSZEN(I) = COSZRO(IR)
        TAUDA(I) = TAUDAR(IR)
   40 CONTINUE
C***ZHAO
C.....  ALL GFDL VARIABLES HAVE K=1 AT THE TOP OF THE ATMOSPHERE.NMC
C       ETA MODEL HAS THE SAME STRUCTURE
C***ZHAO
      DO 50 K=1,L
       DO 50 I=MYIS,MYIE
        IR = I + IBEG - 1
C.....  NOW PUT TEMP,PRESSURES, INTO SW COMMON BLOCK..........
        TEMP(I,K) = TT(IR,K)
        PRESS(I,K) = 10.0 * PP(IR,K)
C.... STORE LYR MOISTURE AND ADD TO SW COMMON BLOCK
        RH2O(I,K)=QQH2O(IR,K)
        IF(RH2O(I,K).LT.H3M6) RH2O(I,K)=H3M6
   50 CONTINUE
C...    *************************
      IF (KO3.EQ.0) GO TO 65
C...    *************************
      DO 60 K=1,L
       DO 60 I=MYIS,MYIE
        QO3(I,K) = O3QO3(I+IBEG-1,K)
   60 CONTINUE
   65 CONTINUE
C...   ************************************
      IF (KALB.GT.0) GO TO 110
C...   ************************************
C..... THE FOLLOWING CODE GETS ALBEDO FROM PAYNE,1972 TABLES IF
C         1) OPEN SEA POINT (SLMSK=1);2) KALB=0
      IQ=INT(TWENTY*HP537+ONE)
      DO 105 I=MYIS,MYIE
         IF(COSZEN(I).GT.0.0 .AND. SLMSK(I+IBEG-1).GT.0.5) THEN
           ZEN=DEGRAD*ACOS(MAX(COSZEN(I),0.0))
           IF(ZEN.GE.H74E1) JX=INT(HAF*(HNINETY-ZEN)+ONE)
           IF(ZEN.LT.H74E1.AND.ZEN.GE.FIFTY)
     1        JX=INT(QUARTR*(H74E1-ZEN)+HNINE)
           IF(ZEN.LT.FIFTY) JX=INT(HP1*(FIFTY-ZEN)+H15E1)
           DZEN=-(ZEN-ZA(JX))/DZA(JX)
           ALB1=ALBD(IQ,JX)+DZEN*(ALBD(IQ,JX+1)-ALBD(IQ,JX))
           ALB2=ALBD(IQ+1,JX)+DZEN*(ALBD(IQ+1,JX+1)-ALBD(IQ+1,JX))
           SFCALB(I)=ALB1+TWENTY*(ALB2-ALB1)*(HP537-TRN(IQ))
         ENDIF
  105 CONTINUE
  110 CONTINUE
C        **********************************
      IF (KO3.GT.0) GO TO 135
C        **********************************
C.... COMPUTE CLIMATOLOGICAL ZONAL MEAN OZONE,
C....   SEASONAL AND SPATIAL INTERPOLATION DONE BELOW.
      DO 130 K=1,L
        DO 125 I=MYIS,MYIE
          DO3V(I)  = DDUO3N(JJROW(I),K) + RSIN1*DDO3N2(JJROW(I),K)
     1                +RCOS1*DDO3N3(JJROW(I),K)
     2                +RCOS2*DDO3N4(JJROW(I),K)
          DO3VP(I) = DDUO3N(JJROW(I)+1,K) + RSIN1*DDO3N2(JJROW(I)+1,K)
     1               +RCOS1*DDO3N3(JJROW(I)+1,K)
     2               +RCOS2*DDO3N4(JJROW(I)+1,K)
C...   NOW LATITUDINAL INTERPOLATION, AND
C          CONVERT O3 INTO MASS MIXING RATIO(ORIGINAL DATA MPY BY 1.E4)
          QO3(I,K) = H1M4 * (DO3V(I)+TTHAN(I)*(DO3VP(I)-DO3V(I)))
  125   CONTINUE
  130 CONTINUE
  135 CONTINUE
C.............
      DO 195 I=MYIS,MYIE
C.....     VISIBLE AND NEAR IR DIFFUSE ALBEDO
        ALVD(I) = SFCALB(I)
        ALND(I) = SFCALB(I)
C.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO
        ALVB(I) = SFCALB(I)
        ALNB(I) = SFCALB(I)
C.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO,IF NOT OCEAN NOR SNOW
C            ..FUNCTION OF COSINE SOLAR ZENITH ANGLE..
        IF (SLMSK(I+IBEG-1).LT.0.5) THEN
         IF (SFCALB(I).LE.0.5) THEN
          ALBD0 = -18.0 * (0.5 - ACOS(COSZEN(I))/PI)
          ALBD0 = EXP (ALBD0)
          ALVD1 = (ALVD(I) - 0.054313) / 0.945687
          ALND1 = (ALND(I) - 0.054313) / 0.945687
          ALVB(I) = ALVD1 + (1.0 - ALVD1) * ALBD0
          ALNB(I) = ALND1 + (1.0 - ALND1) * ALBD0
         END IF
        END IF
  195 CONTINUE
C.....SURFACE VALUES OF RRCL AND TTCL
      DO 200 N=1,2
        DO 200 I=MYIS,MYIE
      RRCL(I,N,1)=ALVD(I)
      TTCL(I,N,1)=ZERO
  200 CONTINUE
      DO 220 N=3,NB
      DO 220 I=MYIS,MYIE
         RRCL(I,N,1)=ALND(I)
         TTCL(I,N,1)=ZERO
  220 CONTINUE
C...     **************************
C...     *  END OF CLOUD SECTION  *
C...     **************************
C... THE FOLLOWING CODE CONVERTS RRVCO2,THE VOLUME MIXING RATIO OF CO2
C   INTO RRCO2,THE MASS MIXING RATIO.
      RRVCO2=RCO2
      RRCO2=RRVCO2*RATCO2MW
  250 IF(ITIMLW .EQ. 0) GO TO 300
C
C             ***********************
C====>        * LONG WAVE RADIATION *
C             ***********************
C
C....     ACCOUNT FOR REDUCED EMISSIVITY OF ANY CLDS
      DO 240 K=1,LP1
      DO 240 I=MYIS,MYIE
        EQCMT(I,K)=CAMT(I,K)*EMCLD(I,K)
  240 CONTINUE
C....    GET CLD FACTOR FOR LW CALCULATIONS
C....
      CALL CLO89(CLDFAC,EQCMT,NCLDS,KBTM,KTOP)
C===>        LONG WAVE RADIATION
      CALL LWR88(HEATRA,GRNFLX,TOPFLX,
     1           PRESS,TEMP,RH2O,QO3,CLDFAC,
     2           EQCMT,NCLDS,KTOP,KBTM)
C....
      DO 280 I=MYIS,MYIE
        IR = I + IBEG - 1
        FLWUP(IR) = TOPFLX(I) * .001E0
        GRNFLX(I)=Q14330*(HSIGMA*TEMP(I,LP1)**4-GRNFLX(I))
C.... GET LW FLUX DOWN AND UP AT GROUND(WATTS/M**2) - GRNFLX=LW DOWN.
        FLWDNS(IR)=GRNFLX(I)/(1.43306E-06*1000.E0)
        FLWUPS(IR)=HSIGMA*.001E0 * TEMP(I,LP1)**4
  280 CONTINUE
C....      CONVERT HEATING RATES TO DEG/SEC
      DO 290 K=1,L
        DO 290 I=MYIS,MYIE
c         if(mype.eq.13.and.(i+ibeg-1).eq.40.and.j.eq.77) then
c         if(mype.eq.13) then
c            print*,'ibeg,i,i+ibeg-1=',ibeg,i,i+ibeg-1
c            print*,'i,k,heatra(i,k)=',i,k,heatra(i,k)
c         endif
          HLW(I+IBEG-1,K)=HEATRA(I,K)*DAYSEC
  290 CONTINUE
  300 CONTINUE
      IF(ITIMSW .EQ. 0) GO TO 350
CSW
      CALL SWR93(FSW,HSW,UF,DF,FSWL,HSWL,UFL,DFL,
     1           PRESS,COSZEN,TAUDA,RH2O,RRCO2,SSOLAR,QO3,
     2           NCLDS,KTOP,KBTM,CAMT,RRCL,TTCL,
     3           ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND)
CSW
C
C.....    GET SW FLUXES IN WATTS/M**2
      DO 320 I=MYIS,MYIE
       IR = I + IBEG - 1
       FSWUP(IR) = UF(I,1) * 1.E-3
       FSWDN(IR) = DF(I,1) * 1.E-3
       FSWUPS(IR) = UF(I,LP1) * 1.E-3
CC..COUPLE W/M2 DIFF, IF FSWDNS(IR)=DF(I,LP1)*1.#E-3
       FSWDNS(IR) = (GDFVB(I)+GDFNB(I)+GDFVD(I)+GDFND(I)) * 1.E-3
C...    DOWNWARD SFC FLUX FOR THE SIB PARAMETERATION
C.....     VISIBLE AND NEAR IR DIFFUSE
       GDFVDR(IR) = GDFVD(I) * 1.E-3
       GDFNDR(IR) = GDFND(I) * 1.E-3
C.....     VISIBLE AND NEAR IR DIRECT BEAM
       GDFVBR(IR) = GDFVB(I) * 1.E-3
       GDFNBR(IR) = GDFNB(I) * 1.E-3
  320 CONTINUE
C....      CONVERT HEATING RATES TO DEG/SEC
      DO 330 K=1,L
        DO 330 I=MYIS,MYIE
          SWH(I+IBEG-1,K)=HSW(I,K)*DAYSEC
  330 CONTINUE
  350 CONTINUE
      RETURN
 1000 FORMAT(1H ,' YOU ARE CALLING GFDL RADIATION CODE FOR',I5,' PTS',
     1           'AND',I4,' LYRS,WITH KDAPRX,KO3,KCZ,KEMIS,KALB = ',5I2)
      END
