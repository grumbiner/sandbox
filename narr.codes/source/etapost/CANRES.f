      SUBROUTINE CANRES(SOLAR,SFCTMP,Q2,SFCPRS,SMC,
     &                  GC,RC,IVEG,ISOIL,
     &                  RSMIN,NROOTS,SMCWLT,SMCREF,
     &                  RCS,RCQ,RCT,RCSOIL)
!      IMPLICIT NONE

C ######################################################################
C                        SUBROUTINE CANRES
C                        -----------------
C       THIS ROUTINE CALCULATES THE CANOPY RESISTANCE WHICH DEPENDS ON
C       INCOMING SOLAR RADIATION, AIR TEMPERATURE, ATMOSPHERIC WATER
C       VAPOR PRESSURE DEFICIT AT THE LOWEST MODEL LEVEL, AND SOIL
C       MOISTURE (PREFERABLY UNFROZEN SOIL MOISTURE RATHER THAN TOTAL)
C ----------------------------------------------------------------------
C        SOURCE:  JARVIS (1976), JACQUEMIN AND NOILHAN (1990 BLM)
C ----------------------------------------------------------------------
C PROGRAM HISTORY LOG:
C   03-01-17  M EK AND H CHUANG - LIFTED IT FROM MODEL FOR POST 
C ----------------------------------------------------------------------
C INPUT:  SOLAR: INCOMING SOLAR RADIATION
C 	  CH:	  SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
C 	  SFCTMP: AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
C 	  Q2:	  AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C 	  Q2SAT:  SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C 	  SFCPRS: SURFACE PRESSURE
C 	  SMC:    VOLUMETRIC SOIL MOISTURE 
C 	  ZSOIL:  SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
C 	  NSOIL:  NO. OF SOIL LAYERS
C 	  IROOT:  NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
C 	  XLAI:   LEAF AREA INDEX
C 	  SMCWLT: WILTING POINT
C 	  SMCREF: REFERENCE SOIL MOISTURE
C 		  (WHERE SOIL WATER DEFICIT STRESS SETS IN)
C
C RSMIN, RSMAX, TOPT, RGL, HS: CANOPY STRESS PARAMETERS SET IN
C SUBROUTINE REDPRM
C
C  (SEE EQNS 12-14 AND TABLE 2 OF SEC. 3.1.2 OF 
C       CHEN ET AL., 1996, JGR, VOL 101(D3), 7251-7268)               
C
C        OUTPUT:  PC: PLANT COEFFICIENT
C                 RC: CANOPY RESISTANCE
C                 GC: CANOPY CONDUCTANCE
C ----------------------------------------------------------------------
C ######################################################################

      INCLUDE "parmeta"
      INCLUDE "parmsoil"
      INCLUDE "params"
      INTEGER K
!      INTEGER NROOT(13)
      INTEGER IROOT(13)
      INTEGER NSOIL
      INTEGER IVEG
      INTEGER ISOIL

!      REAL SIGMA, RD, CP, SLV
      REAL SOLAR, CH, SFCTMP, Q2, SFCPRS 
      REAL SMC(NSOIL), ZSOIL(NSOIL), PART(NSOIL) 
      REAL SMWLT(9),SMREF(9),RSMN(13),RC,PC,Q2SAT,GC
      REAL TOPT,RSMAX,RGL(13),HS(13),XLAI,RCS,RCT,RCQ,RCSOIL,FF
      REAL P,QS,GX,TAIR4,ST1,SLVCP,RR,DELTA

!      PARAMETER (CP=1004.5, SLV=2.501000E6)

      DATA XLAI /4.0/
      DATA RSMAX /5000./
      DATA TOPT /298.0/
      DATA IROOT /4,4,4,4,4,4,3,3,3,2,3,3,2/
      DATA ZSOIL /-0.1,-0.4,-1.0,-2.0/
C  SSIB VEGETATION TYPES (DORMAN AND SELLERS, 1989; JAM)
C
C   1:   BROADLEAF-EVERGREEN TREES  (TROPICAL FOREST)
C   2:   BROADLEAF-DECIDUOUS TREES
C   3:   BROADLEAF AND NEEDLELEAF TREES (MIXED FOREST)
C   4:   NEEDLELEAF-EVERGREEN TREES
C   5:   NEEDLELEAF-DECIDUOUS TREES (LARCH)
C   6:   BROADLEAF TREES WITH GROUNDCOVER (SAVANNA)
C   7:   GROUNDCOVER ONLY (PERENNIAL)
C   8:   BROADLEAF SHRUBS WITH PERENNIAL GROUNDCOVER
C   9:   BROADLEAF SHRUBS WITH BARE SOIL
C  10:   DWARF TREES AND SHRUBS WITH GROUNDCOVER (TUNDRA)
C  11:   BARE SOIL
C  12:   CULTIVATIONS (THE SAME PARAMETERS AS FOR TYPE 7)
C  13:   GLACIAL (THE SAME PARAMETERS AS FOR TYPE 11)
      DATA RSMN /150.0, 100.0, 125.0, 150.0, 100.0, 70.0,
     *             40.0, 300.0, 400.0, 150.0, 400.0, 40.0,
     *            150.0/
      DATA RGL /30.0,  30.0,  30.0,  30.0,  30.0,  65.0,
     *          100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
     *          100.0/
      DATA HS /41.69, 54.53, 51.93, 47.35,  47.35, 54.53,
     *         36.35, 42.00, 42.00, 42.00,  42.00, 36.35,
     *         42.00/
C SOIL TYPES   ZOBLER (1986)      COSBY ET AL (1984) (quartz cont.(1))
C  1        COARSE            LOAMY SAND         (0.82)
C  2        MEDIUM            SILTY CLAY LOAM    (0.10)
C  3        FINE              LIGHT CLAY         (0.25)
C  4        COARSE-MEDIUM     SANDY LOAM         (0.60)
C  5        COARSE-FINE       SANDY CLAY         (0.52)
C  6        MEDIUM-FINE       CLAY LOAM          (0.35)
C  7        COARSE-MED-FINE   SANDY CLAY LOAM    (0.60)
C  8        ORGANIC           LOAM               (0.40)
C  9        GLACIAL LAND ICE  LOAMY SAND         (NA using 0.82)
      DATA SMREF /0.283, 0.387, 0.412, 0.312, 0.338, 0.382,
     &             0.315, 0.329, 0.283/
      DATA SMWLT /0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &             0.069, 0.066, 0.029/

C ----------------------------------------------------------------------
C INITIALIZE CANOPY CONDUCTANCE TERMS
C ----------------------------------------------------------------------
      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

C ----------------------------------------------------------------------
C SET SMCWLT, SMCREF, RSMIN, NROOTS VALUES
C ----------------------------------------------------------------------
      SMCWLT = SMWLT(ISOIL)
      SMCREF = SMREF(ISOIL)
      RSMIN = RSMN(IVEG)
      NROOTS = IROOT(IVEG)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
C ----------------------------------------------------------------------

      FF = 0.55*2.0*SOLAR/(RGL(IVEG)*XLAI)
      RCS = (FF + RSMIN/RSMAX) / (1.0 + FF)

      RCS = MAX(RCS,0.0001)
      RCS = MIN(RCS,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
C ----------------------------------------------------------------------

      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)

      RCT = MAX(RCT,0.0001)
      RCT = MIN(RCT,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
C ----------------------------------------------------------------------

c      P = SFCPRS
C Insert QSAT computation used in ETA2P
      TBLO=SFCTMP
      Q2SAT=PQ0/SFCPRS*EXP(A2*(TBLO-A3)/(TBLO-A4)) 
      QS = Q2SAT
C RCQ EXPRESSION FROM SSIB 
      RCQ = 1.0/(1.0+HS(IVEG)*(QS-Q2))

c      RCQ = MAX(RCQ,0.01)
      RCQ = MAX(RCQ,0.0001)
      RCQ = MIN(RCQ,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
C DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
C ----------------------------------------------------------------------

      GX = (SMC(1)-SMCWLT)/(SMCREF-SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

C####   USING SOIL DEPTH AS WEIGHTING FACTOR
      PART(1) = (ZSOIL(1)/ZSOIL(NROOTS)) * GX

C#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
CC      PART(1) = RTDIS(1) * GX
      
      DO K = 2, NROOTS
        GX = (SMC(K)-SMCWLT)/(SMCREF-SMCWLT)
        IF (GX .GT. 1.) GX = 1.
        IF (GX .LT. 0.) GX = 0.
C####   USING SOIL DEPTH AS WEIGHTING FACTOR        
        PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOTS)) * GX

C#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
CC         PART(K) = RTDIS(K) * GX 
               
      END DO

      DO K = 1, NROOTS
        RCSOIL = RCSOIL+PART(K)
      END DO

      RCSOIL = MAX(RCSOIL,0.0001)
      RCSOIL = MIN(RCSOIL,1.0)

C ----------------------------------------------------------------------
C         DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.
C         CONVERT CANOPY RESISTANCE (RC) TO PLANT COEFFICIENT (PC).
C ----------------------------------------------------------------------

CC/98/01/05/........RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)
c      RC = RCMIN(IVEG)/(XLAI*RCS*RCT*RCQ*RCSOIL)

      RCMIN = RSMIN/XLAI
      RCMAX = RSMAX/XLAI
      RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)

      RC = MAX(RCMIN,MIN(RC,RCMAX))

      GC = 1./RC
      
      RETURN
      END
