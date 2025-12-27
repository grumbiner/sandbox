       PROGRAM GRDETA
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: GRDETA        INTERPOLATES FROM ANLYSIS TO MODEL GRID
C   PRGMMR: ROGERS           ORG: NP22        DATE: 97-12-31  
C
C ABSTRACT: INTERPOLATES FROM THE ANALYSIS GRID TO THE MODEL GRID
c   AND CREATES INITIAL CONDITIONS AND RESTART FILE FOR THE ETA MODEL.
C   THIS VERSION, CREATED TO RUN OFF REANALYSIS SURFACE FILES (SST,SNOW,
C   SEA ICE), USES THE 2 DEG LAT/LON SNOW MASK FILE, WHICH WAS REPLACED
C   BY THE 1 DEG LAT/LON SNOW DEPTH FILE IN OCTOBER 1998.
C
C PROGRAM HISTORY LOG:
C   94-07-25  DENNIS DEAVEN
C   97-02-07  MITCHELL/PETERS/BALDWIN/CHEN/ROGERS: 
C              1) MODIFIED TO READ CRAY USAF/SAB SNOW/ICE ANALYSES
C              2) ADDED DIAGNOSTIC PLOT OF SNOW FIELDS
C              3) MODIFIED TO READ GRIB GLOBAL 1 DEG LAT/LON SST
C              4) MODIFIED TO USE 0.125 DEG LAT/LON FILE OF
C                 VEGETATION FRACTION
C   97-09-01  Y. LIN MODIFIED CODE TO USE GRIB VEG FILE
C   98-06-05  MIKE BALDWIN - CONVERT TO 2-D CODE
C   98-12-04  ERIC ROGERS - MODIFIED TO RUN ETA OFF NCEP REANALYSIS
C             USING 2 DEG LAT/LON SNOW AND GRUMBINE 0.5 DEG LAT/LON
C             SEA ICE. THIS CODE WILL WORK FOR REANALYSIS SURFACE
C             FILES FROM 1 JANUARY 1996 - 30 SEPTEMBER 1998.
C
C USAGE:
C   INPUT FILES:
C     FORT.11  - GESGRD, FIRST GUESS ON ANALYSIS GRID
C     FORT.13  - GRDSPECS, FILE CONTAINING ANALYSIS GRID SPECIFICATIONS
C     FORT.14  - OROGRP80, TERRAIN AND STEPS ON MODEL GRID
C     FORT.15  - GRDETA.PARM, NAMELIST INPUT
C     FORT.20  - AVLM00 - NGM MOISTURE AVAILABILITY
C     FORT.21  - ALB100 - GLOBAL ALBEDO
C     FORT.20  - ALB200 - "           "
C     FORT.23  - ALB300 - "           "
C     FORT.24  - ALB400 - "           "
C     FORT.30  - IVGTYP_1D - VEGETATION TYPE
C     FORT.31  - ISLTYP_1d - SOIL TYPE
C     FORT.32  - ISLOPE_1D - SURFACE SLOPE
C     FORT.33  - SFCANL - GDAS SURFACE ANALYSIS (NEEDED FOR SOIL TEMP/MOISTURE)
C     FORT.34  - SST.INDEX - NCEP REANALYSIS SST GRIB INDEX FILE
C     FORT.35  - SST - NCEP REANALYSIS SST ANALYSIS FILE IN GRIB
C     FORT.36  - SNOW.INDEX - NCEP REANALYSIS SNOW GRIB INDEX FILE
C     FORT.37  - SNOW - NCEP REANALYSIS SNOW MASK FILE IN GRIB
C     FORT.38  - ICE.INDEX - NCEP REANALYSIS SEA ICE GRIB INDEX FILE
C     FORT.39  - ICE - NCEP REANALYSIS SEA ICE FILE IN GRIB
C     FORT.40  - VEG.ETA - 0.125 DEG GLOBAL FILE OF VEGETATION FRACTION
C  
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C
C     FORT.51  - ETA8000 - SCRATCH FILE
C     FORT.52  - DATE - DATE
C     FORT.53  - NFC8000 - "COLD START" GUESS FILE 
C     FORT.54  - NHB8000 - ETA FIXED FIELD FILES
C     FORT.55  - NBC8000 - FIRST GUESS ON ETA GRID BOUNDARY
C     FORT.56  - RESTRT03 - FIRST GUESS RESTART FILE USED AS INPUT TO ANALYSIS
C     FORT.06  - GRDETA.OUT, DIAGNOSTIC TEXT PRINTED OUTPUT
C
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - CNSTS ECONVK ETALL FLIP INTERP GRIBST
C                  ROTLLE SNOHIRES SPLINE SSTHIRES TABLE TABLEQ 
C                  ETA2ETA GRD2ETA GRDETA SST14K ZTERPG C2K SNO16GET SNO8GET
C                  GAULAT GAUTOETA GAUTOICE GETGB BSSLZ1 PUTEM PUTVEG
C                  PRINTAF PRINTETA PRINTIMS
C
C     LIBRARY:
C       W3LIB    - W3FS17,W3FI47,W3FI33,W3AI01,W3FP06,W3FB05
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN90
C   MACHINE:   CRAY
C
C$$$
C----------------------------------------------------------------------
      INCLUDE "parmeta.h"
C----------------------------------------------------------------------
                             L O G I C A L 
     & SIGMA
C----------------------------------------------------------------------
                             D I M E N S I O N
c    & TVE(IM,JM,LMAX),UE(IM,JM,LMAX),VE(IM,JM,LMAX),QE(IM,JM,LMAX)
     & ZE(IM,JM,LMAX1),REF(IM,JM)
     &,ZSE(IM,JM),SPE(IM,JM),SME(IM,JM),SSTE(IM,JM)
     &,SNOE(IM,JM)
     &,IDATE(4),SL(LMAX),SI(LMAX1),TSE(IM,JM),EREFSE(IM,JM)
C----------------------------------------------------------------------
C
        COMMON /GBLATM/TVE(IM,JM,LMAX),UE(IM,JM,LMAX),VE(IM,JM,LMAX)
     1,     QE(IM,JM,LMAX)
C
       NAMELIST /MODTOP/ ETOP
C
C   FOR TIMING
C
      real*8 timef
      real grd2eta_tim,eta2eta_tim,econvk_tim
C
      CALL W3TAGB('GRDETA  ',1998,0160,0065,'NP22   ')
C
C----------------------------------------------------------------------
      SIGMA=.FALSE.
C----------------------------------------------------------------------
      ETOP=25.0
      READ(5,MODTOP,END=100)
  100 CONTINUE
C----------------------------------------------------------------------
      btim=timef()
      CALL GRD2ETA(ZE,SPE,ZSE,SME,SSTE,SNOE,EREFSE,IDATE
     1,            SIGMA,REF,ETOP)
      grd2eta_tim=timef()-btim
      print *,'grd2eta time = ',grd2eta_tim
C----------------------------------------------------------------------
      btim=timef()
      CALL ETA2ETA(ZE,SPE,ZSE,SME,SSTE,SNOE,EREFSE,IDATE
     1,    SIGMA,REF,ETOP)
      eta2eta_tim=timef()-btim
      print *,'eta2eta time = ',eta2eta_tim
C----------------------------------------------------------------------
      btim=timef()
      CALL ECONVK(idate)
      econvk_tim=timef()-btim
      print *,'econvk time = ',econvk_tim
C----------------------------------------------------------------------
      PRINT*," End of analysis grid to model grid program"
C----------------------------------------------------------------------
      CALL W3TAGE('GRDETA  ')
C
      STOP
      END
