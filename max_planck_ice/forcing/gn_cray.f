C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  FNOC  WE HAVE NOT TOUCHED SUBROUTINES LANDAVE OR ICEOUT.  YOU
C  WILL HAVE TO CHANGE THEM WHEN THE NEW DATA BASE IS SET UP.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
C  This code is the PIPS operational model changed to run on the
c  CRAY.  We have left all the Cyber 205 code as is and only 
c  commented them out.  
c
c      SUBROUTINE LANDAVE(DUM,IDIMX,IDIMY,HEFFM) 
c      IMPLICIT REAL*8 (A-H,O-Z)
C************************************************* 
C  R. MICHAEL CLANCY, NRL CODE 322, STENNIS SPACE CENTER, MS  39529 
C************************************************* 
C  THIS ROUTINE ASSIGNS VALUES TO LAND AND ICE-COVERED 
C     POINTS IN A FIELD TO BE OUTPUT TO THE VARIAN PLOTTER.  THIS 
C     MUST BE DONE TO INSURE THAT CONTOURS OF THE FIELD PRODUCED 
C     BY PROGRAM VARIMAP CUT ACROSS THE COASTLINE IN A SMOOTH 
C     AND CONSISTENT FASHION.  VALUES AT THE NON-OPEN SEA POINTS ARE  
C     SET TO A WEIGHTED COMBINATION OF VALUES AT THE 
C     NEAREST OPEN SEA POINTS IN THE PLUS X-, MINUS X-, PLUS Y-, AND  
C     MINUS Y-DIRECTIONS (X AND Y DEFINED RELATIVE TO THE GRID). 
C*************************************************** 
c      DIMENSION DUM(80,142),HEFFM(80,142) 
c      DO 2 IY=1,IDIMY 
c      DO 2 IX=1,IDIMX 
C     SELECT NON-OPENSEA POINTS 
c      IF(HEFFM(IX,IY) .GT. 0.0) GO TO 2 
C     SEARCH IN PLUS X-DIRECTION FOR OPEN SEA POINTS 
c3     DO 20 K=1,60  
c      KXP=IX+K 
c      IF(KXP.GT.IDIMX)GO TO 21 
c      IF(HEFFM(KXP,IY).GT. 0.0) GO TO 21 
c20    CONTINUE 
C     SEARCH IN MINUS X-DIRECTION FOR OPEN SEA POINTS 
c21    DO 30 K=1,60  
c      KXM=IX-K 
c      IF(KXM.LT.1)GO TO 31 
c      IF(HEFFM(KXM,IY).GT. 0.0) GO TO 31 
c30    CONTINUE 
C     SEARCH IN PLUS Y-DIRECTION FOR OPEN SEA POINTS 
c31    DO 40 K=1,60  
c      KYP=IY+K 
c      IF(KYP.GT.IDIMY)GO TO 41 
c      IF(HEFFM(IX,KYP).GT. 0.0) GO TO 41 
c40    CONTINUE 
C     SEARCH IN MINUS Y-DIRECTION FOR OPEN SEA POINTS 
c41    DO 50 K=1,60  
c      KYM=IY-K 
c      IF(KYM.LT.1)GO TO 51 
c      IF(HEFFM(IX,KYM).GT. 0.0) GO TO 51 
c50    CONTINUE 
c51    CONTINUE 
C     DEFINE WEIGHTS AS INVERSE OF NUMBER OF GRID INTERVALS FROM 
C         SELECTED OPEN SEA POINTS TO POINT IN QUESTION 
c      WXP=1./FLOAT(KXP-IX) 
c      WXM=1./FLOAT(IX-KXM) 
c      WYP=1./FLOAT(KYP-IY) 
c      WYM=1./FLOAT(IY-KYM) 
c      IF(KXP.LE.IDIMX) GO TO 120 
c      KXP=1 
c      WXP=0.0 
c 120  IF(KXM.GE.1) GO TO 130  
c      KXM=1 
c      WXM=0.0 
c 130  IF(KYP.LE.IDIMY) GO TO 140 
c      KYP=1 
c      WYP=0.0 
c 140  IF(KYM.GE.1) GO TO 150  
c      KYM=1 
c      WYM=0.0 
c 150  CONTINUE 
c      DUM(IX,IY)=(WXP*DUM(KXP,IY)+WXM*DUM(KXM,IY)+WYP*DUM(IX,KYP)+ 
c     1WYM*DUM(IX,KYM))/(WXP+WXM+WYP+WYM+0.0001) 
c2     CONTINUE 
c      RETURN 
c      END 
c      SUBROUTINE ICEOUT( UICE,VICE,HEFF,AREA,PRESS,FHEFF,DIV, 
c     &ITAU,IDT,IFILE,HEFFM,IDELTM,IPRNT) 
c      IMPLICIT REAL*8 (A-H,O-Z)
C*********************************************************************: 
C 
C       KEN POLLAK FNOC MONTEREY CALIFORNIA   JAN 1985 
C 
C********************************************************************** 
C 
C        HIBLER ICE FORECAST MODEL OUTPUT SUBROUTINE. 
C 
C     THIS ROUTINE WILL WRITE THE FORECAST AT THE GIVEN TAU AND DATE- 
C     TIME -GROUP TO A CRANDIO FILE.  THE FIRST WORD ADDRESS IS PASSED 
C     THROUGH THE ARGUMENTS THEN LOADED INTO AN OUTPUT ARRAY.  SINCE THE 
C     U AND V ICE VELOCITY COMPONENTS ARE ON STAGGERED GRIDS, THESE 
C     FIELDS ARE AVERAGED TO THE MODEL OUTPUT GRID.  THIS DONE BY 
C     FIRST DOING A 4-POINT AVERAGE, THEN SIMPLY EQUATING THE OUTSIDE 
C     EDGES OF THE STAGGERED ARRAY WITH THE OUTPUT ARRAY. 
C 
C     INPUTS: 
C 
C UICE,VICE   - U AND V ICE VELOCITY IN M/SEC 
C      HEFF   - ICE THICKNESS IN M 
C      AREA   - ICE CONCENTRATION GIVEN FROM 0 TO 1 
C     PRESS   - ICE STRENGTH IN NT/M 
C     FHEFF   - ICE GROWTH IN M/DAY 
C       DIV   - ICE DIVERGENCE IN 1/SEC  UNITS 
C      ITAU   - FORECAST TIME OF ATMOSPHERIC FORCING (USE A TAU=0 
C               TO OBTAIN 24 HOUR FORECAST, TAU24 FOR 48 HOUR FORE- 
C               CAST, ETC. 
C      IDT    - ASCII DATE TIME GROUP FROM CALL TO DDTG 
C    IFILE    - CRANDIO  OUTPUT IFILENAME 
C     IDELTM   - MODEL TIME STEP IN HOURS 
C     IPRNT    - IF 0,  GRID PRINT FIELD. 
C 
C     OUTPUTS: 
C 
C        CRANDIO RECORDS TO FILE IFILE  
C 
C     OTHER DEFINITIONS: 
C 
C     FCSTOUT   - OUTPUT ARRAY IN COMMON BUFFER REQUIRED BY CWRITER 
C     IDENT     - 24 WORD IDENT IN COMMON BUFFER  
C  IFIX,IGRD,IHEM,  
C  IPRJ,IUNITS  - ENCODED INTO WORD 6 OF IDENT 
C     IFLP      - FLAPS CHARACTER 
C     INDX      - ADDRESS IN MODEL GRID TO PLACE MEANED STAGGERED GRID. 
C     IPAK      - ZRANDIO PACKING INDICATOR FOR XFRCIO:  SET TO 31 FOR 
C                 FLOATING POINT UNPACKED/UNSCALED CONVERSION, IE., 
C                 CALL TO FXFL NOT REQUIRED ON FRONT END AFTER ZREADER. 
C     IREC      - CRANDIO RECORD NAMES  
C     ISCALE    - CDC 6500 SCALE FACTOR.  NOTE: SINCE ZREADER DOES 
C                 NOT PACK OR UNPACK NON STANDARD LENGTH RECORDS, THE 
C                 FIELDS ARE NOT SCALED OR PACKED ON THE 205. THIS 
C                 IS INCLUDED IN IDENT JUST FOR COMPLETENESS. 
C ISTAT,ISTATUS - CRANDIO STATUS FLAGS  
C     JTAU      - ICE FORECAST TIME EQUALS ITAU + MODEL TIME STEP 
C     LBL       - 2 WORD ARRAY FOR CRANDIO LABEL  
C     LEN24     - FIELD LENGTH PLUS 24 WORDS FOR IDENT 
C     LENGTH    - FIELD LENGTH 
C     PLT       - PLOT CONSTANTS  FOR CONTOURING FIELD 
C     PNAME     - PARAMETER NAME OF FIELD 
C     TEMP      - TEMPORARY ARRAY USED IN UNSTAGGERING SECTION 
C     TITLE     - FIELD TITLE 
C 
C**********************************************************************: 
c      DIMENSION M(7),N(7),ISCALE(7),IUNITS(7),PNAME(7),IREC(7),PLT(4,7) 
c     1  ,LBL(2),TITLE(5),TEMP(79,141),UICE(1),VICE(1),HEFF(1), 
c     2   AREA(1),PRESS(1),FHEFF(1),DIV(1),HEFFM(1) 
c      DIMENSION UDRFT(11139),VDRFT(11139),T(4) 
c      COMMON /BUFFER/ IDENT(24) , FCSTOUT(34280) 
c      DATA IREC/3HP02, 3HP03, 3HP00, 3HP01, 3HP05, 3HP06, 3HP04/ 
c      DATA ISCALE/3*40,46,27,43,47/ 
c      DATA IUNITS /2*2H14, 2H 3, 2H16, 2H52, 2H53, 2H51/ 
c      DATA IFLP/    1H+    /  
c      DATA IPRJ/1H0   /,  IHEM/1H0   /,  IGRD/  2H 2   / 
c      DATA PLT/   0.05,   0.,   0.,   50., 
c     1            0.05,   0.,   0.,   50., 
c     2            0.5,    0.,   0.,   5.0, 
c     3            0.2,    0.,   0.,   20., 
c     4          10000.,   0.,   0.,   10., 
c     5            0.1,    0.,   0.,   10., 
c     6        .000001,    0.,   0.,   10.  / 
c      DATA PNAME/8HUICE VEL,  8HVICE VEL,   8HICE THIK,   8HICE CONC, 
c     +           8HICE PRES,  8HICE GRTH,   8HICE DIVR/ 
c      DATA TITLE/8HHIBLER I,  8HCE MODEL,   8H ARCTIC ,   8HREGIONAL, 
c     +           8H FORECST/  
c      DATA M/2*79,  5*80/,   N/  2*141,   5*142/ 
c      DATA ISET/0/  
C 
C       COMPUTE CONSTANTS 
c      JTAU=ITAU+IDELTM 
c      LEN24 = M(3) * N(3) + 24 
c      IF(ISET  .EQ.  1) GO TO 10 
c      ISET = 1 
c      M1=M(3) 
c      M2=M1-1 
c      N1=N(3) 
c      N2=N1-1 
c      LENUV = M(1) * N(1) 
c      UDRFT(1;LENUV) = 0. 
c      VDRFT(1;LENUV) = 0. 
c      RMTNM = 1852. 
c      SCNM=3600. * FLOAT(IDELTM)/RMTNM  
c      PRINT 8950, M(1), N(1), LENUV, SCNM 
c8950  FORMAT('  VELOCITY GRID DIMENSIONS, LENGTH = ',3I8, 
c     &/,'   ','CONVERSION FROM M/S TO NAUTICAL MILES/TIME STEP=',F10.2) 
c 10   CONTINUE 
C 
C  ACCUMULATE ICE VELOCITY AND CONVERT TO NAUTICAL MILES/DAY 
C   OUTPUT WILL BE NAUTICAL MILES DRIFT FOR TAU INDICATED.  
C 
c      IF(JTAU .EQ. 0) GO TO 12 
c      UDRFT(1;LENUV) = UDRFT(1;LENUV) + UICE(1;LENUV) * SCNM 
c      VDRFT(1;LENUV) = VDRFT(1;LENUV) + VICE(1;LENUV) * SCNM 
c 12   CONTINUE 
C 
C       LOAD FORECAST ARRAY INTO COMMON BLOCK ARRAY FOR CRANDIO OUTPUT 
C 
c      DO 100 K=1,4  
c
C      To stop output p5,p6,p4 by set k=4 instead of k=7
C 
C  FOR JTAU = 0, OUTPUT ONLY HEFF AND AREA 
C 
c      IF((JTAU  .EQ.  0)  .AND.  (K  .LT.  3  .OR. K .GT. 4)) GO TO 100 
c      PRINT 8960, JTAU, PNAME(K) 
c 8960 FORMAT('   JTAU, PNAME = ',I8,3X,A8) 
c      LENGTH = M(K) * N(K) 
c      IF(K  .EQ.  1) TEMP(1,1;LENGTH) = UDRFT(1;LENGTH) 
c      IF(K  .EQ.  2) TEMP(1,1;LENGTH) = VDRFT(1;LENGTH) 
c      IF(K .EQ. 3) FCSTOUT(1;LENGTH) = HEFF(1;LENGTH) 
c      IF(K .EQ. 4) FCSTOUT(1;LENGTH) = AREA(1;LENGTH) 
c      IF(K .EQ. 5) FCSTOUT(1;LENGTH) = PRESS(1;LENGTH) 
c      IF(K .EQ. 6) FCSTOUT(1;LENGTH) = FHEFF(1;LENGTH) 
c      IF(K .EQ. 7) FCSTOUT(1;LENGTH) = DIV(1;LENGTH) 
C 
c      IF(K  .GT.  2)   GO TO 20 
C 
C       UNSTAGGER VELOCITY GRIDS 
C 
C       PRESET ARRAY TO 980 AS A DEBUG DEVICE 
c      FCSTOUT(1;4072) = 980.  
c      KNT=0 
C        CALCULATE 4-POINT MEAN 
c      DO 15 J=2,N2  
c      DO 15 I=2,M2  
c      INDX = (J-1)*M1 + I 
c      T(1) = TEMP(I,J) 
c      T(2) = TEMP(I-1,J) 
c      T(3) = TEMP(I,J-1) 
c      T(4) = TEMP(I-1,J-1) 
c      LNM = 0 
C  EXCLUDE ICE FREE SEA FROM 4-POINT AVERAGE 
c       DO 14 LL=1,4 
c 14    IF(  T(LL)  .NE.  0.  )  LNM = LNM + 1 
c      IF(LNM .EQ. 0) LNM = 1  
c      FCSTOUT(INDX) = ( T(1) + T(2) + T(3) + T(4) )/LNM 
c      IF(LNM .NE. 4) KNT = KNT + 1 
c 15   CONTINUE 
c      PRINT 8962,KNT 
c 8962 FORMAT('   ',I5,' POINTS IN OR NEAR ICE-FREE SEAS ')  
C 
C     END UNSTAGGERING SECTION 
C 
c 20   CONTINUE 
C   FILL IN LAND POINTS (SO PLOTTED CONTOURS LOOK GOOD) 
c      CALL LANDAVE(FCSTOUT,M1,N1,HEFFM) 
C 
C   EQUATE OUTSIDE EDGES 
c      DO 16 I=1,M1  
C  BOTTOM ROW 
c      FCSTOUT(I)=FCSTOUT(I+M1) 
C  TOP ROW 
c      INDX1=I+(N1-1)*M1 
c      INDX2=I+(N1-2)*M1 
c 16   FCSTOUT(INDX1)=FCSTOUT(INDX2) 
c      DO 17 J=2,N2  
C  LEFT COLUMN 
c      INDX1=(J-1)*M1 
c      FCSTOUT(INDX1+1)=FCSTOUT(INDX1+2) 
C  RIGHT COLUMN 
c 17   FCSTOUT(INDX1+M1)=FCSTOUT(INDX1+M1-1) 
C 
C       COMPUTE SYSTEM LABEL AND FILL IN 24 WORD IDENT 
C 
c      CALL SYSLBLC(IREC(K),IDT,JTAU,IFLP,LBL) 
C 
C       PRESET IDENT TO ALL BLANKS THEN INSERT INFORMATION  
c      IDENT(1;24) = 8H 
c      IDENT(1;2) = LBL(1;2) 
c      ENCODE(8,8000,IDENT(3)) LEN24 
c 8000 FORMAT(I8) 
c      IDENT(4) = 8HFP6400UY 
c      IFIX = 48 - ISCALE(K) 
c      ENCODE(8,8005,IDENT(6)) IPRJ,IHEM,IGRD,IFIX,IUNITS(K) 
c 8005 FORMAT(2A1,A2,I2,A2) 
c      ENCODE(8,8010,IDENT(7)) M(K), N(K) 
c 8010 FORMAT(2I4) 
c      IDENT(8) = 8H 32.5 
c      IDENT(9) = 8H 31.0 
c      IDENT(10) = 8H 0.0 
c      IDENT(11) = 8H .32 
c      IDENT(12) = PNAME(K) 
c      IDENT(13;5) = TITLE(1;5) 
C 
C        INSERT PLOT CONSTANTS 
c      DO 30 IW=1,4  
c      ENCODE(8,8015,IDENT(17+IW)) PLT(IW,K) 
c30    CONTINUE 
c 8015 FORMAT(F8.2)  
c      CALL ID203(IDENT(1),999,ISTAT) 
c      IF(IPRNT .EQ. 0) THEN 
C 
C  MAKE GRID PRINTS 
c      CALL QPRNTG(FCSTOUT,LBL(1),LBL(2),0,0,47,25) 
c      PRINT 9000,IFILE,(IDENT(LL),LL=1,21) 
c      ENDIF 
C 
C  OUTPUT TO IFILE  
c      CALL CWRITER(IFILE,LBL,IDENT(1),LEN24,ISTATUS) 
C 
C        WRITE TRANSFER RECORD TO UNIT 11 
C 
c      IPAK = 31 
c      WRITE(11,8050)  LBL,LEN24,IPAK,ISCALE(K) 
c 8050 FORMAT('1ICEFILE  ICEPOOL  2 ',2A8,26X,I7,/, 
c     &'2MASFNWC         ',19X,I2,3X,I2) 
c 100  CONTINUE 
c 9000 FORMAT('    WROTE RECORD TO ',A8,4(/,6(2X,A8)) ) 
c      RETURN 
c      END 
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      FNOC - we have taken out these 205 related cards for unix version
c
c      PROGRAM GRNPIPS(ICERSTRT,TAPE3=ICERSTRT,GRNGRID,TAPE7=GRNGRID, 
c     +        ICECRNT,TAPE9=ICECRNT,ICEFTU,TAPE10=ICEFTU,TAPE20, 
c     +        INPUT,TAPE5=INPUT,FCSTXFR,TAPE11=FCSTXFR,OCHEAT, 
c     +        TAPE4=OCHEAT,ZMBOUN,TAPE14=ZMBOUN) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      PROGRAM GRNPIPS 
C
C*************************START PROLOGUE******************************* 
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                GRNPIPS
C
C  DESCRIPTION:                THIS VERSION OF GRNPIPS WAS BUILT FROM 
C                              THE BARPIPS ICEMDL CODE.  THIS PROGRAM WAS
C                              SUBMITTED DEC 1990.
C
C                              THIS IS THE MAIN DRIVING PROGRAM FOR 
C                              THE HIBLER VISCOUS-PLASTIC SEA ICE MODEL
C                              WHICH IN ITS PRESENT OPERATIONAL FORM
C                              IS CALLED THE POLAR ICE PREDICTION SYSTEM-
C                              GREENLAND SEA (RPIPS-G).
C                              THIS VERSION OF THE CODE HAS    
C                              BEEN ADAPTED TO A CRAY COMPUTER.
C
C  COPYRIGHT:                  N/A     
C
C  CONTRACT NUMBER AND TITLE:  N/A
C
C  REFERENCES:                 FOR A DETAILED DESCRIPTION OF THE MODEL
C                              SEE HIBLER (JOURNAL OF PHYSICAL OCEAN-
C                              OGRAPHY, 1979, MONTHLY WEATHER REVIEW,
C                              1980, PRELLER NORDA REPORT 108, 1985,
C                              AND PRELLER AND POSEY NORDA REPORT 212,
C                              1989.
C
C  CLASSIFICATION:             UNCLASSIFIED
C
C  RESTRICTIONS:               NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:  UNIX 
C
C  USAGE:                      PROGRAM ICEMDL
C
C  PARAMETERS:                 NONE
C
C  COMMON BLOCKS:              THIS MODULE USES THE FOLLOWING COMMON
C                              BLOCKS:
C
C     BLOCK       NAME      TYPE     USAGE             NOTES
C     -----       ----      ----     -----      --------------------
C     /TIMES/     RELAXS    REAL     OUT           TIME SPENT IN RELAX
C                 FORMS     REAL     OUT           TIME SPENT IN FORM
C                 ADVCTS    REAL     OUT           TIME SPENT IN ADVECT
C                 GRWTHS    REAL     OUT           TIME SPENT IN GROWTH
C                 HEATS     REAL     OUT           TIME SPENT IN HEAT
C                 MESHS     REAL     OUT           TIME SPENT IN MESH
C                 INITS     REAL     OUT           TIME SPENT IN ICEINTL
C     /FORCE/     FORCEX    REAL     OUT        
C                 FORCEY    REAL     OUT        
C     /STEP/      DELTAT    REAL     IN
C                 DELTAX    REAL     IN
C                 DELTAY    REAL     IN
C                 DELTA1    REAL     IN
C                 DELTA     REAL     IN
C     /PRESS1/    PRESS     REAL     OUT
C     /GROW/      YNEG      REAL     OUT
C                 FHEFF     REAL     OUT
C     /TGRIDS/    TGRDI     REAL     OUT
C                 TGRDJ     REAL     OUT
C     /TEMP/      TICE      REAL     OUT
C     /OCEANS/    FW        REAL     IN
C     /SNOW/      SNRT      REAL     IN
C     /PRESSUR/   PS        REAL     OUT 
C     /WSTRES/    WSU       REAL     OUT
C                 WSV       REAL     OUT
C     FILES:
C       NAME      UNIT    FILETYPE  ATTRIBUTE  USAGE   DESCRIPTION
C       ----      ----    --------  ---------  -----   -----------
C                   4     PERMANENT SEQUENTIAL   IN    OCEAN FLUXES,FORMATTED
C                  20     PERMANENT SEQUENTIAL   OUT   OUTPUT FOR NRL
C                                                      GRAPHICS, FORMATTED 
C
C  DATA BASES:              NONE
C
C  NON-FILE INPUT/OUTPUT
C       NAME       TYPE       USAGE        DESCRIPTION
C       ----       -----      -----        -----------
C        5          INT        IN          NUMBER OF TIME STEPS, 
C                                           PRINTING,PLOTTING,RESTART
C                                           CODE,DATE-TIME-GROUP
C
C  ERROR CONDITIONS:         NONE
C
C  ADDITIONAL COMMENTS:      NONE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%MAINTENANCE SECTION%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  MODULES CALLED:
C          NAME          DESCRIPTION
C          ----          --------------   
C          DAYNUM        FROM THE DATE-TIME-GROUP IT DETERMINES THE
C                        YEAR, THE MONTH, THE DAY, AND THE HOUR.
C          MESH          DEFINES THE MODEL GRID
C          BNDRY         READS IN THE MODEL LAND-SEA MASKS
C          OCEAN         READS IN THE OCEAN CURRENTS
C          RESTRT        READS IN THE MODEL'S RESTART FIELDS
C          FORM          CALCULATES TERMS FOR THE MOMENTUM EQUATION
C          RELAX         SOLVES THE MOMENTUM EQUATION
C          ADJUST        ESTIMATES THE ICE IN TEH OUTFLOW GRID CELLS
C          INOUT         CHECKS FOR INFLOW/OUTFLOW AT NORTHERN BOUNDARY
C          XSUM          SUMS THE ADJUSTED ICE THICKNESS FROM ALL OF
C                        OF THE GRID CELLS
C          ADVTICE       CALCULATES THE ADVECTION OF ICE THICKNESS
C                        AND COMPACTNESS
C          HEAT          USED TO CREATE MANY OF THE TERMS NEEDED FOR
C                        THE HEAT BUDGET BALANCE
C          GROWTH        CALCULATES THE CHANGE OF THICKNESS AND COMPACTNESS
C          ICEOUT        WRITES OUT THE OUTPUT FIELDS FOR FNOC PLOTTING
C          PRNT          WRITES OUT OUTPUT ARRAYS
C          STATPRT       WRITES OUT TIME SPENT IN EACH ROUTINE 
C
C     LOCAL VARIABLES AND STRUCTURES: N/A ON STRUCTURES
C
C          NAME       TYPE        DESCRIPTION
C          ----       ----       ---------------------- 
C          A22        REAL       MINIMAL COMPACTNESS ALLOWED
C          ADVCTS     REAL       TIME SPENT IN SUBROUTINE ADVECT
C          AMASS      REAL       ICE MASS PER GRID AREA
C          AREA       REAL       FRACTION OF GRID CELL COVERED BY THICK ICE
C          ARSUM      REAL       SUM OVER ALL GRID SQUARES OF ICE COMPACTNESS
C          BHEFF      REAL       ARRAY OF ICE THICKNESS FROM PIPS
C          DELTA1     REAL       FRACTIONAL PART OF AN FNOC GRID SPACE 
C                                WHICH DEFINES THE ICE MODEL Y GRID SPACE
C          DELTA      REAL       FRACTIONAL PART OF AN FNOC GRID SPACE
C                                WHICH DEFINES THE ICE MODEL X GRID SPACE
C          DELTAT     REAL       TIME STEP IN SECONDS
C          DELTAX     REAL       X GRID SPACING IN METERS
C          DELTAY     REAL       Y GRID SPACING IN METERS
C          DELTT      REAL       ALSO TIME STEP ORIGINALLY PLACE IN MODEL
C                                TO DO AN INITIAL 1/2 DELTAT TIME STEP
C                                BUT NOT USED PRESENTLY IN THIS VERSION
C          DIFF1      REAL       HARMONIC DIFFUSION CONSTANT
C          DIV        REAL       ICE DIVERGENCE/CONVERGENCE
C          DRAGA      REAL       ANTISYMMETRIC WATER DRAGE PLUS THE 
C                                CORIOLIS PARAMETER
C          DRAGS      REAL       SYMMETRIC WATER DRAG
C          ERROR      REAL       MAX ERROR ALLOWED IN THE RELATION SCHEME
C          ETA        REAL       NON LINEAR SHEAR VISCOSITY
C          FHEFF      REAL       TOTAL GROWTH RATE OF ICE (THICK PLUS THIN)
C          FHSUM      REAL       SUM OF THICKNESS OVER ALL GRID SQUARES
C          FO         REAL       GROWTH RATE OF THIN ICE
C          FORCEX     REAL       X COMPONENT OF FORCING DUE TO THE OCEAN 
C                                CURRENTS PLUS THE ICE PRESSURE GRADIENT.
C          FORCEY     REAL       Y COMPONENT OF FORCING DUE TO THE OCEAN
C                                CURRENTS PLUS THE ICE PRESSURE GRADIENT. 
C          FORMS      REAL       TIME SPENT IN SUBROUTINE FORM
C          FW         REAL       OCEAN HEAT FLUX
C          GAIRX      REAL       X COMPONENT OF THE WIND
C          GAIRY      REAL       Y COMPONENT OF THE WIND
C          GAREA      REAL       CHANGE OF COMPACTNESS DUE TO FREEZING
C                                OR MELTING
C          GDIFF      REAL       THICK ICE GROWTH COMPONENT PER UNIT AREA
C          GRDI       REAL       X GRID LOCATION FOR DYNAMIC VARIABLES
C          GRDJ       REAL       Y GRID LOCATION FOR DYNAMIC VARIABLES
C          GRSUM      REAL       SUM OVER ALL GRID SQUARES OF GROWTH OF
C                                THIN ICE
C          GWATX      REAL       X COMPONENT OF THE GEOSTROPHIC OCEAN CURRENT
C          GWATY      REAL       Y COMPONENT OF THE GEOSTROPHIC OCEAN CURRENT
C          HCORR      REAL       ADDITIONAL ICE TO BE MELTED FOR MIXED
C                                LAYER BALANCE
C          HDIFF1     REAL       NET GROWTH RATE OF THIN ICE
C          HEATS      REAL       TIME SPENT IN SUBROUTINE HEAT
C          HEFF       REAL       MEAN ICE THICKNESS PER GRID CELL
C          HEFFM      REAL       LAND-SEA MASK FOR THERMODYNAMIC VARIABLES
C          HO         REAL       DEMARCATION BETWEEN THICK AND THIN ICE
C          ICOUNT     INT        COUNTER TO KEEP TRACK OF THE TIME STEPS
C          IDTG       INT        EIGHT CHARACTER DAT-TIME GROUP IN THE
C                                FORM YYMMDDHH
C          IFILE      INT        NAME OF THE CRANDIO FILE (MASFNOC)
C                                CONTAINING THE INPUT FORCING FIELDS
C          INITS      INT        TIME SPENT IN SUBROUTINE ICEINTL
C          ITAU       INT        NUMBER OF HOURS FROM THE START OF THE
C                                FORECAST
C          IRSTRT     INT        USED FOR MODEL INITIALIZATION.  IF IRSTRT 
C                                IS SET TO ANYTHING OTHER THAN 0, THE 
C                                MODEL WILL RESTART FROM CONSTANT INITIAL
C                                CONDITIONS, OTHERWISE IT WILL USE THE
C                                RESTART FIELD PROVIDED
C          ITSTEP     INT        THE NUMBER OF TIME STEPS FOR THIS RUN
C          JFILE      INT        NAME OF THE CRANDIO FILE TO WHICH THE
C                                FORECAST FIELDS ARE WRITTEN
C          LAD        INT        USED IN ADVECT TO DETERMINE THE TIME
C                                STEPPING SCHEME USED.
C          MESHS      INT        TIME SPENT IN SUBROUTINE MESH
C          NX         INT        MASTER COLUMN INDEX DEFINING VELOCITY FIELDS
C          NX1        INT        MASTER COLUMN INDEX DEFINING THERMODYNAMIC
C                                FIELDS
C          NX2        INT        MASTER COLUMN INDEX DEFINING WIND FIELDS
C          NY         INT        MASTER ROW INDEX DEFINING VELOCITY FIELDS
C          NY1        INT        MASTER ROW INDEX DEFINING THERMODYNAMIC
C                                FIELDS
C          NY2        INT        MASTER ROW INDEX DEFINING WIND FIELDS
C          OUT        REAL       LAND-SEA MASK DEFINING THE OUTFLOW GRID CELLS
C          OUTINIT    REAL       LAND-SEA MASK FROM NORWAY-SPITZBERGEN OUTFLOW
C          OUTIN      REAL       LAND-SEA MASK FROM NORTHERN INFLOW/OUTFLOW 
C                                POINTS
C          PLTSTP     INT*       THE INTERVAL, IN TIME STEPS AT WHICH WE
C                                WRITE TO UNIT 20
C          PRESS      REAL       ICE STRENGTH
C          PRTSTP     INT*       THE INTERVAL IN TIMESTEPS AT WHICH TO PRINT
C                                RESULTS
C          RELAXS     REAL       TIME SPENT INSUBROUTINE RELAX
C          RUICE      REAL       U COMPONENT OF THE ICE DRIFT VELOCITY ZEROED
C                                OUT AT POINTS WHERE THICKNESS IS ZERO.
C          RVICE      REAL       V COMPONENT OF THE ICE DRIFT VELOCITY ZEROED
C                                OUT AT POINTS WHERE THICKNESS IS ZERO.
C          SNRT       REAL       SNOW FALL RATE
C          TGRDI      REAL       X LOCATION FOR THE THERMODYNAMIC VARIABLES
C          TGRDJ      REAL       Y LOCATION FOR THE THERMODYNAMIC VARIABLES
C          THEFF      REAL       TOTAL BASIN ICE THICKNESS
C          THEFF1     REAL       TOTAL BASIN ICE THICKNESS AFTER OUTFLOW
C                                ADJUSTMENT
C          THEFF2     REAL       SUM OVER ALL GRID SQUARES OF THICKNESS
C          TOUT1      REAL       OUTFLOW FOR THE TIME STEP
C          THETA      REAL       USED IN SUBROUTINE RELAX TO INDICATE A
C                                BACKWARDS TIME STEP
C          TICE       REAL       MIXED LAYER TEMPERATURE IN THE CASE OF OPEN
C                                WATER OR ICE TEMPERATURE IN THE CASE OF AN
C                                ICE COVER
C          UICE       REAL       THE X COMPONENT OF THE ICE DRIFT DEFINED
C                                AT THREE TIME LEVELS
C          UICEC      REAL       THE INTERMEDIATE X COMPONENT OF THE ICE
C                                DRIFT FOR USE IN THE SEMI-IMPLICIT TIME
C                                STEPPING
C
C          UVM        REAL       LAND-SEA MASK FOR THE VELOCITY VARIABLES
C          VICE       REAL       THE Y COMPONENT OF THE ICE DRIFT DEFINED
C                                AT THREE TIME LEVELS
C          VICEC      REAL       INTERMEDIATE Y COMPONENT OF THE ICE DRIFT
C                                FOR USE IN SEMI-IMPLICIT TIME STEPPING
C          YNEG       REAL       NEGATIVE ICE TO BE MELTED
C
C          ZETA       REAL       NON LINEAR BULK VISCOSITY
C
C  METHOD: 
C
C	 
C     MOMENTUM BALANCE, THE ICE RHEOLOGY, THE ICE THICKNESS 
C     DISTRIBUTION AND THE THERMODYNAMIC HEAT BUDGET.  FOR A MORE 
C     DETAILED DESCRIPTION OF THE MODEL SEE HIBLER (JOURNAL OF 
C     PHYSICAL OCEANOGRAPHY, 1979, MONTHLY WEATHER REVIEW, 1980) AND  
C     PRELLER, NORDA REPORT 108, 1985). 
C          RUNS WITH THIS MODEL FALL INTO TWO CATEGORIES: 
C     INITIALIZATION RUNS AND RESTART RUNS.  IN A RESTART RUN THE 
C     INPUT FILE ON UNIT 3 CONTAINS THE ICE DRIFT, THICKNESS, 
C     COMPACTNESS, LATERAL HEAT AND ICE TEMPERATURE FIELDS FROM 
C     THE MOST RECENT MODEL FORECAST.  FILES WERE WRITTEN TO UNIT 3 
C     BY CYBER 205 PROGRAM "UPDATBF". 
C          IN AN INITIALIZATION RUN, THE FILES READ FROM UNIT 3 ARE 
C     MONTHLY MEAN MODEL CLIMATOLOGICAL VALUES OF ICE DRIFT, THICKNESS, 
C     COMPACTNESS, LATERAL HEAT AND ICE TEMPERATURE.  IF NO RESTART 
C     IS AVAILABLE, THESE CLIMATOLOGY FIELDS ARE OBTAINED FROM CRANDIO 
C     FILE CLIMAT AND ARE WRITTEN TO UNIT 3 BY "UPDATIF". 
C          THREE ADDITIONAL INPUT FILES ARE REQUIRED BY THE MODEL. 
C     UNIT 9 CONTAINS THE GEOSTROPHIC OCEAN CURRENTS, OBTAINED FROM 
C     THE MODEL SOLUTIONS OF THE HIBLER-BRYAN ICE-OCEAN MODEL 
C     (SCIENCE, 1984).  UNIT 4 CONTAINS THE MONTHLY MEAN OCEANIC HEAT 
C     FLUXES ALSO OBTAINED FROM THE HIBLER-BRYAN MODEL.  RECREATION 
C     OF BOTH OF THESE FILES IS PRESENTLY DONE ONLY AT NRL. 
C     UNIT 7 CONTAINS THE LAND SEA TABLES DEFINING THE MODEL. 
C     THESE MAY BE RECREATED BY RUNNING PROGRAM GRNMASK ON "OPSPL1". 
C     THIS GREENLAND SEA MODEL NEEDS ONE ADDITIONAL INPUT FIELD, AN
C     INTERPOLATED ICE THICKNESS FIELD FROM THE PIPS MODEL FOR USE 
C     AS A BOUNDARY CONDITION.  THE GREENLAND SEA MODEL IS RUN 
C     OPERATIONALLY AFTER PIPS.  THE INTERPOLATED PIPS THICKNESS IS    
C     PASSED TO THE GREENLAND SEA VIA UPDATGF AS TAPE 14 = ZMBOUN.
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C          THERE ARE THREE OUTPUT FILES.  ICEFTU (UNIT10) ARE XFRCIO  
C     CARDS FOR RESTART RECORDS, AND FCSTXFR (UNIT11) XFRCIO CARDS FOR 
C     PRODUCT FORECAST RECORDS.  TAPE20 IS A FORECAST FILE FORMATTED FOR 
C     USE BY PROGRAM ICEPLOT (NCAR PLOT ROUTINES) TO MAKE CY205 PLOTS.
C     These files will all be changed for the Cray. 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C          THERE ARE THREE OUTPUT FILES.  1) RESTART RECORDS, 2) FORECAST
C     RECORDS AND 3) FORECAST FORMATTED FOR USE BY GRAPHIC PROGRAMS.
C          THE MODEL ALSO REQUIRES THE XFRCIO TRANSFER OF ZRANDIO FIELDS 
C     A01, A07, A11, A15, A16 AND A18 FROM NOGAPS AND THE MARINE WIND 
C     FIELDS A29 AND A30. 
C          ONE DATA CARD IS NEEDED BY THE PROGRAM.  THIS CARD MUST INPUT 
C     ITSTEP,PLTSTP,PRTSTP AND IRSTRT. (SEE DEFINITIONS BELOW) 
C          IN ITS PRESENT CONFIGURATION, THE MODEL IS DEFINED AS A 
C     SUBREGION OF THE FNOC 63 X 63 POLAR STEREOGRAPHIC GRID.  THIS 
C     SUBREGION IS FURTHER SUBDIVIDED SUCH THAT THE MODEL IS DEFINED BY 
C     A 80 X 142 ARRAY WITH 20 KM GRID SPACING.  THE GRID IS DEFINED IN 
C     DETAIL BY SUBROUTINE MESH.  ALL CALCULATIONS IN THIS MODEL ARE  
C     PERFORMED IN MKS UNITS. 
C*********************************************************************** 
C
C  INCLUDE FILES:  N/A
C
C  COMPILER DEPENDENCIES: FORTRAN 77 
C
C  COMPILER OPTIONS: -a stack  memory allocation
C                    -o zeroinc  optimization options
C                    -d p      disables double precision
C                    -e z      enables debugger
C                    -m 4      message category - error message 
C
C  MAKEFILE:         location? 
C
C  RECORD OF CHANGES: 
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C <<CHANGE NOTICE>>GRNPIPS01 (01 MAR 1991) -- PHAM, T.
C                  TO ADD UNIQSUB TO LOAD CARD.
C
C <<CHANGE NOTICE>>GRNPIPS02 (01 OCT 1991) -- PHAM, T.
C                  TO SET VALUES OF THE TWO LEFT COLUMNS TO VALUES OF    
C                  THE THIRD IN SUBROUTINE ICEOUT.
C
C <<CHANGE NOTICE>>GRNPIPS03 (15 APR 1992) -- PHAM, T.
C                  TO IMPLEMENT THE HIGH RESOLUTION SURFACE STRESS
C                  AS DRIVING FORCE.
C
C**************************END PROLOGUE********************************
C
C 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - WE DEFINED ALL VARIABLES TO DOUBLE PRECISION TO MATCH THE 
C     OLD 205 CODE, BUT WE TURN OFF DOUBLE PRECISION IN THE COMPILE
C     OPTIONS.  DOUBLE PRECISION ON THE 205 IS EQUIVALENT TO SINGLE 
C     PRECISION ON THE CRAY. 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - The pp_ values are necessary only for our test case.
C            They are the model results that we write our and look
C            at.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142,NX2=81,NY2=143) 
      real*4 pp_gairx,pp_gairy,pp_hdiff1,
     *       pp_ruice,pp_rvice,pp_heff,
     *       pp_area,pp_fheff,pp_gdiff,
     *       pp_hcorr,pp_press,pp_div,bh
      DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),ETA(NX1,NY1), 
     *          ZETA(NX1,NY1),
C               GAIRX(NX2,NY2),GAIRY(NX2,NY2), 
     *          AMASS(NX,NY),HEFF(NX1,NY1,3), 
     *          AREA(NX1,NY1,3),UICEC(NX,NY),VICEC(NX,NY),  
     *          GWATX(NX,NY),GWATY(NX,NY),GDIFF(NX1,NY1), 
     *          HDIFF1(NX1,NY1),FO(NX1,NY1),HEFFM(NX1,NY1), 
     *          UVM(NX,NY),OUT(NX1,NY1),DRAGA(NX1,NY1), 
     *          DRAGS(NX1,NY1),GRDI(NX2,NY2),GRDJ(NX2,NY2), 
c     *          IDTG(3),DIV(NX1,NY1),RUICE(NX,NY),RVICE(NX,NY), 
     *          DIV(NX1,NY1),RUICE(NX,NY),RVICE(NX,NY), 
     *          HCORR(NX1,NY1),GAREA(NX1,NY1),
     *          OUTINIT(NX1,NY1),OUTIN(NX1,NY1),BHEFF(NX1,NY1),
     *          bh(nx1,ny1) 
C
      dimension pp_gairx(nx2,ny2),pp_gairy(nx2,ny2),pp_hdiff1(nx1,ny1),
     *          pp_ruice(nx,ny),pp_rvice(nx,ny),pp_heff(nx1,ny1),
     *          pp_area(nx1,ny1),pp_fheff(nx1,ny1),pp_gdiff(nx1,ny1),
     *          pp_hcorr(nx1,ny1),pp_press(nx1,ny1),pp_div(nx1,ny1)
C 
C 
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      COMMON / FORCE /  FORCEX(NX,NY), FORCEY(NX,NY) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA 
      COMMON / PRESS1 / PRESS(NX1,NY1) 
      COMMON / GROW / YNEG(NX1,NY1),FHEFF(NX1,NY1) 
      COMMON / TGRIDS / TGRDI(NX2,NY2),TGRDJ(NX2,NY2) 
      COMMON / TEMP / TICE(NX1,NY1) 
      COMMON / OCEANS / FW(NX1,NY1) 
      COMMON / SNOW / SNRT 
c      COMMON / PRESSUR / PS(NX2,NY2) 
      COMMON / WSTRES / WSU(NX2,NY2),WSV(NX2,NY2)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     FNOC - rfor is a special common block set up for this test.  It
C            is the way we pass our test data set of atmospheric forcing
C            from NOGAPS through the model 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      common / rfor / gairx(nx2,ny2),gairy(nx2,ny2),tair(nx2,ny2),
     *              ps(nx2,ny2),es(nx2,ny2),fsh(nx2,ny2),
     *              ps1(nx2,ny2),es1(nx2,ny2),evap(nx2,ny2)
C 
      INTEGER PLTSTP,PRTSTP 
C 
C 
C     INITIALIZE CONSTANTS AND READ INPUT CARD 
C 
C 
C 
      DATA LAD /2/, ICOUNT /0/ 
      DATA DELTAT /21600./, DELTT /21600./, ERROR /0.000001/,
     *     A22 /0.15/, HO /0.5/ 
      PRINT 1000 
 1000 FORMAT(' >>> FORTRAN 200 VERSION OF GRNLAND <<< ') 
C 
      IDELTM=DELTAT/3600.  + .01 
      TBEGIN = SECOND() 
      READ(5,1113)ITSTEP,PLTSTP,PRTSTP,IRSTRT,IDTG 
 1113 FORMAT(4I5,I8) 
      call daynum(idtg,myr,mm,md,mhr)
C 
C NOW DECIDE ON BASIC PARAMETERS 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC--We replaced the 205 standard subroutine DDTG with our own
C           subroutine called Daynum
c     CALL DDTG(IDTG) 
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF (MM.LE.4) SNRT =  3.215E-9 
      IF (MM.EQ.5) SNRT = 19.29E-9 
      IF (MM.GE.6 .AND. MM.LE.8)   SNRT =  0. 
      IF (MM.EQ.8 .AND. MD.GE.20)  SNRT = 49.603E-9 
      IF (MM.GE.9 .AND. MM.LE.10)  SNRT = 49.603E-9 
      IF (MM.GE.11 .AND. MM.LE.12) SNRT =  3.215E-9 
      CALL MESH(GRDI,GRDJ,N1,N2) 
      ERROR=5.0*ERROR 
      DIFF1=.004*DELTAX 
C 
C DOUBLE HO BECAUSE OF MOD IN GROWTH 
C 
      HO=2.0*HO 
C 
C NOW DEFINE BOUNDARIES 
C 
      CALL BNDRY(HEFFM,UVM,OUTINIT,OUTIN) 
C
C     CREATE OUT SO THAT IT REFLECTS ALL BOUNDARYS
C
      DO 123 J=1,NY1
      DO 123 I=1,NX1
      OUT(I,J)=OUTIN(I,J)
123   CONTINUE
      DO 124 J=2,4
      DO 124 I=1,NX1
      OUT(I,J)=OUTINIT(I,J)
124   CONTINUE 
C 
C     READ AND FORM MONTHLY MEAN OCEAN CURRENTS 
C 
      CALL OCEAN(GWATX,GWATY,UVM) 
C 
C     READ MONTHLY MEAN HEAT FLUXES 
      READ(4,1127)FW 
 1127 FORMAT(10F8.3) 
C
C     READ IN THE BOUNDARY CONDITIONS INTERPOLATED FROM PIPS
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     changed this so it reads a formatted file
c      READ(14)BHEFF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      read(14,1033)bh
1033  format(1x,6e13.6)
      do 555 j=1,ny1
      do 555 i=1,nx1
      bheff(i,j)=dble(bh(i,j))
555   continue
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     FNOC - you will need to make a change here to accomodate
C            the new data base system 
c
C 
C     OPEN THE CRANDIO OUTPUT FILE 
C 
c      IFILE = 7HMASFNOC 
c      CALL COPEN(IFILE,ISTAT) 
c      IF(ISTAT .NE. 0) STOP 'COPEN ERROR' 
c      JFILE = 7HICEFILE 
c      CALL COPEN(JFILE,ISTAT) 
c      IF(ISTAT .NE. 0) STOP 'COPEN2 ERROR'
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      ITAU = 0
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C
C     FNOC--Subroutine Iceintl will have to be replaced with
C           code compatible to the new data base
C 
C     OBTAIN INITIAL SURFACE PRESSURE VALUES 
C 
c      CALL ICEINTL(1,PS,TGRDI,TGRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(7,WSU,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(8,WSV,GRDI,GRDJ,IDTG,NX2,NY2,ITAU)
C
C      FNOC--this is our subroutine call to read in the test data
C            set of atmospheric forcing for this particular test
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      call rforce
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     We called geowind in rforce
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     CREATE GEOSTROPHIC WINDS FROM SURFACE PRESSURES 
c      CALL GEOWIND 
C 
C NOW INITIALIZE SYSTEM 
C FIRST GUESS AT INITIAL HEFF AND AREA  
C 
C     OR READ SAVED DATA FOR RESTART 
C 
      IF(IRSTRT .EQ. 0) THEN  
C 
         CALL RESTRT(UICE,VICE,UICEC,VICEC 
     +        ,HEFF,AREA,YNEG,TICE,NPOCDTG) 
C 
C   OUTPUT TAU=0 FROM UPDATED RESTART RECORDS 
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC-- You will need to fix Iceout to be compatible with your
C            new data base system
C 
c      CALL ICEOUT(UICE,VICE,HEFF,AREA,PRESS,FHEFF,DIV,-IDELTM, 
c     &IDTG(1),JFILE,HEFFM,IDELTM,0) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ELSE 
C 
C 
C 
      PRINT 1114 
 1114 FORMAT(' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ', 
     &/, ' BEGIN SPIN UP TO STEADY STATE, INITIALIZE ARTIC ', 
     & /,' WITH ABOUT 3.3 METERS ICE THICKNESS, TICE AT 273 C ', 
     & /,   '  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ') 
         DO 12 J = 1,NY 
            DO 11 I = 1,NX 
            UICEC(I,J) = 0.0 
            VICEC(I,J) = 0.0
 11         CONTINUE 
 12      CONTINUE 
         DO 99 K = 1,3 
            DO 98 J = 1,NY 
               DO 97 I = 1,NX 
               UICE(I,J,K) = 0.0 
               VICE(I,J,K) = 0.0
 97            CONTINUE
 98         CONTINUE 
 99      CONTINUE 
         DO 101 J = 1,NY1 
            DO 100 I = 1,NX1 
            HEFF(I,J,3)=0.0 
            HEFF(I,J,2)=0.0 
            AREA(I,J,2)=1.0 
            AREA(I,J,3)=1.0 
            HEFF(I,J,1)=(3.0)/0.91  
            HEFF(I,J,1)=HEFF(I,J,1)*OUT(I,J)  
            AREA(I,J,1)=1.0 
            YNEG(I,J)=0.0 
            TICE(I,J) = 273.0
 100        CONTINUE 
 101     CONTINUE 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC  This was a 205 subroutine - now taken out
C     
c      CALL Q5SNDMDF('MSG=', 
c     +'*RESTARTING FROM BASIC INITIAL CONDITIONS*') 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       ENDIF 
C 
C 
C     CALCULATE TOTAL BASIN ICE THICKNESS 
C        EXCEPT AT OUTFLOW CELLS 
C 
      CALL XSUM(HEFF,THEFF) 
      THETA=1.0 
C 
C     START WITH AN  INITIAL VELOCITY FIELD OF ZERO 
C 
      CALL FORM(UICE,VICE,ETA,ZETA,AMASS,GAIRX,GAIRY,GWATX,GWATY, 
     *    DRAGS,DRAGA,OUT,HEFFM,DIV,HEFF,AREA) 
C 
C     SET MASS TO 0 AND DEFINE THE VISCOSITIES 
C 
      DO 103 J = 1,NY 
         DO 102 I = 1,NX 
         AMASS(I,J)=0.0 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C    
C     FNOC - change to double precision value
C
c         ZETA(I,J) = HEFF(I,J,1) * (1.0E+11) 
         zeta(i,j) = heff(i,j,1) * (1.0D+11)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         ETA(I,J)=ZETA(I,J)/4.0 
 102     CONTINUE 
 103  CONTINUE 
C 
C     NOW PERFORM RELAXATION  
C 
      CALL RELAX(UICE,VICE,ETA,ZETA,DRAGS,DRAGA,AMASS,UVM 
     *   ,ERROR,THETA,UICEC,VICEC,HEFFM) 
C 
C     NOW START THE STANDARD PREDICTOR CORRECTOR ITERATION SCHEME 
C 
C     PRINT THE AMOUNT OF TIME SPENT IN INITIALIZATION 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      TNEW = SECOND() 
      TT = TNEW - TBEGIN 
      PRINT 9122,TT 
 9122 FORMAT(1H0,'******   INITIALIZATION TIME - ',E10.4,'  ******',  
     +      /1H1) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 200 KSTEP=1,ITSTEP 
C 
      call daynum(idtg,myr,mm,md,mhr)
      ICOUNT = ICOUNT+1 
      TNEW = SECOND() 
C 
C.....CALL ADJUST TO ESTIMATE THE ICE IN THE OUTFLOW CELLS  
C 
      CALL ADJUST(HEFF,AREA,OUTINIT,HEFFM)  
C   
C     CALL INFLOW/OUTFLOW SUBROUTINE
C
      CALL INOUT(HEFF,AREA,OUTIN,HEFFM,UICE,VICE,MM,BHEFF)
C 
C     SUM THE ADJUSTED ICE THICKNESS FROM ALL GRIDS CELLS AND FIND 
C     THE DIFFERENCE BETWEEN THIS SUM AND THE PREVIOUS THICKNESS 
C     SUM 
C 
      CALL XSUM(HEFF,THEFF1)  
      THEFF1 = THEFF1 - THEFF 
C 
C 
C     FIRST DO THE PREDICTOR  
C 
      DO 121 J=1,NY 
         DO 120 I=1,NX 
         UICE(I,J,3)=UICE(I,J,1) 
         VICE(I,J,3)=VICE(I,J,1) 
         UICEC(I,J)=UICE(I,J,1)  
         VICEC(I,J)=VICE(I,J,1) 
 120     CONTINUE 
 121  CONTINUE 
      THETA=1.0 
      DELTAT=DELTT/1.0 
      CALL FORM(UICE,VICE,ETA,ZETA,AMASS,GAIRX,GAIRY,GWATX,GWATY, 
     *    DRAGS,DRAGA,OUT,HEFFM,DIV,HEFF,AREA) 
      CALL RELAX(UICE,VICE,ETA,ZETA,DRAGS,DRAGA,AMASS,UVM 
     *    ,ERROR,THETA,UICEC,VICEC,HEFFM) 
C 
C NOW DO REGULAR TIME STEP 
C 
      DO 155 J=1,NY 
         DO 154 I=1,NX 
         UICE(I,J,1)=0.5*(UICE(I,J,1)+UICE(I,J,2)) 
         VICE(I,J,1)=0.5*(VICE(I,J,1)+VICE(I,J,2))
 
 154     CONTINUE 
 155  CONTINUE 
C     NOW GO BACKWARDS IN TIME 
      THETA=1.0 
      DELTAT=DELTT  
      CALL FORM(UICE,VICE,ETA,ZETA,AMASS,GAIRX,GAIRY,GWATX,GWATY, 
     *    DRAGS,DRAGA,OUT,HEFFM,DIV,HEFF,AREA) 
C 
C NOW SET U(1)=U(2)=AND SAME FOR V 
C 
      DO 111 J = 1,NY 
         DO 110 I = 1,NX 
         UICE(I,J,3)=UICE(I,J,1) 
         VICE(I,J,3)=VICE(I,J,1) 
         UICEC(I,J)=UICE(I,J,1)  
         VICEC(I,J)=VICE(I,J,1)  
         UICE(I,J,1)=UICE(I,J,2) 
         VICE(I,J,1)=VICE(I,J,2)
 110     CONTINUE 
 111  CONTINUE 
C 
C     NOW PERFORM RELAXATION  
C 
      CALL RELAX(UICE,VICE,ETA,ZETA,DRAGS,DRAGA,AMASS,UVM 
     *     ,ERROR,THETA,UICEC,VICEC,HEFFM) 
C 
      DO 130 J = 1,NY 
         DO 129 I = 1,NX 
C 
C     SAVE THE T+1 TIME VALUES OF U AND V FOR USE IN ADVECTION OF 
C     THE THICKNESS AND COMPACTNESS. 
C 
         UICEC(I,J) = UICE(I,J,1) 
         VICEC(I,J) = VICE(I,J,1) 
C
 129     CONTINUE 
 130  CONTINUE 
C 
C     WRITE THE TIME STEP, THE INITIAL DATE TIME GROUP AND THE NUMBER 
C     OF HOURS PROGRESSED FROM THE INITIALIZATION OF THE FORECAST 
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c      PRINT 1111,ICOUNT,IDTG(1),ITAU 
c 1111 FORMAT(1H0,'*****************************************************' 
c     *,//,'  TIME STEP -',I5,5X,'DTG  - ',A8, 
c     *5X,'ITAU - ',I3) 
C 
      print 1111,icount,myr,mm,md,mhr
 1111 format(1h0,'***********************'
     *,//,' TIME STEP - ',I5,5x,'DTG - ',3i2,
     *5x,'ITAU - ',I3)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C 
C NOW DO ADVECTION  
C 
C     CALCULATE THE ADVECTION OF ICE THICKNESS AND COMPACTNESS 
C 
      CALL ADVTICE(UICEC,VICEC,HEFF,DIFF1,LAD,HEFFM) 
      CALL ADVTICE(UICEC,VICEC,AREA,DIFF1,LAD,HEFFM) 
C 
C     NOW DO THE GROWTH COMPONENT 
C 
C 
C     SUBROUTINE HEAT CALLS ICEINTL, WHICH READS THE THERMODYNAMIC 
C     FORCING FIELDS (AS WELL AS THE WINDS) IN CRANDIO FORMAT.  HEAT  
C     THEN FORMS THE VARIABLES NEEDED FOR THE HEAT BUDGET.  SUBROUTINE 
C     HEAT ALSO CALLS THE SUBROUTINE BUDGET FOR THE HEAT BUDGET 
C     CALCULATION.  SUBROUTINE GROWTH CALCULATES THE CHANGE OF THICKNESS 
C     AND COMPACTNESS DUE TO GROWTH. 
C 
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC--The call to subroutine heat was changed since Gairx and
C           Gairy are passed via common for this test case.
C
C 
c      CALL HEAT(TGRDI,TGRDJ,HEFF,AREA,FO,HDIFF1,GAIRX,GAIRY,ITAU, 
c     *          IDTG)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
      CALL HEAT(TGRDI,TGRDJ,HEFF,AREA,FO,HDIFF1,ITAU, 
     *          IDTG) 
      CALL GROWTH(HEFF,AREA,HO,A22,HDIFF1,FO,HCORR,HEFFM,OUT, 
     1            GAREA) 
C     MUST CALL GROWTH ONLY AFTER CALLING ADVECTION 
C 
C     SUM OF TOTAL ICE IN THE BASIN... EXCLUDING OUTFLOW CELLS 
C 
      CALL XSUM(HEFF,THEFF) 
C 
C     THIS SECTION COMPUTES VARIOUS SUMS NECESSARY FOR INSURING 
C     CONSERVATION PLUS MONITORING VARIOUS CONTRIBUTIONS TO ICE 
C     CHANGES. 
C 
      THEFF2 = 0.0  
      FHSUM = 0.0 
      GRSUM = 0.0 
      ARSUM = 0.0 
      DO 105 J = 1,NY1 
         DO 104 I = 1,NX1 
         HEFF(I,J,1)=HEFF(I,J,1)*OUT(I,J)  
         AREA(I,J,1)=AREA(I,J,1)*OUT(I,J)  
         FHEFF(I,J)=FHEFF(I,J)*OUT(I,J) 
         GAREA(I,J)=GAREA(I,J)*OUT(I,J) 
         HCORR(I,J)=HCORR(I,J)*OUT(I,J) 
         HDIFF1(I,J) = HDIFF1(I,J)*OUT(I,J) 
C 
C     HDIFF1 CONTAINS THE TOTAL OPEN WATER GROWTH FOR THE BASIN 
C 
         GDIFF(I,J)=FHEFF(I,J)-HDIFF1(I,J) 
         GRSUM = GRSUM + HDIFF1(I,J) 
         THEFF2 = THEFF2 + HEFF(I,J,1) 
         ARSUM = ARSUM + AREA(I,J,1) 
         FHSUM = FHSUM + FHEFF(I,J)
 104     CONTINUE 
 105  CONTINUE 
      TOUT1=THEFF-THEFF2-THEFF1 
      THEFF=THEFF2  
C 
C     ZERO OUT ICE DRIFT VELOCITIES AT LOCATIONS WHERE ICE THICKNESS  
C     IS ZERO 
      DO 116 J=1,NY 
         DO 115 I=1,NX 
         RUICE(I,J)=UICE(I,J,1)  
         RVICE(I,J)=VICE(I,J,1)  
         HEFFU=(HEFF(I,J,1)+HEFF(I+1,J,1)+HEFF(I,J+1,1) 
     +          +HEFF(I+1,J+1,1))/4. 
         IF(HEFFU.EQ.0.)RUICE(I,J)=0. 
         IF(HEFFU.EQ.0.)RVICE(I,J)=0.
 115     CONTINUE 
 116  CONTINUE 
C 
C     ZERO OUT THE DIVERGENCE FIELD WHERE THE ICE THICKNESS IS ZERO.  
C 
      DO 117 J=1,NY1 
         DO 118 I=1,NX1 
         IF(HEFF(I,J,1).EQ.0.)DIV(I,J)=0. 
 118     CONTINUE 
 117  CONTINUE 
C 
C 
C-----------  OUTPUT SECTION -------------------- 
C 
C  DO NOT WRITE TO TAPE20 (NRL DEVELOPMENT PLOT FILE) IF PLTSTP=0.  
      IF(PLTSTP .NE. 0)  THEN 
         IF(MOD(ICOUNT,PLTSTP) .EQ. 0  .OR.  ICOUNT .EQ. ITSTEP) THEN 
C 
         KFCST=ITAU+IDELTM 
         PRINT 1116,KFCST 
 1116    FORMAT('   WRITE ',I3,' HOUR FCST TO TAPE20 ') 
C 
         do 2020 i=1,nx2
            do 2010 j=1,ny2
            pp_gairx(i,j)=gairx(i,j)
            pp_gairy(i,j)=gairy(i,j)
 2010       CONTINUE 
 2020    continue
         do 2021 i=1,nx1
            do 2011 j=1,ny1
            pp_hdiff1(i,j)=hdiff1(i,j) 
            pp_heff(i,j)=heff(i,j,1)
            pp_area(i,j)=area(i,j,1)
            pp_fheff(i,j)=fheff(i,j)
            pp_gdiff(i,j)=gdiff(i,j)
            pp_hcorr(i,j)=hcorr(i,J)
            pp_press(i,j)=press(i,j)
            pp_div(i,j)=div(i,j)
 2011       CONTINUE 
 2021    continue
         do 2022 i=1,nx
            do 2012 j=1,ny
            pp_ruice(i,j)=ruice(i,j)
            pp_rvice(i,J)=rvice(i,j)
 2012       CONTINUE 
 2022    continue
c
         WRITE(20,2556)pp_gairx 
         WRITE(20,2556)pp_GAIRY 
         WRITE(20,2550)pp_HDIFF1 
         WRITE(20,2555) ((pp_RUICE(I,J),I=1,NX),J=1,NY)  
         WRITE(20,2555) ((pp_RVICE(I,J),I=1,NX),J=1,NY)  
         WRITE(20,2550)((HEFF(I,J),I=1,NX1),J=1,NY1) 
         WRITE(20,2550)((pp_AREA(I,J),I=1,NX1),J=1,NY1) 
         WRITE(20,2550)pp_FHEFF 
         WRITE(20,2550)pp_GDIFF 
         WRITE(20,2550)pp_HCORR 
         WRITE(20,2550)pp_PRESS 
         WRITE(20,2550)pp_DIV 
 2550    FORMAT(98(1X,6E13.6/),98(1X,6E13.6/)) 
 2555    FORMAT(92(1X,6E13.6/),92(1X,6E13.6/)) 
 2556    FORMAT(104(1X,6E13.6/),104(1X,6E13.6/)) 
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C      FNOC - this is a 205 subroutine
C 
c      CALL Q5SNDMDF('MSG=','*WROTE TO TAPE20*') 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      ENDIF 
      ENDIF 
      T2 = SECOND() 
      TST = T2-TNEW 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C 
C  ---------- DO CRANDIO OUTPUT  (DONE EACH TIME STEP) ------------ 
C 
C 
C  DETERMINE IPRNT: PRINT RESULTS IF  ZERO. 
      IPRNT=MOD(ICOUNT,PRTSTP) 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C      FNOC - Don't forget that this subroutine must be changed
C 
c      CALL ICEOUT(RUICE,RVICE,HEFF,AREA,PRESS,FHEFF,DIV,ITAU, 
c     &IDTG(1),JFILE,HEFFM,IDELTM,IPRNT) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C----PRINT RESULTS ON LAST STEP AND ON SPECIFIED INTERMEDIATE STEPS----- 
C 
      IF(IPRNT .EQ. 0  .OR. ICOUNT .EQ. ITSTEP) THEN 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      PRINT 91,IDTG(1),ICOUNT 
c 91   FORMAT(1H1,'OUTPUT FOR DTG - ',A8,5X,'STEP - ',I10) 
      print 9991,myr,MM,MD,ICOUnt
 9991 format(1H1,'OUTPUT FOR DTG - ',3i2,5x,'STEP - ',I10)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      PRINT 5111,THEFF,THEFF1 
 5111 FORMAT(1H0,'TOTAL ICE THICKNESS - ',G20.12, 
     +      /1H ,'NET ICE THICKNESS -   ',G20.12) 
      PRINT 5004 
 5004 FORMAT(//) 
      PRINT 5002,TOUT1 
      PRINT 5004 
 5002 FORMAT(1X,'OUTFLOW FOR THIS TIME STEP   ',E10.4/) 
 5009 FORMAT(1X,'OPEN WATER GROWTH FOR THIS TIMESTEP  ',E10.4/) 
 5008 FORMAT(1X,'ICE GROWTH FOR THIS TIME STEP  ',E10.4/) 
 5003 FORMAT(1X,'GROWTH OF AREAL ICE EXTENT FOR THIS TIMESTEP  ', 
     *E10.4/) 
      PRINT 5008, FHSUM 
      PRINT 5009, GRSUM 
      PRINT 5003, ARSUM 
      PRINT 5517 
 5517 FORMAT(1H1,' HDIFF1 - GROWTH OF ICE ON OPEN WATER') 
      CALL PRNT(HDIFF1,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(HDIFF1,NX1,NY1,1,14,25,NX1) 
      PRINT 5519 
 5519 FORMAT(1H1,'UICE - U COMPONENT OF ICE DRIFT') 
      CALL PRNT(UICE,NX,NY,3,1,13,NX) 
      CALL PRNT(UICE,NX,NY,3,14,24,NX)  
      PRINT 5520 
 5520 FORMAT(1H1,'VICE - V COMPONENT OF ICE DRIFT') 
      CALL PRNT(VICE,NX,NY,3,1,13,NX) 
      CALL PRNT(VICE,NX,NY,3,14,24,NX)  
      PRINT 5521 
 5521 FORMAT(1H1,'HEFF - ICE THICKNESS') 
      CALL PRNT(HEFF,NX1,NY1,3,1,13,NX1) 
      CALL PRNT(HEFF,NX1,NY1,3,14,25,NX1) 
      PRINT 5522 
 5522 FORMAT(1H1,'AREA - ICE CONCENTRATION') 
      CALL PRNT(AREA,NX1,NY1,3,1,13,NX1) 
      CALL PRNT(AREA,NX1,NY1,3,14,25,NX1) 
      PRINT 5527 
 5527 FORMAT(1H1,' FHEFF - TOTAL GROWTH OF ICE')  
      CALL PRNT(FHEFF,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(FHEFF,NX1,NY1,1,14,25,NX1) 
      PRINT 5529 
 5529 FORMAT(1H1,' GDIFF - GROWTH OF THICK ICE')  
      CALL PRNT(GDIFF,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(GDIFF,NX1,NY1,1,14,25,NX1) 
      PRINT 5537 
 5537 FORMAT(1H1,' HCORR - ICE TO BE MELTED TO MAINTAIN MASS BALANCE') 
      CALL PRNT(HCORR,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(HCORR,NX1,NY1,1,14,25,NX1) 
      PRINT 5541 
 5541 FORMAT(1H1,'PRESS - ICE STRENGTH') 
      CALL PRNT(PRESS,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(PRESS,NX1,NY1,1,14,25,NX1) 
      PRINT 5842 
 5842 FORMAT(1H1,'DIV - DIVERGENCE FIELD (PER SEC)') 
      CALL PRNT(DIV,NX1,NY1,1,1,13,NX1) 
      CALL PRNT(DIV,NX1,NY1,1,14,25,NX1) 
C 
      ENDIF 
C 
C....... SAVE DATA NECESSARY FOR TOMORROWS RESTART 
C
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     FNOC - mhr is just our equivalent of itau since we don't have
c            a ddtg subroutine - we have a daynum routine
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
      IF(mhr .EQ. 24-IDELTM)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC--this is a special savdata call for this special test
C 
c     &CALL SAVDATA(UICE,VICE,UICEC,VICEC,HEFF,AREA,YNEG,TICE 
c     &,IDTG(1),NPOCDTG) 
     *call savdata2(uice,vice,uicec,vicec,heff,area,yneg,tice)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     PRINT THE AMOUNT OF TIME TAKEN FOR THIS TIMESTEP 
C 
      PRINT 9123,TST 
 9123 FORMAT(1H0,'**********  TIME STEP TIME   ',E10.4) 
C 
C     DETERMINE IF MORE TIME STEPS ARE TO BE DONE 
C 
      IF(ICOUNT .EQ. ITSTEP) GO TO 205  
C 
      idtg=idtg+ideltm
      call daynum(idtg,myr,mm,md,mhr)
      if (mhr.ge.24)idtg=(idtg-24)+100
c
c      ITAU=ITAU+IDELTM
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     
C     FNOC - These calls to Iceintl have been replaced by the call to
C            rforce
C
c      CALL ICEINTL(1,PS,TGRDI,TGRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(7,WSU,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(8,WSV,GRDI,GRDJ,IDTG,NX2,NY2,ITAU)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      call rforce
c      CALL GEOWIND 
C 
 200  CONTINUE 
C 
 205  CONTINUE 
C 
C     PRINT REMAINING TIME STATISTICS FOR THE RUN 
C 
      TSTOP = SECOND() 
      TSTOP = TSTOP - TBEGIN  
      CALL STATPRT(TSTOP) 
      STOP 'END OF ICE MODEL' 
      END 
      SUBROUTINE ADVTICE(UICEC,VICEC,HEFF,DIFF1,LAD,HEFFM) 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142)
C 
C******************************START PROLOGUE************************* 
C
C  SCCS IDENTIFICATION:  N/A
C 
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   ADVTICE
C
C  DESCRIPTION:                   SUBROUTINE ADVTICE DOES THE EXPLICIT
C                                 TIME STEPPING FOR THE ADVECTION OF
C                                 ICE THICKNESS AND COMPACTNESS WITHIN 
C                                 THE MODEL.
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION                 UNCLASSIFICATION
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM      
C               DEPENDENECIES:    UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL ADVECT(UICEC,VICEC,HEFF,DIFF1,LAD
C                                             HEFFM) 
C  PARAMETERS:
C     NAME    TYPE     USAGE             DESCRIPTION
C     ----    ----     -----    --------------------------------------
C   UICEC    STRUCTURE  OUT     
C   VICEC    STRUCTURE  OUT 
C   HEFF     STRUCTURE  IN/OUT 
C   DIFF1    STRUCTURE  IN/OUT 
C   LAD      INTERGER   OUT 
C   HEFFM    STRUCTURE  OUT 
C
C  COMMON BLOCKS:
C
C   BLOCK   NAME   TYPE   USAGE          NOTES
C   -----   ----   ----   -----  --------------------------------------
C   /STEP/  DELTAT REAL   OUT
C   /STEP/  DELTAX REAL   OUT
C   /STEP/  DELTAY REAL   OUT
C   /STEP/  DELTA1 REAL   OUT
C   /STEP/  DELTA  REAL   OUT
C
C   /TIMES/ RELAXS REAL   OUT
C   /TIMES/ FORMS  REAL   OUT
C   /TIMES/ ADVCTS REAL   IN
C   /TIMES/ GRWTHS REAL   OUT
C   /TIMES/ HEATS  REAL   OUT
C   /TIMES/ MESHS  REAL   OUT
C   /TIMES/ INITS  INT    OUT
C
C  FILES:                         NONE 
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C****************************MAINTENANCE SECTION**************************
C
C  MODULES CALLED:
C    NAME           DESCRIPTION
C  ------          --------------------------------
C  DIFFUS          USED TO DETERMINE THE DIFFUSION OF ICE THICKNESS
C                  AND CONCENTRATION
C
C  LOCAL VARIABLES AND   
C           STRUCTURES:           MOST VARIABLES ARE DEFINED IN THE
C                                 MAIN PROGRAM 
C
C  METHOD:      INPUTS TO THIS SUBROUTINE ARE THE ICE DRIFT VELOCITIES
C               (UICEC AND VICEC) AT THE T+DELTAT TIME STEP, ICE THICKNESS
C               (HEFF) OR ICE CONCENTRATION (AREA), THE HARMONIC DIFFUSION
C               CONSTANT PASSES TO SUBROUTINE DIFFUS, LAD A VARIABLE USED    
C               TO DETERMINE THE TYPE OF TIME FINITE DIFFERENCE USED
C               (LEAPFROG OR BACKWARD EULER) AND HEFFM, THE THERMODYNAMIC
C               LAND-SEA MASK.
C
C  INCLUDE FILES:  NONE
C
C  COMPILER DEPENDENCIES:  FORTRAN 77 
C
C  COMPILE OPTIONS:  -a stack   memory allocation
C                    -o zeroinc optimization options
C                    -d p       disables double precision
C                    -e z       enables debugger
C                    -m 4       message category - error message 
C
C  MAKEFILE: LOCATED ?
C
C  RECORD OF CHANGES:  NONE
C
C**************************END PROLOGUE************************************ 
C    
      DIMENSION HEFF(NX1,NY1,3),UICEC(NX,NY),VICEC(NX,NY),  
     *          HEFFM(NX1,NY1) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      T1 = SECOND() 
      NXM1 = NX - 1 
      NYM1 = NY - 1 
      LL = LAD 
C 
C NOW DECIDE IF BACKWARD EULER OR LEAPFROG 
C 
      IF(LL.EQ.1) GO TO 100 
C 
C BACKWARD EULER 
C 
      DELTT=DELTAT  
      K3=2 
      K2=2 
      GO TO 101 
C 
C LEAPFROG 
C 
 100  DELTT=DELTAT*2.0 
      K3=3 
      K2=2 
 101  CONTINUE 
C 
C NOW REARRANGE H<S 
C 
      DO 200 J = 1,NY1 
      DO 199 I = 1,NX1 
         HEFF(I,J,3)=HEFF(I,J,2) 
         HEFF(I,J,2)=HEFF(I,J,1)
 199     CONTINUE 
 200  CONTINUE 
 202  CONTINUE 
C 
C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION  
C 
      DELTX=DELTT/(4.0*DELTAX) 
      DELTY=DELTT/(4.0*DELTAY) 
      DO 211 J = 1,NYM1 
         DO 209 I = 1,NXM1 
         HEFF(I+1,J+1,1)=HEFF(I+1,J+1,K3)-DELTX*((HEFF(I+1,J+1,2)+HEFF 
     *   (I+2,J+1,2)) * (UICEC(I+1,J+1) + UICEC(I+1,J)) - 
     *   (HEFF(I+1,J+1,2) 
     *   + HEFF(I,J+1,2)) * (UICEC(I,J+1) + UICEC(I,J))) - DELTY * 
     *   ((HEFF(I+1,J+1,2) + HEFF(I+1,J+2,2)) * (VICEC(I,J+1) + 
     *   VICEC(I+1,J+1)) - (HEFF(I+1,J+1,2) + HEFF(I+1,J,2)) * 
     *   (VICEC(I,J) + VICEC(I+1,J)))
 209     CONTINUE 
 211  CONTINUE 
c
C NOW DECIDE IF DONE 
C 
      IF(LL.EQ.2) GO TO 99 
      IF (LL.EQ.3) GO TO 89 
      GO TO 102 
 89   CONTINUE 
C  NOW FIX UP H(I,J,2) 
      DO 88 J = 1,NY1 
         DO 87 I = 1,NX1 
         HEFF(I,J,2)=HEFF(I,J,3)
 87      CONTINUE 
 88   CONTINUE 
      GO TO 102 
 99   CONTINUE 
C NOW DO BACKWARD EULER CORRECTION 
      DO 220 J=1,NY1 
         DO 210 I=1,NX1 
         HEFF(I,J,3)=HEFF(I,J,2) 
         HEFF(I,J,2)=0.5*(HEFF(I,J,1)+HEFF(I,J,2))
 210     CONTINUE 
 220  CONTINUE 
       do 444 j=142,1,-1
       write(18,445)(heff(i,j,1),i=30,35)
445    format(6e13.6)
444     continue
      LL=3 
      K3=3 
      GO TO 202 
 102  CONTINUE 
C NOW DO DIFFUSION ON H(I,J,K3) 
      DO 240 KD=1,2 
C     GO TO (241,242),KD 
      IF(KD.EQ.1) GO TO 241 
      IF(KD.EQ.2) GO TO 242 
 241  CONTINUE 
      CALL DIFFUS(HEFF,DIFF1,DELTT,HEFFM) 
       do 446 j=142,1,-1
       write(18,445)(heffm(i,j),i=30,35)
446     continue
      GO TO 243 
 242  CONTINUE 
      DIFF2=-(DELTAX**2)/DELTT 
      CALL DIFFUS(HEFF,DIFF2,DELTT,HEFFM) 
       do 448 j=142,1,-1
       write(18,445)(heff(i,j,1),i=30,35)
448     continue
 243  CONTINUE 
      DO 330 J = 1,NY1 
         DO 320 I = 1,NX1 
         write(18,*)i,j,heff(i,j,1),heff(i,j,3),heffm(i,j)
         HEFF(I,J,1)=(HEFF(I,J,1)+HEFF(I,J,3))*HEFFM(I,J)
 320     CONTINUE 
 330  CONTINUE 
 240  CONTINUE 
       do 447 j=142,1,-1
       write(18,445)(heff(i,j,1),i=30,35)
447     continue
      T2 = SECOND() 
      ADVCTS = ADVCTS + (T2 - T1) 
      RETURN 
      END 
      SUBROUTINE DIFFUS(HEFF,DIFF1,DELTT,HEFFM)
C
C**************************START PROLOGUE******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:  DIFFUS
C
C  COPYRIGHT:  N/A
C
C  CONTRACT NUMBER AND TITLE:  N/A
C
C  REFERENCES:  NONE
C
C  CLASSIFICATION:  UNCLASSIFIED
C
C  RESTRICTIONS:  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:   UNIX
C  
C  LIBRARIES OF RESIDENCE:  N/A
C
C  USAGE                           CALL DIFFUS(HEFF,DIFF1,DELTT,HEFFM)
C
C  PARAMETERS:
C    NAME      TYPE     USAGE      DESCRIPTION
C   -----     -----    ------     -------------
C   HEFF     STRUCTURE  IN/OUT    
C   DIFF1    STRUCTURE  OUT  
C   DELTT    STRUCTURE  OUT
C   HEFFM    STRUCTURE  OUT
C
C  COMMON BLOCKS:
C
C   BLOCK     NAME   TYPE     USAGE      NOTES
C   -----    -----   ----     -----     ------------
C   /PRESS1/ PRESS   REAL     OUT
C
C   /STEP/   DELTAT  REAL     OUT
C   /STEP/   DELTAX  REAL     OUT
C   /STEP/   DELTAY  REAL     OUT
C   /STEP/   DELTA1  REAL     OUT
C   /STEP/   DELTA   REAL     OUT
C
C  FILES:    NONE
C
C  DATA BASES:  NONE
C
C  NON-FILE INPUT/OUTPUT:  NONE
C
C  ERROR CONDITIONS:  NONE
C
C************************MAINTENANCE SECTION******************************
C
C  MODULES CALLED:  NONE
C
C  LOCAL VARIABLES AND
C          STRUCTURES:
C
C  NAME     TYPE      DESCRIPTION
C  ----    -----     --------------
C  HEFF1   REAL      DIFFUSED ICE THICKNESS OR CONCENTRATION
C
C  METHOD:    SUBROUTINE DIFFUS IS USED TO DETERMINE, BY EXPLICIT
C             FORWARD TIME DIFFERENCING THE DIFFUSION OF ICE
C             THICKNESS AND CONCENTRATION.  INPUTS TO THE SUBROUTINE
C             ARE THE ICE THICXKNESS OR CONCENTRATION HEFF, DIFF1,
C             THE HARMONIC DIFFUSION CONSTANT, DELTT THE TIME STEP
C             AND HEFFM THE THERMODYNAMIC LAND-SEA MASK.  THE RESULTANT
C             DIFFUSED VARIABLE HEFF1 IS PLACED BACK INTO HEFF AT THE
C             END OF THE SUBROUTINE.
C
C  INCLUDE FILES:  NONE
C
C  COMPILER DEPENDENCIES:  Fortran 77 
C
C  COMPILER OPTIONS:  -a stack   memory allocation
C                     -o zeroinc optimization options
C                     -d p       disable double precision 
C                     -e z       enables debugger
C                     -m 4       message category - error message 
C
C  MAKEFILE:  LOCATED ?
C
C**********************END PROLOGUE****************************************
C
      IMPLICIT REAL*8 (A-H,O-Z) 
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
      DIMENSION HEFF(NX1,NY1,3),HEFF1(NX1,NY1),HEFFM(NX1,NY1) 
      COMMON / PRESS1 / PRESS(NX1,NY1) 
      COMMON / STEP / DELTAT,DELTAX,DELTAY,DELTA1,DELTA 
C 
C 
      DO 210 J = 1,NY1 
         DO 200 I = 1,NX1 
         HEFF1(I,J)=0.0
 200     CONTINUE 
 210  CONTINUE 
C NOW DO DIFFUSION  
      DELTXX=DELTT*DIFF1/(DELTAX**2) 
      DELTYY=DELTT*DIFF1/(DELTAY**2) 
      DO 220 J = 2,NY 
         DO 215 I = 2,NX 
         HEFF1(I,J)=DELTXX*((HEFF(I+1,J,3)-HEFF(I,J,3))*HEFFM(I+1,J) 
     *      -(HEFF(I,J,3)-HEFF(I-1,J,3))*HEFFM(I-1,J)) 
     *      +DELTYY*((HEFF(I,J+1,3)-HEFF(I,J,3))*HEFFM(I,J+1) 
     *      -(HEFF(I,J,3)-HEFF(I,J-1,3))*HEFFM(I,J-1))
 215     CONTINUE 
 220  CONTINUE 
      DO 260 J = 1,NY1 
         DO 250 I = 1,NX1 
         HEFF(I,J,3)=HEFF1(I,J) 
 250  CONTINUE 
 260  CONTINUE 
      RETURN 
      END 
      SUBROUTINE XSUM(HEFF,S1)
C
C****************************START PROLOGUE*****************************
C
C  SCCS  IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   XSUM
C
C  DESCRIPTION:                   SUBROUTINE XSUM SUMS EVERY VALUE OF
C                                 THE INPUT ARRAY INTO THE SCALAR S1.
C  
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE 
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL XSUM(HEFF,S1)
C
C  PARAMETERS:
C     NAME     TYPE    USAGE    DESCRIPTION
C    -----     ----    -----    ------------
C     HEFF     REAL    OUT      
C     S1       REAL    IN 
C
C  COMMON BLOCKS:                 NONE
C
C  FILES:                         NONE
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C**************************MAINTENANCE SECTION**************************
C
C  MODULES CALLED:  NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:
C
C     NAME    TYPE   USAGE     DESCRIPTION
C    -----    -----  -----     ------------
C      S1     REAL   IN/OUT 
C
C  COMMON BLOCKS:                 NONE
C
C  FILES:                         NONE
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C  METHOD:                        SUBROUTINE XSUM SUMS EVERY VALUE OF
C                                 THE ARRAY INTO THE SCALAR S1
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         Fortran 77 
C
C  COMPILE OPTIONS:               -a stack   memory allocation
C                                 -o zeroinc optimization options
C                                 -d p       disables double precision
C                                 -e z       enables debugger
C                                 -m 4       message category - error 
C                                                               message 
C
C  MAKEFILE:                      LOCATED?
C
C************************END PROLOGUE*********************************** 
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX1=80,NY1=142) 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
C 
      DIMENSION HEFF(NX1,NY1,3) 
      S1=0.0 
      DO 100 J = 1,NY1 
         DO 90 I = 1,NX1 
         S1 = S1 + HEFF(I,J,1)
 90      CONTINUE 
 100  CONTINUE 
      RETURN 
      END 
      SUBROUTINE MEAN(HEFF,HMEAN,OUT) 
C
C**************************START PROLOGUE********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   MEAN
C
C  DESCRIPTION:                   SUBROUTINE MEAN CALCULATES A MEAN VALUE
C                                 BY AVERAGING THE EIGHT SURROUNDING POINTS.
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:  N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED    
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSETM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL MEAN(HEFF,MEAN,OUT)
C
C  PARAMETERS:
C    NAME      TYPE    USAGE      DESCRIPTION
C    ----     -----    -----      -----------------
C    HEFF      REAL    OUT
C    HMEAN     REAL    IN
C    OUT       REAL    OUT
C
C  COMMON BLOCKS:                 NONE 
C
C  FILES:                         NONE
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT          NONE
C
C  ERROR CONDITIONS               NONE
C
C****************************MAINTENANCE SECTION**************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:
C
C   NAME    TYPE     DESCRIPTION
C  -----    ----     --------------
C  HMEAN    REAL     AVERAGED ARRAY
C
C  METHOD:    SUBROUTINE MEAN CALCULATES THE MENA OF A VALUE BY AVERAGING
C             THE SURROUNDING EIGHT POINTS.  INPUT TO THIS SUBROUTINE ARE
C             THE ARRAY TO BE AVERAGED AND THE OUTFLOW LAND SEA MASK.
C             THE AVERAGED ARRAY IS RETURNED AS HMEAN.  THIS SUBROUTINE
C             IS CALLED BY SUBROUTINE ADJUST.
C
C  INCLUDE FILES:    NONE
C
C  COMPILER DEPENDENCIES:  Fortran 77 
C
C  COMPILE OPTIONS:   -a stack  memory allocation
C                     -o zeroinc optimization option
C                     -d p      disables double precision
C                     -e z      enables debugger
C                     -m 4      message category - error message
C 
C
C  MAKEFILE:      LOCATED?
C
C**************************END PROLOGUE**********************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
      DIMENSION HEFF(NX1,NY1,3), HMEAN(NX1,NY1), OUT(NX1,NY1) 
      DO 101 J=2,NY 
         DO 100 I=2,NX 
         HMEAN(I,J)=(HEFF(I+1,J,1)*OUT(I+1,J)+HEFF(I+1,J+1,1)*
     *     OUT(I+1,J+1) 
     *    +HEFF(I+1,J-1,1)*OUT(I+1,J-1)+HEFF(I,J+1,1)*OUT(I,J+1) 
     *    +HEFF(I,J-1,1)*OUT(I,J-1)+HEFF(I-1,J,1)*OUT(I-1,J) 
     *     +HEFF(I-1,J+1,1)*OUT(I-1,J+1)+HEFF(I-1,J-1,1)*OUT(I-1,J-1) 
     *    )/(OUT(I+1,J)+OUT(I+1,J+1)+OUT(I+1,J-1)+OUT(I,J+1)+OUT(I,J-1) 
     *     +OUT(I-1,J)+OUT(I-1,J+1)+OUT(I-1,J-1)+.00001)
 100     CONTINUE 
 101  CONTINUE 
c
      RETURN 
      END 
      SUBROUTINE OCEAN(GWATX,GWATY,UVM)
C
C**************************START PROLOGUE********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   OCEAN
C
C  DESCRIPTION:                   SUBROUTINE OCEAN READS IN THE U AND V
C                                 COMPONENTS OF THE GEOSTROPHIC OCEAN
C                                 CURRENTS.
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
c               DEPENDENCIES:      UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL OCEAN(GWATX,GWATY,UVM)
C
C  PARAMETERS:
C     NAME      TYPE     USAGE       DESCRIPTION
C   ------     ------   -------     ----------------
C   GWATX       REAL     IN
C   GWATY       REAL     IN
C   UVM         REAL     OUT
C
C  COMMON BLOCKS:                 NONE 
C
C  FILES:                         
C   NAME   UNIT   FILE TYPE   ATTRIBUTE   USAGE   DESCRIPTION
C   ----   ----   ---------   ---------   -----   ------------
C           9     PERMANENT   SEQUENTIAL  IN
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE 
C
C  ERROR CONDITIONS:              NONE 
C     
C**************************MAINTENANCE SECTION****************************
C
C  MODULES CALLED                 NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:           NONE 
C                                 
C  METHOD:                        THE U AND V COMPONENTS OF THE GEOSTROPHIC
C                                 OCEAN CURRENTS, GWATX AND GWATY, ARE READ
C                                 IN FROM UNTI 9 AND CONVERTED TO MKS UNITS
C                                 THE VALUES ARE THEN MASKED AT THE 
C                                 BOUNDARIES AND RETURNED TO THE MAIN PROG.
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DENPENDENCIES:        Fortran 77 
C
C  COMPILE OPTIONS:               -a stack  memory allocation
C                                 -o zeroinc optimization options
C                                 -d p      disables double precision
C                                 -e z      enables debugger
C                                 -m 4      message category - error
C                                                              message 
C
C  MAKEFILE:                      LOCATED?
C
C**********************END PROLOGUE**************************************** 
C   
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141) 
C 
C*********************************************************************** 
C 
C      RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
      DIMENSION GWATX(NX,NY),GWATY(NX,NY),UVM(NX,NY) 
      dimension gx(nx,ny),gy(nx,ny)
C 
      READ(9,1000) Gwatx 
      READ(9,1000) Gwaty 
 1000 FORMAT(10F8.3) 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c     added this to the do 10 loop.  
c
C      CONVERT THE COMPONENTS FROM CENTIMETERS TO METERS 
C 
c      GWATX(1,1;NX*NY) = GWATX(1,1;NX*NY)/100. 
c      GWATY(1,1;NX*NY) = GWATY(1,1;NX*NY)/100. 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C 
C      APPLY THE VELOCITY LAND SEA MASK TO THE ARRAY 
C 
      DO 10 J=1,NY  
         DO 9 I=1,NX  
         GWATX(I,J) = GWATX(I,J)/100. * UVM(I,J) 
         GWATY(I,J) = GWATY(I,J)/100. * UVM(I,J)
 9       CONTINUE 
 10   CONTINUE 
C 
      RETURN 
      END 
      SUBROUTINE BNDRY(HEFFM,UVM,OUTINIT,OUTIN)
C
C**********************START PROLOGUE**************************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   BNDRY
C
C  DESCRIPTION:                   SUBROUTINE BNDRY READS IN THE LAND-
C                                 SEA MASKS
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    N/A
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        NONE
C
C  USAGE:                         CALL BNDRY(HEFFM,UVM,OUTINIT,OUTIN)
C
C  PARAMETERS:
C    NAME      TYPE    USAGE     DESCRIPTION
C    ----      ----    -----     ----------------
C    HEFFM     REAL    OUT    
C    UVM       REAL    OUT
C    OUTINIT   REAL    IN/OUT
C    OUTIN     REAL    IN/OUT
C
C  COMMON BLOCKS:                 NONE
C
C  FILES:
C    NAME   UNIT   FILE TYPE   ATTRIBUTE   USAGE   DESCRIPTION
C    ----   ----   ---------   ---------   -----   -----------
C            7     PERMANENT   SEQUENTIAL   IN      
C
C  DATA BASES:                    NONE 
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C****************************MAINTENANCE SECTION************************
C
C
C  MODULES CALLED:                NONE
C 
C  LOCAL VARIABLES AND 
C           STRUCTURES:           NONE
C
C  METHOD:                        SUBROUTINE BNDRY READS IN THE VELOCITY
C                                 LAND-SEA BOUNDARY, UVM, THE THERMODYNAMIC 
C                                 LAND-SEA BOUNDARY, HEFFM, THE OUTFLOW
C                                 LAND-SEA BOUNDARY FOR NORWAY-SPITZBERGGEN ,
C                                 OUTINIT AND OUTIN, THE NORTHERN INFLOW/
C                                 OUTFLOW POINTS FROM UNIT 7 AND
C                                 RETURNS THEM TO THE MAIN PROGRAM.
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         Fortran 77 
C
C  COMPILER OPTIONS:              -a stack  memory
C                                 -o zeroinc optimization options
C                                 -d p      disables double precision
C                                 -e z      enables debugger
C                                 -m 4  message category - error message 
C
C  MAKEFILE:                      LOCATION?
C
C**************************END PROLOGUE********************************* 
C  
      IMPLICIT REAL*8 (A-H,O-Z)
      real*4 u,h,o,o2
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C 
C*********************************************************************** 
C 
C      RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
      DIMENSION HEFFM(NX1,NY1), UVM(NX,NY), OUTINIT(NX1,NY1),
     + OUTIN(NX1,NY1)
      dimension h(nx1,ny1),u(nx,ny),o(nx1,ny1),o2(nx1,ny1)
C 
C READ IN VELOCITY MASK 
C
      READ (7,1050)((U(i,j),i=1,79),j=1,141)
      READ (7,1050)((H(i,j),i=1,80),j=1,142)
      READ (7,1050)((O(i,j),i=1,80),j=1,142) 
      READ (7,1050)((O2(i,j),i=1,80),j=1,142)
 1050 FORMAT(1x,6e13.6)
c
      do 10 i=1,nx
      do 10 j=1,ny
      uvm(i,j)=dble(u(i,j))
10    continue
      do 11 i=1,nx1
      do 11 j=1,ny1
      heffm(i,j)=dble(h(i,j))
      outinit(i,j)=dble(o(i,j))
      outin(i,j)=dble(o2(i,j))
11    continue
        do 13 j=142,1,-1
        write(18,12)(heffm(i,j),i=30,35)
12      format(1x,6e13.6)
13      continue
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - this is 205 specific code
C 
C      PRINT THE THREE LAND SEA MASKS.  
C 
c      CALL QPRNTG(  UVM,6H   UVM,1H ,1,1,NX,NY) 
c      CALL QPRNTG(HEFFM,6H HEFFM,1H ,1,1,NX1,NY1) 
c      CALL QPRNTG(  OUT,6H   OUT,1H ,1,1,NX1,NY1) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      RETURN 
      END 
c
      SUBROUTINE RELAX(UICE,VICE,ETA,ZETA,DRAGS,DRAGA,AMASS,UVM, 
     *ERROR,THETA,UICEC,VICEC,HEFFM)
C
C**************************START PROLOGUE******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   RELAX
C
C  DESCRIPTION:                   THIS SUBROUTINE USES THE METHOD OF
C                                 RELAXATION TO SOLVE THE MOMENTUM
C                                 EQUATIONS FOR ICE MOTION
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL RELAX(UICE,VICE,ETA,ZETA,DRAGS,
C                                 DRAGA,AMASS,UVM,ERROR,THETA,UICEC,
C                                 VICEC,HEFFM)
C
C  PARAMETERS
C    NAME    TYPE      USAGE       DESCRIPTION
C    ----    ----      -----       ----------------
C    UICE    REAL      IN/OUT
C    VICE    REAL      IN/OUT
C    ETA     REAL      OUT
C    ZETA    REAL      OUT
C    DRAGS   REAL      OUT
C    DRAGA   REAL      OUT
C    AMASS   REAL      OUT
C    UVM     REAL      OUT
C    ERROR   REAL      OUT
C    THETA   REAL      OUT
C    UICEC   REAL      IN/OUT
C    VICEC   REAL      IN/OUT
C    HEFFM   REAL      OUT
C
C  COMMON BLOCKS:
C
C     BLOCK   NAME   TYPE   USAGE   NOTES
C     -----   ----   ----   -----   ------------
C     /FORCE/ FORCEX REAL   OUT
C     /FORCE/ FORCEY REAL   OUT
C     /STEP/  DELTAT REAL   OUT
C     /STEP/  DELTAX REAL   OUT
C     /STEP/  DELTAY REAL   OUT
C     /STEP/  DELTA1 REAL   OUT
C     /STEP/  DELTA  REAL   OUT
C     /PRESS1/PRESS  REAL   OUT
C
C  FILES:                         NONE 
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C********************MAINTENANCE SECTION********************************
C
C  MODULES CALLED:                FELLIP
C
C       NAME              DESCRIPTION
C       ----              ------------
C       FELLIP            DOES FINITE DIFFERENCING OF MOMENTUM
C                         TERMS CONTAINING SPACIALLY VARYING BULK
C                         AND SHEAR VISCOSITIES
C
C  LOCAL VARIABLES AND 
C           STRUCTURES:   
C
C        NAME   TYPE    DESCRIPTION
C        ----   ----    -------------
C        WFA    REAL    RELAXATION FACTOR
C
C  METHOD: 
C 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C
C*********************************************************************** 
C 
C     THE ORIGINAL SUBROUTINE RELAX WAS WRITTEN BY WILLIAM HIBLER.  THE 
C     PRESENT VERSION WAS UPDATED BY DAN MOORE.  DOCUMENTATION OF THIS 
C     SUBROUTINE WAS DONE BY BOTH HIBLER AND MOORE. 
C 
C========================================================== 
C  HIBLER IDENTICAL VERSION OF RELAX 
C  WRITTEN BY DAN MOORE   NRL  MARCH 29,1984 
C 
C  THIS VERSION OF RELAX IS NUMERICALLY IDENTICAL TO THAT IN THE 
C  ORIGINAL HIBLER CODE. 
C  IT HAS BEEN MADE MORE EFFICIENT BY CALCULATING THE COEFFICIENTS 
C  NECESSARY FOR THE RELAXATION STEPS AT THE START OF THE SUBROUTINE  
C  INSTEAD OF AT EACH STEP. 
C  GIVEN THE PRESENT ICE RHEOLOGY THERE AREA 17 INDEPENDENT COEFFICIENTS 
C  THESE ARE STORED IN AN ARRAY R(NX,NY,17). 
C  THIS ARRAY COULD BE TAKEN OR GIVEN FROM GENERAL WORKSPACE AS IT IS 
C  RESET EACH TIME THE SUBROUTINE IS ENTERED. 
C  EXTRA WORK IS NEEDED TO PRODUCE A TOTALLY VECTORIZED VERSION OF 
C  RELAX.  THIS WILL BE DONE SHORTLY. 
C================================================================= 
C
C  INCLUDE FILES:                NONE
C
C  COMPILER DEPENDENCIES:        FORTRAN 77
C
C  COMPILE OPTIONS:              -a stack   memory allocation
C                                -o zeroinc optimization options
C                                -d p       disable double precision
C                                -e z       enable debugger
C                                -m 4  message category - error message
C
C  MAKEFILE:                     LOCATION?
C
C************************END PROLOGUE***********************************
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142,NXM1=NX-1,NYM1=NY-1) 
      PARAMETER (MX=NX,MY=NY) 
C : : : : : : : : : : : : : : :  : : : : : : : : : : : : : : : : : : : : 
C 
C     EXTRA PARAMETERS FOR 4 COLOR RELAXATION SOLVER 
C 
      PARAMETER (M4X=NX1/2,M4Y=NY1/2,M4XY=M4X*M4Y) 
      PARAMETER (M4XP=M4X+1,M4XM=M4X-1) 
      PARAMETER (M1STOP=(NY/2-1)*M4X+NX/2) 
      PARAMETER (M2STOP=(NY/2-1)*M4X+M4XM) 
      PARAMETER (M3STOP=(M4Y-2)*M4X+NX/2) 
      PARAMETER (M4STOP=(M4Y-1)*M4X-1)  
C----------------------------------------------------------------------- 
C  EXTRA ARRAYS FOR 4 COLOUR SOLVER 
C 
      real*4 R(M4XY,19,4) 
      real*4 FXM(M4XY,4),FYM(M4XY,4),U4(M4XY,4),V4(M4XY,4) 
      real*4 U4C(M4XY),V4C(M4XY),FX3(M4XY),FY3(M4XY) 
      real*4 S11,S21,S12,S22,S13,S23,S14,S24 
C______________________________________________________________________ 
      DIMENSION UICE(NX,NY,3), VICE(NX,NY,3), ETA(NX1,NY1), 
     *          ZETA(NX1,NY1), DRAGS(NX1,NY1), DRAGA(NX1,NY1), 
     *          FXETA(4), FYETA(4), UICEC(NX,NY), VICEC(NX,NY), 
     *          FYZETA(4), FXZETA(4), AMASS(NX,NY), 
     *          COEFI(NX,NY), HEFFM(NX1,NY1), UVM(NX,NY) 
      COMMON / FORCE / FORCEX(NX,NY), FORCEY(NX,NY) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA 
      COMMON / PRESS / PRESS(NX1,NY1) 
      COMMON / TIME / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      T1 = SECOND() 
      PRINT 13 
 13   FORMAT(1H0,'ENTERED RELAX') 
C======================================================================= 
C    CLEAN UP ARRAYS TO ALLOW FULL 1-D VECTOR OPERATIONS 
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      M1=M4XP 
      M2=M4X 
      M3=M4XM 
      M4=M1STOP 
      M5=M2STOP 
      M6=M3STOP 
      M7=M4STOP 
      DO 1 K=1,4 
      DO 1 J=1,19 
      DO 1 I=1,M4XY 
    1 R(I,J,K)=0. 
      DO 2 K=1,4 
      DO 2 J=1,M4XY 
      FXM(J,K)=0. 
      FYM(J,K)=0. 
      U4(J,K)=0. 
      V4(J,K)=0. 
    2 CONTINUE 
      DO 3 I=1,M4XY 
      U4C(I)=0. 
      V4C(I)=0. 
      FX3(I)=0. 
      FY3(I)=0. 
    3 CONTINUE 
      ICOUNT=0 
      WFA=1.45 
      DELIN =1.0/DELTAX 
      DELIN2=0.5/(DELTAX**2)  
      K=1 
C FIRST SET U(2)=U(1) 
      DO 99 J=1,MY  
      DO 99 I=1,MX  
      UICE(I,J,2)=UICE(I,J,1) 
      VICE(I,J,2)=VICE(I,J,1) 
C NOW MAKE SURE BDRY PTS ARE EQUAL TO ZERO 
      UICE(I,J,1)=UICE(I,J,3)*UVM(I,J)  
      VICE(I,J,1)=VICE(I,J,3)*UVM(I,J)  
 99   CONTINUE 
C NOW SET UP COEFFICIENTS OF DIAGONAL COMPONENTS  
      DO 102 J=1,MY 
      DO 102 I=1,MX 
      COEFI(I,J)=1.0/(AMASS(I,J)/DELTAT+2.0*THETA*(0.5*DRAGS(I,J) 
     1  +2.0*((ETA(I,J)+ETA(I+1,J)+ETA(I,J+1)+ETA(I+1,J+1)) 
     2  +.5*(ZETA(I,J)+ZETA(I+1,J)+ZETA(I,J+1)+ZETA(I+1,J+1)) 
     3  )/(4.0*(DELTAX**2)))) 
 102  CONTINUE 
C NOW CALCULATE ALL FUNCTIONS OF PREVIOUS U AND V VALUES 
      TTHETA=2.0*(1.0-THETA)  
C====================================================================== 
C  MODIFY INITIAL LOOP TO SKIP UNNECESSARY CALCULATIONS WHEN THETA=1  
C  MODIFY LOOP TO LOAD COEFFICIENTS INTO 4 COLOUR ARRAY 
C...................................................................... 
      DO 111 K4=1,4 
      DO 111 IJ4=1,M4XY 
      J4=IJ4/M4X+1  
      I4=IJ4-(J4-1)*M4X 
      J=2*J4-(4-K4)/2 
      IF(J.LE.1.OR.J.GE.NY)GO TO 111 
      I=2*I4-MOD(K4,2) 
      IF(I.LE.1.OR.I.GE.NX)GO TO 111 
      FXM(IJ4,K4)=UICE(I,J,2)*AMASS(I,J)/DELTAT+FORCEX(I,J) 
      FYM(IJ4,K4)=VICE(I,J,2)*AMASS(I,J)/DELTAT+FORCEY(I,J) 
      IF(TTHETA.EQ.0.)GO TO 111 
      CALL FELLIP(UICE,VICE,ETA,FXETA,I,J,2) 
      CALL FELLIP(UICE,VICE,ZETA,FXZETA,I,J,2) 
      CALL FELLIP(VICE,UICE,ETA,FYETA,I,J,2) 
      CALL FELLIP(VICE,UICE,ZETA,FYZETA,I,J,2) 
      FX0=0.5*(FXETA(1)+FXZETA(1)+FXETA(2)+FXETA(3)+FXZETA(4)-FXETA(4)) 
      FX0=TTHETA*FX0 
      FX1=-TTHETA*0.5*DRAGS(I,J)*UICE(I,J,2) 
      FX2=TTHETA*0.5*DRAGA(I,J)*VICE(I,J,2) 
      FY0=0.5*(FYETA(1)+FYETA(2)+FYZETA(2)+FYZETA(3)-FYETA(3)+FYETA(4)) 
      FY0=FY0*TTHETA 
      FY1=-TTHETA*0.5*DRAGS(I,J)*VICE(I,J,2) 
      FY2=-TTHETA*0.5*DRAGA(I,J)*UICE(I,J,2) 
      FXC=AMASS(I,J)*0.5*TTHETA* 
     1  (UICEC(I,J)*(UICE(I+1,J,2)-UICE(I-1,J,2)) 
     2  +VICEC(I,J)*(UICE(I,J+1,2)-UICE(I,J-1,2)))/(2.0*DELTAX) 
      FXM(IJ4,K4)=FXM(IJ4,K4)+FX0+FX1+FX2-FXC 
      FYC=AMASS(I,J)*0.5*TTHETA* 
     1  (UICEC(I,J)*(VICE(I+1,J,2)-VICE(I-1,J,2)) 
     2  +VICEC(I,J)*(VICE(I,J+1,2)-VICE(I,J-1,2)))/(2.0*DELTAX) 
      FYM(IJ4,K4)=FYM(IJ4,K4)+FY0+FY1+FY2-FYC 
  111 CONTINUE 
C====================================================================== 
C  SECTION TO CALCULATE RELAXATION COEFFICIENTS EXPLICITLY  
C THE ALGORITHM ASSUMES THAT THE RIGHT HAND SIDE FOR THE TWO SIMULTANEOU 
C EQUATIONS TO BE SOLVED AT EACH RELATION STEP IS 
C 
C                      \ 0   R(4)   0 \         \R(11) R(12) R(13)\ 
C                      \              \         \                 \ 
C RHS(X) =  FXM(I,J) + \R(2)   0  R(3)\U(I,J) + \R(8)  R(9)  R(10)\V(I,J 
C                      \              \         \                 \ 
C                      \ 0   R(1)   0 \         \R(5)  R(6)  R(7) \ 
C 
C 
C                      \R(11)-R(12) R(13)\         \ 0   R(17)   0  \ 
C                      \                 \         \                \ 
C RHS(Y) =  FYM(I,J) + \-R(8) R(9) -R(10)\U(I,J) + \R(15)  0   R(16)\V(I 
C                      \                 \         \                \ 
C                      \R(5) -R(6)  R(7) \         \ 0   R(14)   0  \ 
C 
C  WARNING ]]  CHANGE THE PHYSICS AND YOU WILL NEED TO CHANGE BOTH THIS 
C  SECTION AND THE RELAXATION IF THE CHANGE INVOLVES POINTS OTHER THAN 
C  THOSE INDICATED ABOVE OR CHANGES IN THE SYMMETRIES INDICATED ABOVE 
C  [NO REAL PROBLEM, JUST ADD EXTRA VALUES TO ARRAY R AND EXTRA LINES 
C  TO THE DEFINITIONS OF FX3 AND FY3 BELOW 
C_______________________________________________________________________ 
      S1TH=.5*DELIN2*THETA 
      S2TH=2.*S1TH  
      SDL=.5*THETA*DELIN 
      DO 10300 K4=1,4 
      DO 10300 IJ4=1,M4XY 
      J4=IJ4/M4X+1  
      I4=IJ4-(J4-1)*M4X 
      J=2*J4-(4-K4)/2 
      IF(J.LE.1.OR.J.GE.NY)GO TO 10300  
      I=2*I4-MOD(K4,2) 
      IF(I.LE.1.OR.I.GE.NX)GO TO 10300  
C...................ANALYTIC COEFFCIENTS 
      R(IJ4,1,K4)=S2TH*(ETA(I,J)+ETA(I+1,J)) 
     +                +SDL*AMASS(I,J)*VICEC(I,J)  
      R(IJ4,2,K4)=S2TH*(ETA(I,J+1)+ETA(I,J) 
     +                +ZETA(I,J+1)+ZETA(I,J)) 
     +                +SDL*AMASS(I,J)*UICEC(I,J)  
      R(IJ4,3,K4)=S2TH*(ETA(I+1,J+1)+ETA(I+1,J) 
     +                +ZETA(I+1,J+1)+ZETA(I+1,J)) 
     +                   -AMASS(I,J)*UICEC(I,J)*SDL 
      R(IJ4,4,K4)=S2TH*(ETA(I+1,J+1)+ETA(I,J+1))  
     +                   -AMASS(I,J)*VICEC(I,J)*SDL 
      R(IJ4,5,K4)=S1TH*ZETA(I,J) 
      R(IJ4,6,K4)=-S1TH*(2.*(ETA(I,J)-ETA(I+1,J)) 
     +                    +ZETA(I+1,J)-ZETA(I,J)) 
      R(IJ4,7,K4)=-S1TH*ZETA(I+1,J) 
      R(IJ4,8,K4)=-S1TH*(2.*(ETA(I,J+1)-ETA(I,J)) 
     +                    +ZETA(I,J)-ZETA(I,J+1)) 
      R(IJ4,9,K4)=S1TH*(ZETA(I+1,J)+ZETA(I,J+1) 
     +                      -ZETA(I,J)-ZETA(I+1,J+1)) 
      R(IJ4,10,K4)=S1TH*(2.*(ETA(I+1,J+1)-ETA(I+1,J)) 
     +                   +(ZETA(I+1,J)-ZETA(I+1,J+1))) 
      R(IJ4,11,K4)=-S1TH*ZETA(I,J+1) 
      R(IJ4,12,K4)=S1TH*(2.*(ETA(I,J+1)-ETA(I+1,J+1)) 
     +                   +(ZETA(I+1,J+1)-ZETA(I,J+1))) 
      R(IJ4,13,K4)=S1TH*ZETA(I+1,J+1) 
      R(IJ4,14,K4)=S2TH*(ETA(I,J)+ETA(I+1,J) 
     +                +ZETA(I,J)+ZETA(I+1,J)) 
     +                +SDL*AMASS(I,J)*VICEC(I,J)  
      R(IJ4,15,K4)=S2TH*(ETA(I,J)+ETA(I,J+1)) 
     +                +SDL*AMASS(I,J)*UICEC(I,J)  
      R(IJ4,16,K4)=S2TH*(ETA(I+1,J+1)+ETA(I+1,J)) 
     +                   -AMASS(I,J)*UICEC(I,J)*SDL 
      R(IJ4,17,K4)=S2TH*(ETA(I+1,J+1)+ETA(I,J+1)  
     +                +ZETA(I+1,J+1)+ZETA(I,J+1)) 
     +                   -AMASS(I,J)*VICEC(I,J)*SDL 
      R(IJ4,18,K4)=THETA*DRAGA(I,J)*COEFI(I,J) 
      R(IJ4,19,K4)=COEFI(I,J)*UVM(I,J)/(1.+R(IJ4,18,K4)**2) 
10300 CONTINUE 
C 
C  RELAXATION COEFFICIENTS CALCULATED, NOW RELAX DIRECTLY 
C 
C------------------------------------------------------------ 
C  LOAD UICE AND VICE INTO U4 AND V4 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      DO 104 K4=1,4 
      DO 104 IJ4=1,M4XY 
      J4=IJ4/M4X+1  
      I4=IJ4-(J4-1)*M4X 
      J=2*J4-(4-K4)/2 
      IF(J.LT.1.OR.J.GT.NY)GO TO 1041 
      I=2*I4-MOD(K4,2) 
      IF(I.LT.1.OR.I.GT.NX)GO TO 1041 
      U4(IJ4,K4)=UICE(I,J,1)  
      V4(IJ4,K4)=VICE(I,J,1)  
      GO TO 104 
 1041 U4(IJ4,K4)=0. 
      V4(IJ4,K4)=0. 
  104 CONTINUE 
C 
C    START OF 4 COLOUR RELAXATION LOOP  
C 
  100 CONTINUE 
C=================================================================== 
C  NOW RELAX ON GRID 1 
C................................................................... 
C 
      DO 10310 I=M4XP,M1STOP  
      FX3(I) = FXM(I,1) 
     +  + U4(I-1   ,2) * R(I,2,1)  +  U4(I     ,2) * R(I,3,1) 
     +  + U4(I-M4X ,3) * R(I,1,1)  +  U4(I     ,3) * R(I,4,1) 
     +  + V4(I-1   ,2) * R(I,8,1)  +  V4(I     ,2) * R(I,10,1) 
     +  + V4(I-M4X ,3) * R(I,6,1)  +  V4(I     ,3) * R(I,12,1) 
     +  + V4(I-M4XP,4) * R(I,5,1)  +  V4(I     ,4) * R(I,13,1) 
     +  + V4(I-1   ,4) * R(I,11,1) +  V4(I-M4X ,4) * R(I,7,1) 
     +  + V4(I,1)      * R(I,9,1) 
      FY3(I)  =  FYM(I,1) 
     +  + V4(I-1   ,2) * R(I,15,1) +  V4(I     ,2) * R(I,16,1) 
     +  + V4(I-M4X ,3) * R(I,14,1) +  V4(I     ,3) * R(I,17,1) 
     +  - U4(I-1   ,2) * R(I,8,1)  -  U4(I     ,2) * R(I,10,1) 
     +  - U4(I-M4X ,3) * R(I,6,1)  -  U4(I     ,3) * R(I,12,1) 
     +  + U4(I-M4XP,4) * R(I,5,1)  +  U4(I     ,4) * R(I,13,1) 
     +  + U4(I-1   ,4) * R(I,11,1) +  U4(I-M4X ,4) * R(I,7,1) 
     +  + U4(I,1)      * R(I,9,1) 
10310 CONTINUE 
C 
C  NOW CLEAN UP ARRAYS 
C 
      DO 10311 I=1,M4X 
      U4C(I)=0. 
      V4C(I)=0. 
10311 CONTINUE 
C 
      DO 10312 I=M1STOP,M4XY  
      U4C(I)=0. 
      V4C(I)=0. 
10312 CONTINUE 
C 
C  NOW CALCULATE VELOCITY CORRECTIONS 
C 
      DO 10313 I=M4XP,M1STOP  
      U4C(I)=((FX3(I)+FY3(I)*R(I,18,1))*R(I,19,1)-U4(I,1))  
     + *WFA 
      V4C(I)=((FY3(I)-FX3(I)*R(I,18,1))*R(I,19,1)-V4(I,1))  
     + *WFA 
10313 CONTINUE 
C 
C  NOW ADD IN CORRECTIONS 
C 
      DO 10314 I=M4XP,M1STOP  
      U4(I,1) = U4(I,1) + U4C(I) 
      V4(I,1) = V4(I,1) + V4C(I) 
10314 CONTINUE 
C 
C  NOW FIND VALUE OF LARGEST CORRECTION 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     had to change this is run on cray 
c
c      U4C(1;M4XY)=VABS(U4C(1;M4XY);U4C(1;M4XY)) 
c      V4C(1;M4XY)=VABS(V4C(1;M4XY);V4C(1;M4XY)) 
c      S11=Q8SMAX(U4C(1;M4XY)) 
c      S21=Q8SMAX(V4C(1;M4XY)) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
       call qmax(u4c,v4c,s11,s21)
C=================================================================== 
C  NOW RELAX ON GRID 2 
C................................................................... 
C 
      DO 10320 I=M4X,M2STOP 
      FX3(I) = FXM(I,2) 
     +  + U4(I     ,1) * R(I,2,2)  +  U4(I+1   ,1) * R(I,3,2) 
     +  + U4(I-M4X ,4) * R(I,1,2)  +  U4(I     ,4) * R(I,4,2) 
     +  + V4(I     ,1) * R(I,8,2)  +  V4(I+1   ,1) * R(I,10,2) 
     +  + V4(I-M4X ,4) * R(I,6,2)  +  V4(I     ,4) * R(I,12,2) 
     +  + V4(I-M4X ,3) * R(I,5,2)  +  V4(I+1   ,3) * R(I,13,2) 
     +  + V4(I     ,3) * R(I,11,2) +  V4(I-M4XM,3) * R(I,7,2) 
     +  + V4(I,2)      * R(I,9,2) 
      FY3(I)  =  FYM(I,2) 
     +  + V4(I     ,1) * R(I,15,2) +  V4(I+1   ,1) * R(I,16,2) 
     +  + V4(I-M4X ,4) * R(I,14,2) +  V4(I     ,4) * R(I,17,2) 
     +  - U4(I     ,1) * R(I,8,2)  -  U4(I+1   ,1) * R(I,10,2) 
     +  - U4(I-M4X ,4) * R(I,6,2)  -  U4(I     ,4) * R(I,12,2) 
     +  + U4(I-M4X ,3) * R(I,5,2)  +  U4(I+1   ,3) * R(I,13,2) 
     +  + U4(I     ,3) * R(I,11,2) +  U4(I-M4XM,3) * R(I,7,2) 
     +  + U4(I,2)      * R(I,9,2) 
10320 CONTINUE 
C 
C  NOW CLEAN UP ARRAYS 
C 
      U4C(M1STOP)=0. 
      V4C(M1STOP)=0. 
C 
C  NOW CALCULATE VELOCITY CORRECTIONS 
C 
      DO 10323 I=M4X,M2STOP 
      U4C(I)=((FX3(I)+FY3(I)*R(I,18,2))*R(I,19,2)-U4(I,2))  
     + *WFA 
      V4C(I)=((FY3(I)-FX3(I)*R(I,18,2))*R(I,19,2)-V4(I,2))  
     + *WFA 
10323 CONTINUE 
C 
C  NOW ADD IN CORRECTIONS 
C 
      DO 10324 I=M4X,M2STOP 
      U4(I,2) = U4(I,2) + U4C(I) 
      V4(I,2) = V4(I,2) + V4C(I) 
10324 CONTINUE 
C 
C  NOW FIND VALUE OF LARGEST CORRECTION 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     make cray correction
c
c      U4C(1;M4XY)=VABS(U4C(1;M4XY);U4C(1;M4XY)) 
c      V4C(1;M4XY)=VABS(V4C(1;M4XY);V4C(1;M4XY)) 
c      S12=Q8SMAX(U4C(1;M4XY)) 
c      S22=Q8SMAX(V4C(1;M4XY)) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
       call qmax(u4c,v4c,s12,s22)
C=================================================================== 
C  NOW RELAX ON GRID 3 
C................................................................... 
C 
      DO 10330 I=2,M3STOP 
      FX3(I) = FXM(I,3) 
     +  + U4(I-1   ,4) * R(I,2,3)  +  U4(I     ,4) * R(I,3,3) 
     +  + U4(I     ,1) * R(I,1,3)  +  U4(I+M4X ,1) * R(I,4,3) 
     +  + V4(I-1   ,4) * R(I,8,3)  +  V4(I     ,4) * R(I,10,3) 
     +  + V4(I     ,1) * R(I,6,3)  +  V4(I+M4X ,1) * R(I,12,3) 
     +  + V4(I-1   ,2) * R(I,5,3)  +  V4(I+M4X ,2) * R(I,13,3) 
     +  + V4(I+M4XM,2) * R(I,11,3) +  V4(I     ,2) * R(I,7,3) 
     +  + V4(I,3)      * R(I,9,3) 
      FY3(I)  =  FYM(I,3) 
     +  + V4(I-1   ,4) * R(I,15,3) +  V4(I     ,4) * R(I,16,3) 
     +  + V4(I     ,1) * R(I,14,3) +  V4(I+M4X ,1) * R(I,17,3) 
     +  - U4(I-1   ,4) * R(I,8,3)  -  U4(I     ,4) * R(I,10,3) 
     +  - U4(I     ,1) * R(I,6,3)  -  U4(I+M4X ,1) * R(I,12,3) 
     +  + U4(I-1   ,2) * R(I,5,3)  +  U4(I+M4X ,2) * R(I,13,3) 
     +  + U4(I+M4XM,2) * R(I,11,3) +  U4(I     ,2) * R(I,7,3) 
     +  + U4(I,3)      * R(I,9,3) 
10330 CONTINUE 
C 
C  NOW CLEAN UP ARRAYS 
C 
      DO 10332 I=M3STOP,M2STOP 
      U4C(I)=0. 
      V4C(I)=0. 
10332 CONTINUE 
C 
C  NOW CALCULATE VELOCITY CORRECTIONS 
C 
      DO 10333 I=2,M3STOP 
      U4C(I)=((FX3(I)+FY3(I)*R(I,18,3))*R(I,19,3)-U4(I,3))  
     + *WFA 
      V4C(I)=((FY3(I)-FX3(I)*R(I,18,3))*R(I,19,3)-V4(I,3))  
     + *WFA 
10333 CONTINUE 
C 
C  NOW ADD IN CORRECTIONS 
C 
      DO 10334 I=2,M3STOP 
      U4(I,3) = U4(I,3) + U4C(I) 
      V4(I,3) = V4(I,3) + V4C(I) 
10334 CONTINUE 
C 
C  NOW FIND VALUE OF LARGEST CORRECTION 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     make cray corrections
c
c      U4C(1;M4XY)=VABS(U4C(1;M4XY);U4C(1;M4XY)) 
c      V4C(1;M4XY)=VABS(V4C(1;M4XY);V4C(1;M4XY)) 
c      S13=Q8SMAX(U4C(1;M4XY)) 
c      S23=Q8SMAX(V4C(1;M4XY)) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
       call qmax(u4c,v4c,s13,s23)
C=================================================================== 
C  NOW RELAX ON GRID 4 
C................................................................... 
C 
      DO 10340 I=1,M4STOP 
      FX3(I) = FXM(I,4) 
     +  + U4(I     ,3) * R(I,2,4)  +  U4(I+1   ,3) * R(I,3,4) 
     +  + U4(I     ,2) * R(I,1,4)  +  U4(I+M4X ,2) * R(I,4,4) 
     +  + V4(I     ,3) * R(I,8,4)  +  V4(I+1   ,3) * R(I,10,4) 
     +  + V4(I     ,2) * R(I,6,4)  +  V4(I+M4X ,2) * R(I,12,4) 
     +  + V4(I     ,1) * R(I,5,4)  +  V4(I+M4XP,1) * R(I,13,4) 
     +  + V4(I+M4X ,1) * R(I,11,4) +  V4(I+1   ,1) * R(I,7,4) 
     +  + V4(I,4)      * R(I,9,4) 
      FY3(I)  =  FYM(I,4) 
     +  + V4(I     ,3) * R(I,15,4) +  V4(I+1   ,3) * R(I,16,4) 
     +  + V4(I     ,2) * R(I,14,4) +  V4(I+M4X ,2) * R(I,17,4) 
     +  - U4(I     ,3) * R(I,8,4)  -  U4(I+1   ,3) * R(I,10,4) 
     +  - U4(I     ,2) * R(I,6,4)  -  U4(I+M4X ,2) * R(I,12,4) 
     +  + U4(I     ,1) * R(I,5,4)  +  U4(I+M4XP,1) * R(I,13,4) 
     +  + U4(I+M4X ,1) * R(I,11,4) +  U4(I+1   ,1) * R(I,7,4) 
     +  + U4(I,4)      * R(I,9,4) 
10340 CONTINUE 
C 
C  NOW CLEAN UP ARRAYS 
C 
      U4C(M3STOP)=0. 
      V4C(M3STOP)=0. 
C 
C  NOW CALCULATE VELOCITY CORRECTIONS 
C 
      DO 10343 I=1,M4STOP 
      U4C(I)=((FX3(I)+FY3(I)*R(I,18,4))*R(I,19,4)-U4(I,4))  
     + *WFA 
      V4C(I)=((FY3(I)-FX3(I)*R(I,18,4))*R(I,19,4)-V4(I,4))  
     + *WFA 
10343 CONTINUE 
C 
C  NOW ADD IN CORRECTIONS 
C 
      DO 10344 I=1,M4STOP 
      U4(I,4) = U4(I,4) + U4C(I) 
      V4(I,4) = V4(I,4) + V4C(I) 
10344 CONTINUE 
C 
C  NOW FIND VALUE OF LARGEST CORRECTION 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
c     make sun correction
c
c      U4C(1;M4XY)=VABS(U4C(1;M4XY);U4C(1;M4XY)) 
c      V4C(1;M4XY)=VABS(V4C(1;M4XY);V4C(1;M4XY)) 
c      S14=Q8SMAX(U4C(1;M4XY)) 
c      S24=Q8SMAX(V4C(1;M4XY)) 
c 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       call qmax(u4c,v4c,s14,s24)
C 
C   NOW TIDY UP LOOP AND DECIDE IF TO CONTINUE 
C 
      ICOUNT=ICOUNT+1 
      IF(ICOUNT.GT.1000)GO TO 201 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     had to change hmax1 to amax1 and extend to dble
c
c      S1 = EXTEND(HMAX1(S11,S12,S21,S22,S13,S23,S14,S24)) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      s1 = dble(amax1(s11,s12,s21,s22,s13,s23,s14,s24)) 
      IF(S1.LE.ERROR)GO TO 200 
      GO TO 100 
C 
C    CONVERGENCE FAILS SECTION 
C 
  201 CONTINUE 
      PRINT 11,ICOUNT 
   11 FORMAT(1X,'NO CONVERGENCE AFTER ',I8,'  ITERATIONS')  
C 
C    WIND UP RELAX SECTION 
C 
  200 CONTINUE 
C------------------------------------------------------------ 
C  RELOAD UICE AND VICE 
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      DO 210 K4=1,4 
      DO 210 IJ4=1,M4XY 
      J4=IJ4/M4X+1  
      I4=IJ4-(J4-1)*M4X 
      J=2*J4-(4-K4)/2 
      IF(J.LE.1.OR.J.GE.NY)GO TO 210 
      I=2*I4-MOD(K4,2) 
      IF(I.LE.1.OR.I.GE.NX)GO TO 210 
      UICE(I,J,1)=U4(IJ4,K4)  
      VICE(I,J,1)=V4(IJ4,K4)  
  210 CONTINUE 
      PRINT 500,S1  
      PRINT 501,ICOUNT 
500   FORMAT(1X,'MAX ERROR AND U AND V POWER  ',3E12.5) 
501   FORMAT(1X,'NUMBER OF ITERATIONS ARE   ',I20) 
      T2 = SECOND() 
      RELAXS = RELAXS + (T2 - T1) 
      RETURN 
      END 
C
      SUBROUTINE FELLIP(UICE,VICE,ETA,F,I,J,K)
C
C************************START PROLOGUE********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   FELLIP
C
C  DESCRIPTION:                   PERFORMS FINITE DIFFERENCING OF 
C                                 MOMENTUM TERMS CONTAINING SPACIALLY
C                                 VARYING BULK AND SHEAR VISCOSITIES
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL FELLIP(UICE,VICE,ETA,F,I,J,K)
C
C  PARAMETERS:
C     NAME     TYPE    USAGE     DESCRIPTION
C     ----     ----    -----     -------------
C     UICE     REAL    OUT
C     VICE     REAL    OUT
C     ETA      REAL    OUT
C     F        REAL    IN
C     I        INT     OUT
C     J        INT     OUT
C     K        INT     OUT
C
C  COMMON BLOCKS:
C
C     BLOCK     NAME    TYPE    USAGE     NOTES
C     -----     ----    ----    -----     -----
C     /STEP/    DELTAT  REAL    OUT
C     /STEP/    DELTAX  REAL    OUT
C     /STEP/    DELTAY  REAL    OUT
C     /STEP/    DELTA1  REAL    OUT
C     /STEP/    DELTA   REAL    OUT
C
C  FILES:                         NONE 
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE 
C
C  ERROR CONDITIONS:              NONE
C
C************************MAINTENANCE SECTION****************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:           NONE
C
C  METHOD:        SUBROUTINE FELLIP PREFORMS THE FINITE DIFFERENCING OF
C                 MOMENTUM TERMS CONTAINING SPACIALLY VARYING BULK AND
C                 SHEAR VISCOSITIES.  INPUTS INTO THIS SUBROUTINE ARE    
C                 THE ICE DRIFT VELOCITY, UICE AND VICE, ETA, EITHER
C                 THE NON LINEAR SHEAR OR BULK VISCOSITY, I THE ITH
C                 GRID POINT AN J THE JTH GRID POINT AND K THE TIME LEVEL.
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         FORTRAN 77
C
C  COMPILE OPTIONS:               -a stack   memory allocation
C                                 -o zeroinc optimization options
C                                 -d p       disable double precision
C                                 -e z       enable debugger
C                                 -m 4  message category - error message
C
C  MAKEFILE:                      LOCATION ?
C
C**************************END PROLOGUE********************************** 
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
C     SUBROUTINE FELLIP PERFORMS THE FINITE DIFFERENCING OF MOMENTUM  
C     TERMS CONTAINING SPACIALLY VARYING BULK AND SHEAR VISCOSITIES.  
C     INPUTS INTO THIS SUBROUTINE ARE THE ICE DRIFT VELOCITY UICE AND 
C     VICE, ETA EITHER THE NON LINEAR SHEAR OR BULK VISCOSITY, I THE  
C     ITH GRID POINT AND J THE JTH GRID POINT AND K THE TIME LEVEL. 
C     THE OUTPUT ARRAY F, RETURNS THE FINITE DIFFERENCE TERMS. 
C 
C*********************************************************************** 
C 
      DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),ETA(NX1,NY1),F(4) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA 
      S1=.5/(DELTAX**2) 
      F(1)=S1*(UICE(I+1,J,K)*(ETA(I+1,J+1)+ETA(I+1,J)) 
     *-UICE(I,J,K)*(ETA(I+1,J+1)+ETA(I,J)+ETA(I+1,J)+ETA(I,J+1)) 
     *+UICE(I-1,J,K)*(ETA(I,J+1)+ETA(I,J))) 
      F(2)=S1*(UICE(I,J+1,K)*(ETA(I+1,J+1)+ETA(I,J+1)) 
     *-UICE(I,J,K)*(ETA(I+1,J+1)+ETA(I,J)+ETA(I+1,J)+ETA(I,J+1)) 
     *+UICE(I,J-1,K)*(ETA(I,J)+ETA(I+1,J))) 
      F(3)=S1*(VICE(I-1,J-1,K)*ETA(I,J)+VICE(I,J-1,K)*(-ETA(I,J) 
     *+ETA(I+1,J))-VICE(I+1,J-1,K)*ETA(I+1,J)+VICE(I-1,J,K)*(-ETA(I,J+1) 
     *+ETA(I,J))  +VICE(I,J,K)*(-ETA(I,J)-ETA(I+1,J+1)+ETA(I+1,J) 
     *+ETA(I,J+1))) 
      F(3)=F(3)+S1*(VICE(I+1,J,K)*(-ETA(I+1,J)+ETA(I+1,J+1)) 
     *-VICE(I-1,J+1,K)*ETA(I,J+1) 
     *+VICE(I,J+1,K)*(-ETA(I+1,J+1)+ETA(I,J+1)) 
     *+VICE(I+1,J+1,K)*ETA(I+1,J+1)) 
      F(4)=S1*(VICE(I-1,J-1,K)*ETA(I,J)+VICE(I,J-1,K)*(-ETA(I+1,J) 
     *+ETA(I,J))-VICE(I+1,J-1,K)*ETA(I+1,J)+VICE(I-1,J,K)*(ETA(I,J+1) 
     *-ETA(I,J))+VICE(I,J,K)*(ETA(I,J+1)+ETA(I+1,J)-ETA(I,J)-ETA(I+1, 
     *J+1))) 
      F(4)=F(4)+S1*(VICE(I+1,J,K)*(ETA(I+1,J)-ETA(I+1,J+1)) 
     *-VICE(I-1,J+1,K)*ETA(I,J+1) 
     *+VICE(I,J+1,K)*(ETA(I+1,J+1)-ETA(I,J+1)) 
     *+VICE(I+1,J+1,K)*ETA(I+1,J+1)) 
      F(3)=F(3)*.5  
      F(4)=F(4)*.5  
      RETURN 
      END 
      SUBROUTINE GROWTH(HEFF,AREA,HO,A22,HDIFF1,FO,HCORR,HEFFM,OUT, 
     *                  GAREA)
C
C**************************START PROLOGUE*******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A 
C
C  MODULE NAME:                   GROWTH
C
C  DESCRIPTION:                   SUBROUTINE GROWTH CALCULATES THE
C                                 CHANGES IN ICE THICKNESS AND ICE
C                                 CONCENTRATION EACH TIME STEP
C 
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        NONE
C
C  USAGE:                         CALL GROWTH(HEFF,AREA,HO,A22,HDIFF1,FO,
C                                             HCORR,HEFFM,OUT,GAREA)
C  PARAMETERS:
C     NAME      TYPE     USAGE      DESCRIPTION
C     ----      ----     -----      --------------
C     HEFF      REAL     IN/OUT
C     AREA      REAL     IN/OUT
C     HO        REAL     OUT
C     A22       REAL     OUT
C     HDIFF1    REAL     IN/OUT
C     FO        REAL     IN
C     HCORR     REAL     IN
C     HEFFM     REAL     OUT
C     OUT       REAL     OUT
C     GAREA     REAL     IN
C
C  COMMON BLOCKS:
C
C    BLOCK     NAME     TYPE     USAGE     DESCRIPTION
C    -----     ----     ----     -----     --------------
C    /GROW/    YNEG     REAL     OUT
C    /GROW/    FHEFF    REAL     IN/OUT
C    /STEP/    DELTAT   REAL     OUT
C    /STEP/    DELTAX   REAL     OUT
C    /STEP/    DELTAY   REAL     OUT
C    /STEP/    DELTA1   REAL     OUT
C    /STEP/    DELTA    REAL     OUT
C
C  FILES:                         NONE
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C************************MAINTENANCE SECTION********************************
C
C  MODULES CALLED: 
C          NAME      DESCRIPTION
C          ----      -----------
C          RNEGT     REMOVES "NEGATIVE ICE" FROM THE ICE THICKNESS FIELD
C
C  LOCAL VARIABLES AND 
C           STRUCTURES:           NONE 
C
C  METHOD:                        THIS SUBROUTINE CALCULATES THE CHANGE
C                                 OF ICE THICKNESS AND ICE CONCENTRATION
C                                 THIS SUBROUTINE MUST BECALLED AFTER
C                                 ADVECTION.  GHEFF IS THE AVERAGE GROWTH
C                                 TENDENCY FOR THIS TIME STEP.  GAREA IS
C                                 AVERAGE COMPACTNESS TENDENCY. INPUTS
C                                 TO THIS SUBROUTINE AR ICE THICKNESS,
C                                 HEFF, THE CONCNENTRATION, AREA, HO THE
C                                 MINIMUM ICE THICKNESS, A22 THE MINIMUM
C                                 ICE CONCENTRATION, AND THE LAND-SEA
C                                 MASKS HEFFM AND OUT.  OUTPUTS ARE THE
C                                 NEW GROWTH OF THIN ICE, HDIFF1, FO THE
C                                 FROWTH RATE OF THIN ICE, HCORR, THE
C                                 ADDITIONAL ICE TO BE MELTED FOR THE
C                                 MIXED LAYER BALANCE AND GAREA, THE CHANGE
C                                 OF AREAL ICE EXTENT DUE TO MELTING AND
C                                 FREEZING.
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         Fortran 77 
C
C  COMPILER OPTIONS:              -a stack   memory allocation
C                                 -o zeroinc optimization options
C                                 -d p       disables double precision
C                                 -e z       enables debugger 
C                                 -m 4  message category - error message
C 
C  MAKEFILE:                      LOCATION?
C
C************************END PROLOGUE**************************************** 
      PARAMETER (NX1=80,NY1=142) 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION GAREA(NX1,NY1), HCORR(NX1,NY1) 
C 
C*********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C*********************************************************************** 
C 
C 
      DIMENSION HEFF(NX1,NY1,3), AREA(NX1,NY1,3), 
     *          HDIFF1(NX1,NY1),FO(NX1,NY1),GHEFF(NX1,NY1)  
      DIMENSION HEFFM(NX1,NY1), OUT(NX1,NY1) 
      COMMON/ GROW /YNEG(NX1,NY1),FHEFF(NX1,NY1)
      COMMON / STEP/ DELTAT,DELTAX,DELTAY,DELTA1,DELTA  
      T1 = SECOND() 
C 
C 
C     SOLVE FOR THE ACTUAL GROWTH OF THICK AND THIN ICE OVER A 
C     TIME STEP 
C 
      DO 101 J = 1,NY1 
         DO 100 I = 1,NX1 
C 
         GHEFF(I,J)=-DELTAT*FHEFF(I,J) 
         GAREA(I,J) = DELTAT * FO(I,J) 
         GHEFF(I,J)=-1.0*AMIN1(HEFF(I,J,1),GHEFF(I,J)) 
         HDIFF1(I,J)=-DELTAT*HDIFF1(I,J) 
         GAREA(I,J) = AMAX1(0.0,GAREA(I,J)) 
         HDIFF1(I,J)=-1.0*AMIN1(HEFF(I,J,1),HDIFF1(I,J)) 
         HCORR(I,J)=AMIN1(0.0,GHEFF(I,J))  
C 
C     AREAL CHANGE DUE TO FREEZING AND MELTING.  FIRST TERM: 
C     AREAL DECREASE OF OPEN WATER DUE TO FREEZING.  SECOND TERM 
C     IS AREAL INCREASE OF OPEN WATER DUE TO MELTING. 
C 
         GAREA(I,J)=2.0*(1.0-AREA(I,J,2))*GAREA(I,J)/HO+.5*HCORR(I,J)* 
     *   AREA(I,J,2)/(HEFF(I,J,1) + .00001) 
C 
C     CORRECT AREA AND HEFF 
C 
         AREA(I,J,1) = AREA(I,J,1) + GAREA(I,J) 
         HEFF(I,J,1) = HEFF(I,J,1) + GHEFF(I,J) * OUT(I,J) 
C  CALCULATE ADDITIONAL ICE TO BE MELTED FOR MIXED LAYER BALANCE 
         HCORR(I,J)=GHEFF(I,J)-DELTAT*FHEFF(I,J)
 100     CONTINUE 
 101  CONTINUE 
      CALL RNEGT(HEFF,YNEG,HCORR) 
C 
C NOW ZERO OUTSIDE POINTS 
      DO 111 J=1,NY1 
         DO 110 I=1,NX1 
         AREA(I,J,1)=AREA(I,J,1)*HEFFM(I,J) 
         HEFF(I,J,1)=HEFF(I,J,1)*HEFFM(I,J) 
C 
C NOW SET AREA(I,J,1)=0 WHERE NO ICE IS 
         AREA(I,J,1)=AMIN1(AREA(I,J,1),HEFF(I,J,1)/.0001) 
C 
C NOW TRUNCATE AREA 
         AREA(I,J,1) = AMIN1(1.0,AREA(I,J,1)) 
         AREA(I,J,1)=AMAX1(A22,AREA(I,J,1)) 
C NOW CALCULATE ADDITIONAL ICE TO BE MELTED FOR MIXED LAYER BALANCE 
C 
         HCORR(I,J)= GHEFF(I,J) - DELTAT * FHEFF(I,J) 
C 
C NOW STORE COMMON GROWTH RATE 
         FHEFF(I,J)=GHEFF(I,J) 
         FO(I,J)=HDIFF1(I,J)
 110     CONTINUE 
 111  CONTINUE 
      T2 = SECOND() 
      GRWTHS = GRWTHS + (T2 - T1) 
      RETURN 
      END
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c     FNOC - This is a change in heat due to our test atmospheric
c            forcing data
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
c      SUBROUTINE HEAT(GRDI,GRDJ,HEFF,AREA,FO,HDIFF1,GAIRX,GAIRY,ITAU, 
c     *                IDTG) 
      SUBROUTINE HEAT(GRDI,GRDJ,HEFF,AREA,FO,HDIFF1,ITAU, 
     *                IDTG)
C
C********************START PROLOGUE************************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   HEAT
C
C  DESCRIPTION:                   THIS SUBROUTINE CALLS SUBROUTINE
C                                 ICEINTL TO READ IN THE FNOC FORCING
C                                 CALCULATES VARIABLES NEEDED FOR
C                                 THE HEAT BUDGET AND THEN CALLS BUDGET
C 
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL HEAT(GRDI,GRDJ,HEFF,AREA,FO,HDIFF1,
C                                            ITAU,IDTG)
C
C  PARAMETERS:
C    NAME    TYPE      USAGE     DESCRIPTION
C   -----    ----      -----     -----------
C   GRDI     REAL      OUT   
C   GRDJ     REAL      OUT
C   HEFF     REAL      OUT
C   AREA     REAL      OUT
C   FO       REAL      IN/OUT
C   HDIFF1   REAL      IN 
C   ITAU     INT       OUT
C   IDTG     INT       OUT
C
C  COMMON BLOCKS:
C
C      BLOCK     NAME     TYPE    USAGE       NOTES
C     ------    -----     ----    ------     ------------------------
C     /GROW/     YNEG     REAL    OUT
C     /GROW/    FHEFF     REAL    IN
C     /TEMP/    TICE      REAL    OUT
C     /TIMES/   RELAXS    REAL    OUT
C     /TIMES/   FORMS     REAL    OUT
C     /TIMES/   ADVCTS    REAL    OUT
C     /TIMES/   GRWTHS    REAL    OUT
C     /TIMES/   HEATS     REAL    OUT  
C     /TIMES/   MESHS     INT     OUT
C     /INITS/   INITS     INT     OUT
C
C  FILES:                          NONE
C
C  DATA BASES:                     NONE
C
C  NON-FILE INPUT/OUTPUT:          NONE
C
C  ERROR CONDITIONS:               NONE
C
C************************MAINTENANCE SECTION*****************************
C
C  MODULES CALLED:
C          NAME      DESCRIPTION
C          ----      --------------
C          ICEINTL    READS IN FNOC FORCING 
C          BUDGET     CALCULATED THE HEAT BUDGET BALANCE
C
C  LOCAL VARIABLES AND 
C           STRUCTURES:
C
C   NAME   TYPE    DESCRIPTION
C   ----   ----    ------------
C   GX     REAL    THE X COMPONENT OF THE GEOSTROPHIC WIND SPEED
C   GY     REAL    THE Y COMPONENT OF THE GEOSTROPHIC WIND SPEED
C   HMIXI  REAL    THE INVERSE MIXED LAYER DEPTH
C   KOPEN  INT     SWITCH TO TELL BUDGET WHETHER TO DO OPEN WATER
C                  OR ICE CALCULATIONS
C
C  METHOD:                          SUBROUTINE HEAT CALLS SUBROUTINE
C                                   ICEINTL TO READ IN THE NOGAPS FIELDS
C                                   AND CALCULATES FROM THEM THE VARIABLES
C                                   NEEDED FOR THE HEAT BUDGET.
C                                   UG = MAGNITUDE OF WIND
C                                   TAIR = AIR TEMP IN DEGREES K
C                                   QA = SPECIFIC HUMIDITY
C                                   FSH = INCOMING SHORT WAVE RADIATION
C                                   FLO = NET LONG WAVE RADIATION
C                                   SUBROUTINE HEAT CALLS BUDGET WHICH
C                                   CALCULATES THE CHANGES IN GROWTH RATES
C                                   FOR OPEN WATER AND ICE COVERED WATER.
C                                   INPUTS TO THE SUBROUTINE ARE THE I
C                                   AND J GRID POINT ARRAYS, GRDI AND GRDJ,
C                                   THE ICE THICKNESS, HEFF AND ICE   
C                                   CONCENTRATION, AREA, THE HOUR OF THE
C                                   FORECAST, ITAU, AND THE DATE TIME GROUP,
C                                   IDTG.  OUTPUTS ARE THE GROWTH RATE OF
C                                   ICE ON OPEN WATER, FO, AND THE NET
C                                   GROWTH OF THIN ICE, HDIFF1.  THE TOTAL
C                                   GROWTH OF ICE FHEFF, IS PASSED TO THE    
C                                   MAIN PROGRAM VIA COMMON BLOCK GROW.
C
C  INCLUDE FILES:                   NONE
C
C  COMPILER DEPENDENCIES:           Fortran 77 
C
C  COMPILE OPTIONS:                 -a stack  memory allocation
C                                   -o zeroinc optimization options
C                                   -d p      disables double precision
C                                   -e z      enables debugger
C                                   -m 4  message category - error message 
C
C  MAKEFILE:                        LOCATION?
C
C**********************END PROLOGUE****************************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX1=80,NY1=142,NX2=81,NY2=143) 
C 
C********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER,MS. 39529 
C 
C********************************************************************** 
C 
      DIMENSION TMIX(NX1,NY1),qa(nX2,NY2),  
     *          FLO(NX2,NY2), GRDI(NX2,NY2), GRDJ(NX2,NY2), 
     *          FO(NX1,NY1), 
     *          AREA(NX1,NY1,3),HEFF(NX1,NY1,3),IDTG(3),UG(NX1,NY1), 
     *          HICE(NX1,NY1), AR(NX1,NY1), HDIFF1(NX1,NY1) 
c      DIMENSION TMIX(NX1,NY1), TAIR(NX2,NY2), QA(NX2,NY2),  
c     *          GAIRX(NX2,NY2), 
c     *          GAIRY(NX2,NY2),ES(NX2,NY2),
      COMMON / GROW / YNEG(NX1,NY1),FHEFF(NX1,NY1) 
      COMMON / TEMP / TICE(NX1,NY1)
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - Again this is our common block for the forcing for this
C            special test case.
      COMMON / rfor / gairx(nx2,ny2),gairy(nx2,ny2),tair(nx2,ny2),
     *                ps(nx2,ny2),es(nx2,ny2),fsh(nx2,ny2),
     *                ps1(nx2,ny2),es1(nx2,ny2),evap(nx2,ny2)
C
C     All subsequent call to Iceintl have been replaced by this
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
c      COMMON / RAD / FSH(NX2,NY2) 
c      COMMON/PRESSUR/PS(NX2,NY2) 
      T1 = SECOND() 
C 
C 
      DO 10 I = 1,NX1 
         DO  9 J = 1,NY1 
         GX = (GAIRX(I,J)+GAIRX(I+1,J)+GAIRX(I,J+1)+GAIRX(I+1,J+1))*.25  
         GY = (GAIRY(I,J)+GAIRY(I+1,J)+GAIRY(I,J+1)+GAIRY(I+1,J+1))*.25  
         UG(I,J) = SQRT(GX**2 + GY**2)
 9       CONTINUE 
 10   CONTINUE 
C 
C     READ IN FIELD A07, SURFACE AIR TEMPERATURE  
C 
c      CALL ICEINTL(2,TAIR,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
C 
C 
C 
      DO 15 J = 1,NY1 
         DO 14 I = 1,NX1 
         TAIR(I,J) = TAIR(I,J) + 273.0
 14      CONTINUE 
 15   CONTINUE 
C 
C     READ IN A12 SURFACE VAPOR PRESSURE 
C 
c      CALL ICEINTL(3,ES,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
      DO 20 J = 1,NY1 
         DO 19 I = 1,NX1 
         QA(I,J) = (0.622 * ES(I,J)) / (PS(I,J) - ES(I,J))
 19      CONTINUE 
 20   CONTINUE 
C 
C     READ IN FIELDS A11, SHORT WAVE RADIATION, A16, SENSIBLE 
C     VAPOR PRESSURE AND A18, TOTAL HEAT FLUX.  RINV IS THE 
C     SCALE FACTOR TO CHANGE FIELDS INTO MKS UNITS. 
C 
c      CALL ICEINTL(4,FSH,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(5,PS,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
c      CALL ICEINTL(6,ES,GRDI,GRDJ,IDTG,NX2,NY2,ITAU) 
      RINV=1000./86. 
      DO 30 J = 1,NY1 
         DO 29 I = 1,NX1 
         PS1(I,J) = PS1(I,J) * RINV 
         ES1(I,J) = ES1(I,J) * RINV 
         FSH(I,J) = FSH(I,J) * RINV 
         FLO(I,J) = PS1(I,J) - ES1(I,J) + FSH(I,J) 
         FLO(I,J) = - FLO(I,J)
 29      CONTINUE 
 30   CONTINUE 
C 
C     HMIXI IS THE INVERSE MIXED LAYER DEPTH.  MIXED LAYER DEPTH IS 
C     SET EQUAL TO A CONSTANT VALUE.  SET THE MINIMAL AREAL EXTENT 
C     OF ICE TO .15.  HICE IS THE "MEAN" ICE THICKNESS OVER A GRID 
C     CELL [THIS ASSUMES THE AMOUNT OF THIN ICE IS SMALL].  AR IS 
C     THE AREAL EXTENT OF ICE AT THE PREVIOUS TIME STEP. 
C 
      HMIXI=1.0/30.0 
      DO 200 J = 1,NY1 
         DO 190 I = 1,NX1 
         AREA(I,J,2) = AMAX1(0.15,AREA(I,J,2))
 190     CONTINUE 
 200  CONTINUE 
      DO 101 J=1,NY1 
         DO 100 I=1,NX1 
         FHEFF(I,J)=0.0 
         HICE(I,J)=HEFF(I,J,2)/AREA(I,J,2) 
         AR(I,J)=AMIN1(AREA(I,J,2),HEFF(I,J,2)*1.0D+04)
 100     CONTINUE 
 101  CONTINUE 
C  NOW DETERMINE THE MIXED LAYER TEMPERATURE 
      DO 102 J=1,NY1 
         DO 104 I=1,NX1 
         TMIX(I,J)=72.076D+00*HMIXI*YNEG(I,J)+271.2D+00
 104     CONTINUE 
 102  CONTINUE 
C 
C     DO HEAT BUDGET CALCULATIONS. 
C     KOPEN IS USED TO DETERMINE WHETHER WE DO A BUDGET CALCULATION 
C     FOR OPEN WATER OR ICE COVERED WATER.  IF KOPEN <0, DO OPEN 
C     WATER.  IF KOPEN IS >0 DO ICE COVERED WATER.  BUDGET RETURNS 
C     FO, GROWTH RATE OF ICE ON OPEN WATER OR FHEFF, GROWTH RATE OF 
C     THICK ICE.  HDIFF1 IS THE NET GROWTH OF ICE ON OPEN WATER. 
C 
      KOPEN = -1 
      CALL BUDGET(HICE,FO,KOPEN,UG,TMIX,TAIR,QA,FLO) 
      KOPEN = 2 
      CALL BUDGET(HICE,FHEFF,KOPEN,UG,TICE,TAIR,QA,FLO) 
C 
C     FHEFF NOW BECOMES THE TOTAL GROWTH OF ICE 
C 
      DO 103 J=1,NY1 
         DO 106 I=1,NX1 
         FHEFF(I,J)=FHEFF(I,J)*AR(I,J)+(1.0-AR(I,J))*FO(I,J) 
         HDIFF1(I,J)=(1.0-AR(I,J))*FO(I,J)
 106     CONTINUE 
 103  CONTINUE 
      T2 = SECOND() 
      HEATS = HEATS + (T2 - T1) 
      RETURN 
      END 
      SUBROUTINE RNEGT(HEFF,YNEG,FH) 
C  
C**************************START PROLOGUE******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   RNEGT
C
C  DESCRIPTION:                   SUBROUTINE REMOVES NEGATIVE ICE FROM
C                                 THE MODEL
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL RNEGT(HEFF,YNEG,FH)
C
C  PARAMETER:
C    NAME   TYPE      USAGE      DESCRIPTION
C   -----   ----      -----     ------------------
C   HEFF    REAL      OUT  
C   YNEG    REAL      IN/OUT
C   FH      REAL      OUT
C
C  COMMON BLOCKS:                 NONE 
C
C  FILES:                         NONE
C
C  DATA BASES:                    NONE
C
C  ERROR CONDITIONS               NONE
C
C************************MAINTENANCE SECTION******************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
c           STRUCTURES:           NONE
C
C  METHOD:                        THIS SUBROUTINE REMOVES NEGATIVE ICE
C                                 FROM THE TWO LEVEL MODEL.  INPUTS TO
C                                 THE SUBROUITNE ARE THE NEGATIVE MELT    
C                                 VALUE, YNEG, THE ICE THICKNESS, HEFF,
C                                 AND FH, THE ADDITIONAL ICE TO BE MELTED
C                                 YNEG AND HEFF ARE UPDATED AND RETURNED
C                                 TO THE MAIN PROGRAM.
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         Fortran 77 
C
C  COMPILER OPTIONS:              -a stack   memory allocation
C                                 -o zeroinc optimization options
C                                 -d p       disables double precision
C                                 -e z       enables debugger
C                                 -m 4  message category - error message  
C
C  MAKEFILE:                      LOCATED?
C
C************************END PROLOGUE************************************ 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX1=80,NY1=142) 
C 
C********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C********************************************************************** 
C 
      DIMENSION HEFF(NX1,NY1,3),YNEG(NX1,NY1),FH(NX1,NY1) 
      DO 101 J=1,NY1 
         DO  91 I=1,NX1 
         YNEG(I,J)=YNEG(I,J)+FH(I,J)
 91      CONTINUE 
 101  CONTINUE 
C  NOW CORRECT THE ICE THICKNESS 
      DO 102 J=1,NY1 
      DO 92 I=1,NX1 
         HEFF(I,J,3)=HEFF(I,J,1)-YNEG(I,J) 
         HEFF(I,J,1)=AMAX1(0.0D+00,HEFF(I,J,3))
 92      CONTINUE 
 102  CONTINUE 
C  NOW CORRECT YNEG 
      DO 103 J=1,NY1 
         DO  93 I=1,NX1 
         YNEG(I,J)=HEFF(I,J,1)-HEFF(I,J,3)
 93      CONTINUE 
 103  CONTINUE 
      RETURN 
      END 
      SUBROUTINE BUDGET(HICE1,TSUM,KOPEN,UG,TSFC,TAIR, 
     *                  QA,FLO)
C
C**********************START PROLOGUE**********************************
C
C  SCCS IDENTIFICATION: N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   BUDGET
C
C  DESCRIPTION:                   SUBROUTINE BUDGET COMPUTES THE SURFACE
C                                 TEMPERATURE OF ICE WHICH BALANCES THE
C                                 SURFACE HEAT BUDGET.
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  COMPUTER OPERATING SYSTEM
C               DEPENDENCES:      UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL BUDGET(HICE1,TSUM,KOPEN,UG,TSFC,TAIR
C                                             QA,FLO)
C
C  PARAMETERS:
C     NAME       TYPE      USAGE         DESCRIPTION
C     ----       ----      -----         ------------------
C     HICE1      REAL      OUT
C     TSUM       REAL      IN/OUT
C     KOPEN       INT      OUT
C     UG         REAL      OUT
C     TSFC       REAL      IN/OUT
C     TAIR       REAL      IN/OUT
C       QA       REAL      OUT
C      FLO       REAL      OUT
C
C  COMMON BLOCKS:
C
C     BLOCK    NAME     TYPE     USAGE        NOTES 
C     -----    ----     ----     -----        -----
C     /RAD/    FSH      REAL      OUT
C     /GROW/   YNEG     REAL      OUT
C     /GROW/   FHEFF    REAL      OUT 
C     /OCEANS/ FW       REAL      OUT
C     /SNOW/   SNRT     REAL      OUT
C     /STEP/   DELTAT   REAL      OUT
C     /STEP/   DELTAX   REAL      OUT
C     /STEP/   DELTAY   REAL      OUT
C     /STEP/   DELTA1   REAL      OUT
C     /STEP/   DELTA    REAL      OUT
C
C  FILES:                         N/A
C
C  DATA BASES:                    N/A
C
C  NON-FILE INPUT/OUTPUT:         N/A
C
C  ERROR CONDITIONS:              N/A
C
C******************************MAILTENANCE SECTION************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:           
C
C      NAME      TYPE       DESCRIPTION
C      ----      ----       --------------
C      QO        REAL       THE INVERSE OF THE VOLUMETRIC HEAT OF FUSION
C                           OF ICE [M**3/J]
C      D1        REAL       THE BULK SENSIBLE HEAT TRANSFER COEFFICIENT
C                           [J/M**3 K]
C      D1W       REAL       THE BULK LATENT HEAT TRANSFER COEFFICIENT OVER
C                           WATER [J/M**3]
C      D1I       REAL       THE BULK LATENT HEAT TRANSFER COEFFICIENT OVER
C                           ICE [J/M**3]
C      D3        REAL       STEFAN-BOLTZMANN CONSTANT TIMES THE SURFACE
C                           EMMISSIVITY [W/M**2 K**4]
C      TB        REAL       271.2, THE FREEZING POINT OF SEA WATER [K]
C      ALB       REAL       ALBEDO [0.1 FOR OPEN WATER, 0.75 FOR ICE
C                           WITH A SURFACE TEMPERATURE BELOW FREEZING
C                           AND 0.66 FOR ICE WITH A SURFACE TEMPERATURE
C                           ABOVE FREEZING]
C      FW        REAL       THE OCEAN HEAT FLUX.  THIS ARRAY IS READ INTO
C                           THE PROGRAM IN ICEMDL FROM UNIT 4 AND PASSED
C                           TO SUBROUTINE BUDGET VIA COMMON BLOCK OCEANS
C      FICE      REAL       ICE GROW AT EACH OF THE 7 LEVELS
C      HSI       REAL       SNOW DEPTH AT EACH OF 7 LEVELS
C      POOL1     REAL       TEMPORARY ARRAY
C      POOL2     REAL       TEMPORARY ARRAY
C      SNOW      REAL       STORES MIDPOINT VALUE OF SNOW DEPTH IN 7 LEVEL
C                           LOOP
C      SSUM      REAL       MEAN OF SNOW DEPTH FROM ALL 7 LEVELS
C      THICK     REAL       STORES MIDPOINT VALUE OF ICE THICNESS
C      TMID      REAL       STORES MIDPOINT SURFACE TEMPERATURE
C      TMID2     REAL       SAVES MIDPOINT SURFACE TEMPERATURE OF NEXT TIME
C                           STEP
C      TSFC      REAL       SURFACE ICE TEMPERATURE COMPUTED AT EACH LEVEL
C      TSUM      REAL       MEAN ICE THICKNESS FROM ALL 7 LEVELS
C
C  METHOD:  SUBROUTINE BUDGET COMPUTES THE GROWTH RATES OF THICK AND THIN
C           ICE.  IT ALSO COMPUTES, BY ITERATION, THE SURFACE TEMPERATURE
C           OF ICE WHICH BALANCES THE SURFACE HEAT BUDGET.  THIS TEMPERATURE
C           DICTATES THE CONDUCTION OF HEAT THROUGH ICE AND HENCE THE GROWTH
C           RATES.
C           INPUT TO THIS SUBROUTINE ARE HICE1, THE AVERAGE ICE THICKNESS
C           OVER THE GRID CELL, KOPEN WHICH DETERMINES WHETHER THE CALCULATION
C           IS DONE FOR ICE COVERED WATER OR OPEN WATER, US IS THE WIND,
C           TSFC IS EITHER THE MIXED LAYER TEMPERATURE IN THE CASE OF OPEN
C           WATER OR THE ICE TEMPERATURE IN THE CASE OF ICE COVERED WATER,
C           TAIR IS THE AIR TEMPERATURE, QA IS THE SPECIFIC HUMIDITY AND FLO
C           IS THE LONG WAVE RADIATION.  OUTPUT IS THE GROWTH RATE, FICE.
C
C  INCLUDE FILES:            NONE
C
C  COMPILER DEPENDENCIES:    Fortran 77 
C
C  COMPILE OPTIONS:          -a stack   memory allocation
C                            -o zeroinc optimization option
C                            -d p       disables double precision
C                            -e z       enables debugger
C                            -m 4  message category - error message 
C
C  MAKEFILE:                 LOCATION?
C
C  RECORD OF CHANGES:     N/A
C
C**********************************END PROLOGUE*****************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX1=80,NY1=142,NX2=81,NY2=143) 
C 
C********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C********************************************************************** 
C 
      DIMENSION TICE(NX1,NY1),FICE(NX1,NY1), 
     *HICE(NX1,NY1),HICE1(NX1,NY1),ALB(NX1,NY1),  
     *B(NX1,NY1),A1(NX1,NY1),A2(NX1,NY1),A3(NX1,NY1), 
     *TSUM(NX1,NY1),TSFC(NX1,NY1), 
     *HSI(NX1,NY1),Q0(NX1,NY1),POOL1(NX1), 
     *POOL2(NX1),EM(NX1,NY1),SNOW(NX1,NY1),SSUM(NX1,NY1), 
     *TMID(NX1,NY1),THICK(NX1,NY1),TMID2(NX1,NY1), 
     *TAIR(NX2,NY2),QA(NX2,NY2),FLO(NX2,NY2),UG(NX1,NY1) 
      COMMON / RAD / FSH(NX2,NY2) 
      COMMON / GROW / YNEG(NX1,NY1),FHEFF(NX1,NY1) 
      COMMON / OCEANS / FW(NX1,NY1) 
      COMMON / SNOW / SNRT 
      COMMON / STEP / DELTAT,DELTAX,DELTAY,DELTA1,DELTA 
      DATA AKI / 2.1656 /, AKS / .31 /,C1 / 2.7798202E-6 /,
     *     C2 / -2.6913393E-3 /,C3 / 0.97920849 /,C4 /-158.63779 /,
     *     C5 / 9653.1925 /, QIS / .364 / 
      QS1 = 0.622/1013.0 
      TB=271.2 
      IMAX=10 
      D1=2.284 
      D1W = 5.6875D+03 
      D1I = 6.4475D+03 
      D3 = 5.5E-08  
      TMELT=273.16  
      DO 10 J=1,NY1 
         DO  9 I=1,NX1 
         HSI(I,J) = 0.0 
 9       CONTINUE
 10   CONTINUE 
      IF(KOPEN.GT.0) GO TO 51 
C     OPEN WATER CASE (HERE TICE IS THE TEMPERATURE OF THE MIXED LAYER 
C     FOR A DETAILED DISCUSSION OF THE EQUATIONS DEFINED IN LOOPS 
C     101 AND 105 SEE HIBLER (MONTHLY WEATHER REVIEW, 1980, 
C     PP. 1968-1969). 
      DO 101 J = 1,NY1 
         DO 100 I = 1,NX1 
         TICE(I,J) = TSFC(I,J) 
         Q0(I,J) = 1.0E-6/302. 
         A1(I,J)= 0.9 * FSH(I,J) + FLO(I,J) + D1 * UG(I,J) * TAIR(I,J) + 
     *         D1W * UG(I,J) * QA(I,J) 
         B(I,J) = QS1 * 6.11 * EXP(17.2694 *(TSFC(I,J) - TMELT) / 
     *                  (TSFC(I,J) - TMELT + 237.3)) 
         A2(I,J) = -D1 * UG(I,J) * TSFC(I,J) - D1W * UG(I,J) * B(I,J) 
         TSUM(I,J) = Q0(I,J) * (-FW(I,J) - A1(I,J) - A2(I,J) + SNRT 
     *               * 110.E6)
 100     CONTINUE 
 101  CONTINUE 
      GO TO 223 
 51   CONTINUE 
C      CASE FOR ICE COVER.  COMPUTES ICE TEMPERATURE AND GROWTH RATES. 
C      HICE LIMITS ICE THICKNESS TO NO LESS THEN 0.05M. 
      ITER = 0 
      DO 104 J = 1,NY1 
         DO 122 I = 1,NX1 
         EM(I,J) = 0.99 
         ALB(I,J) = 0.65 
         HICE(I,J) = 0.80 
         POOL1(I) = 0.97 
         Q0(I,J) = 1.0E-6/110.
 122     CONTINUE 
         DO 123 I=1,NX1 
         IF (HSI(I,J).EQ.0.0) EM(I,J) = POOL1(I) 
         IF (HSI(I,J).NE.0.0) ALB(I,J) = HICE(I,J)
 123     CONTINUE 
      DO 104 I=1,NX1 
      POOL1(I) = 1.0E-6/302.  
      HICE(I,J) = AMAX1(HICE1(I,J),0.05) 
      IF (HSI(I,J).EQ.0.0) Q0(I,J) = POOL1(I) 
 104  CONTINUE 
C     DETERMINE FIXED FORCING TERM IN HEAT BUDGET 
      DO 105 J = 1,NY1 
         DO 209 I = 1,NX1 
         A1(I,J)=(1.0 - ALB(I,J))*FSH(I,J)+FLO(I,J)+D1*UG(I,J)*TAIR(I,J) 
     *          + D1I * UG(I,J) * QA(I,J) 
 209     CONTINUE 
 105  CONTINUE 
C 
C     NOW BEGIN LOOP TO CALCULATE GROWTH AT EACH OF SEVEN LEVELS 
C 
      NLEVEL = 7 
      DO 201 J=1,NY1 
         DO 200 I=1,NX1 
         THICK(I,J) = HICE(I,J)  
         SNOW(I,J) = HSI(I,J) 
         TSUM(I,J) = 0.0 
         SSUM(I,J) = 0.0
         TMID(I,J) = TSFC(I,J)
 200     CONTINUE
 201  CONTINUE 
      DO 202 NLV=1,NLEVEL 
      ITER = 0 
C     COMPUTE SNOW AND ICE THICKNESS FOR LEVEL NLV 
         DO 203 J=1,NY1 
            DO 205 I=1,NX1 
            TSFC(I,J) = TMID(I,J) 
            HICE(I,J) = FLOAT(NLV)*2*THICK(I,J)/(NLEVEL+1) 
            TZ = TSFC(I,J)-273.16 
            TY = FLOAT(NLV)*2*SNOW(I,J)/(NLEVEL+1) 
            IF (TZ.LT.0.0) HSI(I,J) = TY 
            IF (TZ.GE.0.0) HSI(I,J) = SNOW(I,J)
 205        CONTINUE 
 203     CONTINUE 
C     NOW COMPUTE OTHER TERMS IN HEAT BUDGET. 
C     START OF ITERATION 
 60    CONTINUE 
         DO 106 J=1,NY1 
            DO 116 I=1,NX1 
            B(I,J)=QS1*6.11*EXP(21.8746*(TSFC(I,J)-TMELT) 
     *             /(TSFC(I,J)-TMELT+265.5))  
            A3(I,J)=D1I*UG(I,J)*B(I,J)*21.8746*265.5/((TSFC(I,J)  
     1         -TMELT+265.5)**2) 
            A2(I,J)=-D1*UG(I,J)*TSFC(I,J)-D1I*UG(I,J)*B(I,J) 
            TICE(I,J)=AKI*AKS/(AKS*HICE(I,J)+AKI*HSI(I,J)) 
            A3(I,J)=A3(I,J)+4.0*D3*EM(I,J)*TSFC(I,J)**3+TICE(I,J)+
     *              D1*UG(I,J) 
            B(I,J)=TICE(I,J)*(TB-TSFC(I,J))
 116        CONTINUE 
 106     CONTINUE 
         IF(ITER.GE.IMAX) GO TO 52 
C  SURFACE TEMPERATURE OF ICE 
         DO 109 J=1,NY1 
            DO 108 I=1,NX1 
            TSFC(I,J)=TSFC(I,J)+(A1(I,J)+A2(I,J)+B(I,J))/A3(I,J) 
 108        CONTINUE 
 109     CONTINUE 
         ITER=ITER+1 
         IF(ITER.LT.IMAX) GO TO 60 
         ITER=IMAX+1 
C        IF THE ITERATION YIELDS AN ABOVE FREEZING VALUE, THE SURFACE 
C        TEMPERATURE OF ICE IS SET AT THE FREEZING POINT (273.16K) 
         DO 107 J=1,NY1 
            DO 117 I=1,NX1 
            TSFC(I,J)=AMIN1(TSFC(I,J),TMELT) 
 117        CONTINUE 
 107  CONTINUE 
         GO TO 60 
 52      CONTINUE 
C 
         DO 115 J=1,NY1 
            DO 215 I=1,NX1 
            TICE(I,J) = TICE(I,J)*(HICE(I,J)/AKI*TSFC(I,J)+HSI(I,J)/
     *                  AKS*TB) 
            HSI(I,J) = AMAX1(HSI(I,J),.0001) 
 215        CONTINUE 
 115  CONTINUE
         DO 103 J=1,NY1 
            DO 118 I=1,NX1 
C     POOL1 = DEPTH OF MELTED SNOW 
            POOL1(I) = Q0(I,J)*(A1(I,J)+A2(I,J)+AKS/HSI(I,J)*(TICE(I,J)- 
     *                 TSFC(I,J))) 
C     POOL2 = EXCESS HEAT AFTER ALL SNOW IS MELTED; IT IS USED TO MELT I 
            POOL2(I) = (HSI(I,J)/DELTAT-POOL1(I))*QIS+1.E-6/302.*
     *                 (AKS/HSI(I,J) 
     *                 *(TICE(I,J)-TSFC(I,J))-AKI/HICE(I,J)*(TB-  
     *                 TICE(I,J))+.09*FSH(I,J)+.02*FLO(I,J)) 
C     IF SNOW EXISTED AND POOL2 IS LESS THAN 0 THEN MELT ICE 
C     IF NO SNOW EXISTED RECOMPUTE POOL1 USING ICE CONSTANTS (INSTEAD OF 
C     TO GET ICE MELT 
            TZ1 = HSI(I,J)-.0001 
            TY1 = -POOL1(I)-(AKI/HICE(I,J)*(TB-TICE(I,J))-AKS/HSI(I,J)* 
     *            (TICE(I,J)-TSFC(I,J)))*Q0(I,J) 
            TX1 = AMIN1(0.0,POOL2(I)) 
            IF (TZ1.EQ.0.0) FICE(I,J) = TY1 
            IF (TZ1.NE.0.0) FICE(I,J) = TX1 
            IF (TZ1.EQ.0.0) HSI(I,J) = 0.0 
118         CONTINUE 
            DO 103 I=1,NX1 
C     CORRECT SNOW DEPTH 
            V1 = HSI(I,J)-POOL1(I)*DELTAT 
            POOL1(I) = AMAX1(V1,0.0) 
            TZ2 = TSFC(I,J) - TMELT 
            TX2 = HSI(I,J) + DELTAT * SNRT 
            IF (TZ2.EQ.0.0) HSI(I,J) = POOL1(I) 
            IF (TZ2.NE.0.0) HSI(I,J) = TX2 
C     ADD ICE MELT AT ICE-OCEAN INTERFACE TO MELT AT TOP OF ICE 
            FICE(I,J) = 1.E-6/302.*(AKI/HICE(I,J)*(TB-TICE(I,J))-
     *                  FW(I,J)) + FICE(I,J)
 103     CONTINUE 
C     NOW ADD SNOW AND ICE DEPTH CHANGES TO 7-LEVEL SUM AND SAVE THE  
C     MIDPOINT TSFC FOR NEXT TIME STEP COMPUTATIONS. 
         DO 204 J=1,NY1 
            DO 206 I=1,NX1 
            TSUM(I,J) = FICE(I,J)/NLEVEL+TSUM(I,J) 
            SSUM(I,J) = HSI(I,J)/NLEVEL+SSUM(I,J) 
            NZ3 = NLV-(NLEVEL+1)/2  
            IF (NZ3.EQ.0) TMID2(I,J)=TSFC(I,J) 
            IF (NZ3.NE.0) TMID2(I,J)=TMID(I,J)
 206        CONTINUE 
 204     CONTINUE 
 202  CONTINUE 
         DO 222 J=1,NY1 
            DO 224 I=1,NX1 
            HSI(I,J) = SSUM(I,J) 
            TSFC(I,J) = TMID2(I,J) 
 224        CONTINUE 
 222     CONTINUE 
223   CONTINUE 
      RETURN 
      END 
      SUBROUTINE FORM(UICE,VICE,ETA,ZETA,AMASS,GAIRX,GAIRY,GWATX,GWATY, 
     *   DRAGS,DRAGA,OUT,HEFFM,DIV,HEFF,AREA)
C
C****************************START PROLOGUE********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   FORM
C
C  COPYRIGHT:                     N/A 
C  
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL FORM(UICE,VICE,ETA,ZETA,AMASS,GAIRX
C                                 GAIRY,GWATX,GWATY,DRAGS,DRAGA,OUT,HEFFM,
C                                 DIV,HEFF,AREA)
C  PARAMETERS:
C      NAME            TYPE        USAGE          DESCRIPTION
C      ----            ----        -----          -------------
C      UICE            REAL        OUT
C      VICE            REAL        OUT
C      ETA             REAL        OUT
C      ZETA            REAL        OUT
C      AMASS           REAL        IN 
C      GAIRX           REAL        OUT
C      GAIRY           REAL        OUT
C      GWATX           REAL        OUT
C      GWATY           REAL        OUT
C      DRAGS           REAL        IN
C      DRAGA           REAL        IN
C      OUT             REAL        OUT
C      HEFFM           REAL        OUT
C      DIV             REAL        OUT 
C      HEFF            REAL        OUT
C      AREA            REAL        OUT
C
C  COMMON BLOCKS:
C
C        BLOCK     NAME     TYPE     USAGE          NOTES
C        -----     ----     ----     -----         ----------------
C        /TIMES/   RELAXS   REAL     OUT
C        /TIMES/   FORMS    REAL     IN 
C        /TIMES/   ADVCTS   REAL     OUT
C        /TIMES/   GRWTHS   REAL     OUT
C        /TIMES/   HEATS    REAL     OUT
C        /TIMES/   MESHS    REAL     OUT
C        /TIMES/   INITS    INT      OUT
C        /FORCE/   FORCEX   REAL     IN/OUT
C        /FORCE/   FORCEY   REAL     IN/OUT
C        /PRESS1/  PRESS    REAL     IN
C        /STEP/    DELTAT   REAL     OUT
C        /STEP/    DELTAX   REAL     OUT
C        /STEP/    DELTAY   REAL     OUT
C        /STEP/    DELTA1   REAL     OUT
C        /STEP/    DELTA    REAL     OUT 
C        /WSTRES/  WSU      REAL     OUT
C        /WSTRES/  WSV      REAL     OUT
C
C  FILES:                          NONE
C
C  DATA BASES:                     NONE
C
C  NON-FILE INPUT/OUTPUT:          NONE
C
C  ERROR CONDITIONS:               NONE
C
C************************MAINTENANCE SECTION******************************
C
C  MODULES CALLED:
C     NAME            DESCRIPTION
C     ----            ---------------------
C     PLAST           CALCULATES STRAIN RATES DIVERGENCE AND
C                     NON-LINEAR VISCOSITIES
C 
C  LOCAL VARIABLES AND
C           STRUCTURES:
C      NAME     TYPE         DESCRIPTION
C      ----     ----         ------------------
C      FCOR     REAL         CORIOLIS PARAMETER
C      RHOAIR   REAL         DENSITY OF AIR
C      SINWIN   REAL         SIN OF THE TURNING ANGLE FOR THE WIND
C      COSWIN   REAL         COS OF THE TURNING ANGLE FOR THE WIND
C      SINWAT   REAL         SIN OF THE TURNING ANGLE FOR THE WATER
C      COSWAT   REAL         COS OF THE TURNING ANGLE FOR THE WATER
C      ECCEN    REAL         RATIO OF THE PRINCIPLE AXES OF THE PLASTIC
C                            YIELD ELLIPSE
C
C  METHOD:   THIS SUBROUTINE FORMS THE BASIC PARAMETERS FOR THE
C            SUBROUTINE RELAX.  INPUTS TO THE SUBROUTINE ARE THE
C            ICE DRIFT UICE AND VICE, THE OCEAN CURRENTS GWATX AND
C            GWATY, THE MARINE WINDS GAIRX AND GAIRY, THE OUTFLOW
C            (OUT) AND THE THERMODYNAMIC LAND-SEA MASK HEFFM, THE
C            ICE THICKNESS HEFF, THE ICE CONCENTRATION AREA.  OUTPUT
C            FROM THIS SUBROUTINE ARE THE AVERAGE ICE MASS IN A GRID
C            SQUARE AMASS, THE SYMMETRIC (DRAGS) AND ANTISYMMETRIC
C            (DRAGA) WATER DRAG. 
C
C  INCLUDE FILES:            NONE 
C
C  COMPILER DEPENDENCIES:    Fortran 77 
C
C  COMPILE OPTIONS:          -a stack   memory allocation
C                            -o zeroinc optimization options
C                            -d p       disables double precision
C                            -e z       enables debugger
C                            -m 4  messsage category - error message 
C
C  MAKEFILE:                 LOCATION?
C
C**************************END PROLOGUE**************************************
C  
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142,NX2=81,NY2=143) 
C 
C 
C********************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C********************************************************************** 
C 
      DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),ETA(NX1,NY1),ZETA(NX1,NY1), 
     *          AMASS(NX,NY),GAIRX(NX2,NY2),GAIRY(NX2,NY2),GWATX(NX,NY), 
     *          GWATY(NX,NY),DIV(NX1,NY1),DRAGS(NX1,NY1),DRAGA(NX1,NY1), 
     *          HEFFM(NX1,NY1),OUT(NX1,NY1),HEFF(NX1,NY1,3), 
     *          AREA(NX1,NY1,3),COR(NX,NY),DWATN(NX,NY) 
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      COMMON / FORCE / FORCEX(NX,NY),FORCEY(NX,NY)  
      COMMON / PRESS1 / PRESS(NX1,NY1) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA 
      COMMON/ WSTRES / WSU(NX2,NY2),WSV(NX2,NY2) 
C 
C     DEFINE THE CORIOLIS PARAMETER, DENSITY OF AIR, THE TURNING ANGLE 
C     FOR THE WINDS AND THE TURNING ANGLE FOR WATER AND THE SCALE 
C     FACTOR FOR PLAST CALCULATIONS OF VISCOSITY. 
C     TURNING ANGLE USED FOR WINDS IS 23 DEGREES  
C     TURNING ANGLE USED FOR WATER IS 25 DEGREES  
C 
      DATA     FCOR /1.46D-04/ 
     *,        RHOAIR/1.3/ 
     *,        SINWIN/.3907/  
     *,        COSWIN/.9205/  
     *,        SINWAT/0.4226/ 
     *,        COSWAT/0.9063/ 
     *,        ECCEN/2.0/ 
      T1 = SECOND() 
C 
C 
C     SET UP MASS PER UNIT AREA, AMASS, CORIOLIS TERM, COR, THE NON 
C     LINEAR WATER DRAG, DWATN AND THE ANTISYMMETRIC WATER DRAG + 
C     CORIOLIS TERM, DRAGA 
C 
C 
      DO 101 J = 1,NY 
         DO 100 I = 1,NX 
         AMASS(I,J) = 0.91D+03 * 0.25 * (HEFF(I,J,1) + HEFF(I+1,J,1) + 
     *     HEFF(I,J+1,1) + HEFF(I+1,J+1,1)) 
         COR(I,J) = AMASS(I,J) * FCOR 
         DWATN(I,J) = 5.5 * SQRT((UICE(I,J,1) - GWATX(I,J)) * * 2 + 
     *    (VICE(I,J,1) - GWATY(I,J)) * * 2) 
         DWATN(I,J)=AMAX1(DWATN(I,J),0.055D+00) 
         DRAGA(I,J) = DWATN(I,J) * SINWAT + COR(I,J)
 100     CONTINUE 
 101  CONTINUE 
C 
C 
C     SET UP THE NON LINEAR WIND DRAG, DAIRN 
C 
C 
      DO 105 J = 1,NY 
      JJ = J + 1 
         DO 104 I = 1,NX 
         II = I + 1 
C 
C     SET UP SYMMETRIC WATER DRAG 
         DRAGS(I,J) = DWATN(I,J) * COSWAT  
C 
C     NOW SET UP FORCING FIELD 
C 
C     FIRST DO WIND 
         FORCEX(I,J)=WSU(II,JJ)  
         FORCEY(I,J)=WSV(II,JJ)  
C
 104     CONTINUE 
 105  CONTINUE 
      DO 107 J = 1,NY 
         DO 108 I = 1,NX 
C   NOW ADD IN CURRENT FORCE  
C 
         FORCEX(I,J) = FORCEX(I,J) + DWATN(I,J) * (COSWAT * GWATX(I,J) - 
     *    SINWAT * GWATY( I,J))  
         FORCEY(I,J) = FORCEY(I,J) + DWATN(I,J) * (SINWAT * GWATX(I,J) 
     *    +COSWAT * GWATY( I,J ))
 108     CONTINUE 
 107  CONTINUE 
C 
C     NOW ADD IN TILT 
C 
      DO 109 J = 1,NY 
         DO 110 I = 1,NX 
         FORCEX(I,J) = FORCEX(I,J) - COR(I,J) * GWATY(I,J) 
         FORCEY(I,J) = FORCEY(I,J) + COR(I,J) * GWATX(I,J)
 110     CONTINUE 
 109  CONTINUE 
C 
C     STORE THE INPUT DATA 
C 
C     NOW SET UP ICE PRESSURE AND VISCOSITIES 
C     PLAST SETS UP VISCOSITIES AND CALCULATES DIVERGENCE 
C 
      DO 115 J=1,NY1 
         DO 116 I=1,NX1 
         PRESS(I,J)= 2.75D+4 * HEFF(I,J,1) * EXP(-20.0 * (1.0  
     *    - AREA(I,J,1)))
 116     CONTINUE 
 115  CONTINUE 
         CALL PLAST( UICE,VICE,PRESS,ETA,ZETA,ECCEN, 
     *     HEFFM,DIV)  
C     NOW SET VISCOSITIES AND PRESSURE EQUAL TO ZERO AT OUTFLOW 
C     POINTS 
      DO 106 J=1,NY1 
         DO 126 I=1,NX1 
         ETA(I,J)=ETA(I,J)*OUT(I,J) 
         ZETA(I,J)=ZETA(I,J)*OUT(I,J) 
         PRESS(I,J)=PRESS(I,J)*OUT(I,J)
 126     CONTINUE 
 106  CONTINUE 
C     NOW CALCULATE PRESSURE FORCE AND ADD TO EXTERNAL FORCE 
      DO 117 J = 1,NY 
         DO 118 I = 1,NX 
         FORCEX(I,J)=FORCEX(I,J)-(0.25/DELTAX)* 
     *   (PRESS(I+1,J)+PRESS(I+1,J+1)-PRESS(I,J)-PRESS(I,J+1)) 
         FORCEY(I,J)=FORCEY(I,J)-(0.25/DELTAY)* 
     *   (PRESS(I,J+1)+PRESS(I+1,J+1)-PRESS(I,J)-PRESS(I+1,J))
 118     CONTINUE 
 117  CONTINUE 
      T2 = SECOND() 
      FORMS = FORMS + (T2 - T1) 
      RETURN 
      END 
c      SUBROUTINE GEOWIND(GAIRX,GAIRY) 
      SUBROUTINE GEOWIND
C
C********************START PROLOGUE************************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   GEOWIND
C
C  DESCRIPTION:                   CALCULATES GEOSTROPHIC WINDS FROM
C                                 SURFACE PRESSURE FIELDS
C  
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    N/A
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL GEOWIND(GAIRX,GAIRY)
C
C  PARAMETERS:
C      NAME      TYPE      USAGE         DESCRIPTION
C      ----      ----      -----         -----------------
C      GAIRX     REAL      OUT
C      GAIRY     REAL      OUT
C     
C  COMMON BLOCKS:
C
C     BLOCK    NAME     TYPE     USAGE      NOTES
C     -----    ----     ----     -----      -----
C   /PRESSUR/  PS       REAL     OUT
C   /STEP/     DELTAT   REAL     OUT
C   /STEP/     DELTAX   REAL     OUT
C   /STEP/     DELTAY   REAL     OUT
C   /STEP/     DELTA1   REAL     OUT
C   /STEP/     DELTA    REAL     OUT
C
C  FILES:
C
C  DATA BASES:                     N/A 
C
C  NON-FILE INPUT/OUTPUT:          N/A 
C
C  ERROR CONDITIONS:               N/A
C
C************************MAINTENANCE SECTION*****************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
C            STRUCTURES:          NONE
C
C  METHOD:                        NOGAPS PRESSURE FIELDS ARE USED TO
C                                 CALCULATE GEOSTROPHIC WINDS
C
C  INCLUDE FILES:                 NONE
C
C  COMPILER DEPENDENCIES:         Fortran 77 
C  
C  COMPILE OPTIONS:               -a stack   memory allocation
C                                 -o zeroinc optimization options
C                                 -d p       disables double precision
C                                 -e z       enables debugger
C                                 -m 4  message category - error message 
C
C  MAKEFILE:                      LOCATED?
C
C**************************END PROLOGUE************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142,NX2=81, 
     *           NY2=143) 
      DIMENSION VFCOR(NX,NY)
c      DIMENSION GAIRX(NX2,NY2),GAIRY(NX2,NY2) 
c      COMMON / PRESSUR / PS(NX2,NY2)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - Don't forget that rfor is our common block for passing
C            the test data set of atmospheric forcing
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
      common /rfor/ gairx(nx2,ny2),gairy(nx2,ny2),tair(nx2,ny2),
     *              ps(nx2,ny2),es(nx2,ny2),fsh(nx2,ny2),
     *              ps1(nx2,ny2),es1(nx2,ny2),evap(nx2,ny2)
      COMMON / STEP / DELTAT,DELTAX,DELTAY,DELTA1,DELTA 
C 
C     DEFINE CORIOLIS TERM AND DENSITY OF AIR 
C 
      CALL VARICOR(VFCOR)
      RHO=1.3 
C 
C     CONVERSION FACTOR FROM MILLIBARS TO NT/(M*M) 
C 
      CMBNM=100. 
C 
C     DEFINE INNER GEOSTROPHIC WIND VALUES 
C 
      DO 10 J=1,NY  
         DO  9 I=1,NX  
         GAIRX(I+1,J+1)=-1.0/(VFCOR(I,J)*RHO)*CMBNM*((PS(I,J+1)+
     *                   PS(I+1,J+1))*.5- 
     *                  (PS(I,J)+PS(I+1,J))*.5)/DELTAY 
         GAIRY(I+1,J+1)=1.0/(VFCOR(I,J)*RHO)*CMBNM*((PS(I+1,J)+
     *                   PS(I+1,J+1))*.5- 
     *                  (PS(I,J)+PS(I,J+1))*.5)/DELTAX
 9       CONTINUE 
 10   CONTINUE 
C 
C     SET BOUNDARY VALUES EQUAL TO INSIDE VALUES  
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     had to change the do 20 and do 30 indexs to go from 1 - nx1
c     and 1 - ny1 to compile on the Cray.
c      DO 20 I=2,NX1 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 20 I=1,NX1 
      GAIRX(I,1)=GAIRX(I,2) 
      GAIRY(I,1)=GAIRY(I,2) 
      GAIRX(I,NY2)=GAIRX(I,NY1) 
      GAIRY(I,NY2)=GAIRY(I,NY1) 
 20   CONTINUE 
C 
c      DO 30 J=2,NY1 
      DO 30 J=1,NY1 
      GAIRX(1,J)=GAIRX(2,J) 
      GAIRY(1,J)=GAIRY(2,J) 
      GAIRX(NX2,J)=GAIRX(NX1,J) 
      GAIRY(NX2,J)=GAIRY(NX1,J) 
 30   CONTINUE 
C 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      Had to get rid of these
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      GAIRX(1,1)=GAIRX(2,2) 
c      GAIRY(1,1)=GAIRY(2,2) 
c      GAIRX(NX2,NY2)=GAIRX(NX1,NY1) 
c      GAIRY(NX2,NY2)=GAIRY(NX1,NY1) 
C 
      RETURN 
      END 
      SUBROUTINE PLAST(UICE,VICE,PRESS,ETA,ZETA,ECCEN,HEFFM, 
     *   DIV)
C
C************************START PROLOGUE************************************
C 
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   PLAST
C
C  DESCRIPTION:                   SUBROUTINE PLAST CALCULATES STRAIN
C                                 RATES, DEVIRGENCE AND VISCOSITIES
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL PLAST(UICE,VICE,PRESS,ETA,ZETA,
C                                            ECCEN,HEFFM,DIV)
C
C  PARAMETERS:
C     NAME     TYPE     USAGE       DESCRIPTION
C     ----     ----     -----       ------------------
C     UICE     REAL     OUT
C     VICE     REAL     OUT
C     PRESS    REAL     OUT
C     ETA      REAL     IN
C     ZETA     REAL     IN
C     ECCEN    REAL     OUT
C     HEFFM    REAL     OUT
C     DIV      REAL     IN
C
C  COMMON BLOCKS:
C
C     BLOCK    NAME    TYPE     USAGE      NOTES
C     -----    ----    ----     -----     --------------------
C     /STEP/   DELTAT  REAL     OUT
C     /STEP/   DELTAX  REAL     OUT
C     /STEP/   DELTAY  REAL     OUT
C     /STEP/   DELTA1  REAL     OUT
C     /STEP/   DELTA   REAL     OUT
C
C  FILES:                          NONE 
C 
C  DATA BASES:                     NONE 
C  
C  NON-FILE INPUT/OUTPUT:          NONE
C
C  ERROR CONDITIONS:               NONE 
C
C**********************MAINTENANCE SECTION*******************************
C
C  MODULES CALLED:                 NONE
C
C  LOCAL VARIABLES AND    
C           STRUCTURES:            
C
C     NAME        TYPE       DESCRIPTION
C     ----        ----       --------------
C     E11         REAL       THE XX STRAIN COMPONENT
C     E22         REAL       THE YY STRAIN COMPONENT
C     E12         REAL       THE XY STRAIN COMPONENT
C     ZMIN        REAL       THE MINIMUM ALLOWABLE BULK VISCOSITY VALUE
C     ZMAX        REAL       THE MAXIMUM ALLOWABLE BULK VISCOSITY VALUE:
C 
C  METHOD:   SUBROUTINE PLAST CALCULATES STRAIN RATES, DIVERGENCE
C            AND NONLINEAR VISCOSITIES BASED ON PLASTIC FLOW SPECIFIED
C            BY AN ELLIPTIC YIELD CURVE.
C            INPUTS TO THE SUBROUTINE ARE THE ICE DRIFT VELOCITY UICE
C            AND VICE, THE ICE STRENGTH, PRESS; THE RATIO OF THE PRINCIPAL
C            AXES OF THE PLASTIC YIELD ELLIPSE, ECCEN;  THERMODYNAMIC LAND
C            SEA MASK, HEFFM.  SUBROUTINE OUTPUTS ARE THE NONLINEAR BULK
C            VISCOSITY, ZETA.  THE NON LINEAR SHEAR VISCOSITY, ETA AND
C            THE DIVERGENCE, DIV. 
C
C  INCLUDE FILES:                  NONE
C
C  COMPILER DEPENDENCIES:          Fortran 77 
C
C  COMPILER OPTIONS:               -a stack  memory allocation
C                                  -o zeroinc optimization options
C                                  -d p      disable double precision
C                                  -e z      enable debugger
C                                  -m 4  message category - error message 
C
C  MAKEFILE:                       LOCATION?
C
C****************************END PROLOGUE**********************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C 
      DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),PRESS(NX1,NY1), 
     *          ZETA(NX1,NY1),DIV(NX1,NY1),ETA(NX1,NY1), 
     *          HEFFM(NX1,NY1),E11(NX1,NY1),E22(NX1,NY1),E12(NX1,NY1) 
      COMMON / STEP / DELTAT, DELTAX, DELTAY, DELTA1, DELTA 
C 
C     SET CONSTANTS AND ZERO OUT STRAIN RATE ARRAYS.  THEN CALCULATE  
C     STRAIN RATES.  E11 IS THE XX STRAIN COMPONENT, E22 IS THE YY 
C     STRAIN COMPONENT AND E12 IS THE XY STRAIN COMPONENT.  ZMIN AND  
C     ZMAX ARE THE MINIMUM AND MAXIMUM ALLOWABLE BULK VISCOSITY 
C     VALUES. 
C 
      ECM2=1.0/(ECCEN**2) 
       GMIN = 1.0D-20 
C 
      do 100 j=1,ny1
         do  90 i=1,nx1
         E11(i,j) = 0.0 
         E12(i,j) = 0.0 
         E22(i,j) = 0.0 
         zeta(i,j)= 0.0
 90      CONTINUE 
100   continue
      ZMIN = 4.0D + 08 
C***** 
      DO 101 J = 2,NY 
         DO 102 I = 2,NX 
         E11(I,J) = (0.5/DELTAX) * (UICE(I,J,1) + UICE(I,J-1,1) 
     *     -UICE(I-1,J,1)-UICE(I-1,J-1,1)) 
         E22(I,J) = (0.5/DELTAY) * (VICE(I,J,1) + VICE(I-1,J,1) 
     *     -VICE(I,J-1,1)-VICE(I-1,J-1,1)) 
         E12(I,J) = (0.25/DELTAY) * (UICE(I,J,1) + UICE(I-1,J,1) 
     *      -UICE(I,J-1,1)-UICE(I-1,J-1,1)) 
     *     +(0.25/DELTAX)*(VICE(I,J,1)+VICE(I,J-1,1) 
     *     -VICE(I-1,J,1)-VICE(I-1,J-1,1))
 102     CONTINUE 
 101  CONTINUE 
C  NOW EVALUATE VISCOSITIES 
C 
      DO 110 J = 2,NY 
         DO 109 I = 2,NX 
         DELT = (E11(I,J) * * 2 + E22(I,J) * * 2) * (1.0 + ECM2) + 4.0 * 
     *        ECM2 * E12(I,J) * * 2 + 2.0 * E11(I,J) * E22(I,J) * 
     *        (1.0 - ECM2) 
         DELT1=SQRT(DELT) 
         DELT1=AMAX1(GMIN,DELT1) 
         ZETA(I,J)=0.5*PRESS(I,J)/DELT1
 109     CONTINUE 
 110  CONTINUE 
C 
C     NOW SET THE UPPER AND LOWER BOUNDS OF THE NON LINEAR BULK 
C     VISCOSITY. 
C 
      DO 115 J = 1,NY1 
         DO 114 I = 1,NX1 
         ZMAX = (5.0D+12 / 2.0E+04) * PRESS(I,J) 
         ZETA(I,J) = AMIN1(ZMAX,ZETA(I,J)) 
         ZETA(I,J) = AMAX1(ZMIN,ZETA(I,J))
 114     CONTINUE 
 115  CONTINUE 
C 
C     DEFINE THE NON LINEAR SHEAR VISCOSITY, ZERO OUT THE LAND-SEA 
C     BOUNDARIES OF THE VARIOUS STRAIN RATES AND DEFINE DIVERGENCE. 
C     DEFINITIONS COMMENTED OUT WERE USED BY HIBLER TO DEFINE 
C     STRESSES USED ONLY FOR DIAGNOSTIC PURPOSES. 
C 
      DO 120 J = 1,NY1 
         DO 119 I = 1,NX1 
         ETA(I,J)=ECM2*ZETA(I,J) 
         E11(I,J) = E11(I,J) * HEFFM(I,J)  
         E22(I,J) = E22(I,J) * HEFFM(I,J)  
         E12(I,J) = E12(I,J) * HEFFM(I,J)  
C     SS11 = (ZETA(I,J) - ETA(I,J)) * (E11(I,J) + E22(I,J)) - 
C    *       PRESS(I,J) * 0.5 
C      STRESS(I,J,1)= 2.0*ETA(I,J)*E11(I,J) + SS11 
C      STRESS(I,J,2)= 2.0*ETA(I,J)*E22(I,J)+SS11  
C      STRESS(I,J,2)=2.0*ETA(I,J)*E12(I,J) 
C 
C     CALCULATE THE ICE DIVERGENCE AS THE SUM OF THE STRAIN RATES 
C 
         DIV(I,J) = E11(I,J) + E22(I,J)
 119     CONTINUE 
 120  CONTINUE 
      RETURN 
      END 
      SUBROUTINE ADJUST(HEFF,AREA,OUT,HEFFM)
C
C**********************START PROLOGUE********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   ADJUST
C
C  DESCRIPTION:                   SUBROUTINE ADJUST ESTIMATES ICE 
C                                 THICKNESSES FOR THE OUTFLOW GRID CELLS
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL ADJUST(HEFF,AREA,OUT,HEFFM)
C
C  PARAMETERS:
C    NAME      TYPE     USAGE     DESCRIPTION 
C    ----      ----     -----     --------------------
C    HEFF      REAL     IN/OUT   
C    AREA      REAL     IN/OUT 
C    OUT       REAL     OUT
C    HEFFM     REAL     OUT
C
C  COMMON BLOCKS:                 NONE 
C
C  FILES                          NONE
C
C  DATA BASES:                    NONE
C
C  NON-FILE INPUT/OUTPUT:         NONE
C
C  ERROR CONDITIONS:              NONE
C
C**********************MAINTENANCE SECTION********************************
C
C  MODULES CALLED:                MEAN
C      NAME                 DESCRIPTION
C      ----                 ----------------
C      MEAN                 SUBROUTINE TO AVERAGE SURROUNDING GRID
C                           POINT VALUES
C 
C  LOCAL VARIABLES AND
C          STRUCTURES:            NONE
C
C  METHOD:  SUBROUTINE ADJUST ESTIMATES REASONABLE ICE THICKNESSES
C           AND CONCENTRATIONS FOR THE OUTFLOW CELLS.  MEAN CALCULATES
C           THE AVERAGE THICKNESS IN SURROUNDING CELLS AND RETURNS
C           IT AS OUT2.  THAT AMOUNT IS THEN ADDED TO THE THICKNESS
C           AND TO THE CONCENTRATION IN THE OUTFLOW GRID CELLS.
C           INPUTS TO THIS SUBROUTINE ARE ICE THICKNESS (HEFF), ICE
C           CONCENTRATION (AREA), THE OUTFLOW LAND-SEA MASK (OUT)
C           AND THE THERMODYNAMIC LAND SEA MASK (HEFFM).
C           AFTER THE NEW VALUES ARE CALCULATED FOR HEFF AND AREA,
C           THEY ARE RETURNED CORRECTED TO THE MAIN PROGRAM.
C
C  INCLUDE FILES:                  NONE
C
C  COMPILER DEPENDENCIES:          Fortran 77 
C
C  COMPILE OPTIONS:                -a stack   memory allocation
C                                  -o zeroinc optimization options
C                                  -d p       disable double precision
C                                  -e z       enable debugger
C                                  -m 4  message category - error message 
C
C  MAKEFILE:                       LOCATION?
C
C**********************END PROLOGUE****************************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C 
      DIMENSION HEFF(NX1,NY1,3),AREA(NX1,NY1,3),HEFFM(NX1,NY1), 
     *          OUT(NX1,NY1),OUT2(NX1,NY1) 
      CALL MEAN(HEFF,OUT2,OUT) 
      DO 100 J = 2,NY 
         DO  90 I = 2,NX 
         HEFF(I,J,1) = HEFF(I,J,1) + (HEFFM(I,J) - OUT(I,J)) * OUT2(I,J)
 90      CONTINUE 
 100  CONTINUE 
      CALL MEAN(AREA,OUT2,OUT) 
      DO 110 J = 2,NY 
         DO 105 I = 2,NX 
         AREA(I,J,1) = AREA(I,J,1) + (HEFFM(I,J) - OUT(I,J)) * OUT2(I,J)
 105     CONTINUE 
 110  CONTINUE 
      RETURN 
      END 
      SUBROUTINE INOUT(HEFF,AREA,OUT,JEFFM,UICE,VICE,MM,BHEFF)
C
C**************************START PROLOGUE******************************
C
C  SCCS
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULES NAME:                  INOUT 
C
C  DESCRIPTION:                   TEST THE NORTHERN BOUNDARIES OF THE 
C                                 GREENLAND SEA TO DETERMINE IF THERE IS 
C                                 INFLOWIND OR OUTFLOWING ICE.
C 
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C              DEPENDENCIES:      UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL INOUT(HEFF,AREA,OUT,HEFFM,UICE,VICE,
C                                            MM,BHEFF)
C
C  PARAMETERS:
C     NAME        TYPE     USAGE      DESCRIPTION
C     ----        ----     -----      ----------------
C     HEFF        REAL     IN
C     AREA        REAL     IN
C     OUT         REAL     IN
C     HEFFM       REAL     IN
C     UICE        REAL     IN
C     VICE        REAL     IN
C     MM          INT      IN
C     BHEFF       REAL     IN
C
C  COMMON BLOCKS:                 N/A
C
C  FILES:                         N/A
C
C  DATA BASES:                    N/A 
C 
C  NON-FILE INPUT/OUTPUT          N/A
C
C  ERROR CONDITIONS:              NONE
C
C************************MAINTENANCE SECTION******************************
C
C  MODULES CALLED:                MEAN 
C
C  LOCAL VARIABLES AND
C           STRUCTURES:
C
C     NAME    TYPE                DESCRIPTION
C     ----    ----                --------------------
C
C  METHOD:        
C
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE INOUT 
C 
C     PURPOSE    SUBROUTINE INOUT TESTS THE NORTHERN BOUNDARIES
C                OF THE GREENLAND SEA TO DETERMINE IF THERE IS INFLOWING
C                OR OUTFLOWING ICE.  UICE IS CHECKED TO DETERMINE WHETHER
C                ICE IS FLOWING IN OR OUT.  UICE>0 DEFINES INFLOW.  IF 
C                ICE FLOWS IN, THICKNESS FROM THE PIPS MODEL (BHEFF) IS
C                USED AS THE ICE THICKNESS AT THE INFLOW GRID CELL.  IF
C                ICE FLOWS OUT, THE OLD HIBLER OUTFLOW BOUDARY CONDITION
C                IS USED (AS DEFINED IN ADJUST).  THE ICE THICKNESS AT
C                THOSE GRID CELLS.
C 
C     USAGE 
C                INPUT 
C
C  INCLUDE FILES:                    NONE
C
C  COMPILER DEPENDENCIES:            Fortran 77 
C
C  COMPILE OPTIONS:                  -a stack    memory allocation
C                                    -o zeroinc  optimization options
C                                    -d p        disables double precision
C                                    -e z        enables debugger
C                                    -m 4  message category - error message 
C
C  MAKEFILE:                         LOCATION?
C
C***********************END PROLOGUE************************************
C
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
      DIMENSION HEFF(NX1,NY1,3),AREA(NX1,NY1,3),OUT(NX1,NY1), 
     *HEFFM(NX1,NY1),UICE(NX,NY,3),VICE(NX,NY,3),OUT2(NX1,NY1), 
     *BHEFF(NX1,NY1),SHEFF(NX1,NY1) 
      CALL MEAN(HEFF,OUT2,OUT) 
      I=2 
      DO 100 J=110,NY 
      BIN=HEFFM(I,J)-OUT(I,J) 
      IF(BIN.NE.1.)GO TO 100  
      IF(UICE(I,J,1).GT.0.)GO TO 90 
      HEFF(I,J,1)=HEFF(I,J,1)+BIN*OUT2(I,J) 
      GO TO 100 
C 
C     SAVE THE OLD THICKNESS TO LATER DETERMINE CONCENTRATION 
C     CHANGE OLD THICKNESS (HEFF) INTO NEW THICKNESS (BHEFF) 
C     STORE THE OLD HEFF (AT ADJACENT TO BOUNDARY POINTS) IN SHEFF 
C 
   90 SHEFF(I,J) = HEFF(I+1,J,1) 
      IF(SHEFF(I,J).EQ.0.0) SHEFF(I,J)=HEFF(I,J+1,1) 
      IF(SHEFF(I,J).EQ.0.0) SHEFF(I,J)=HEFF(I+1,J+1,1) 
      HEFF(I,J,1) = BHEFF(I,J) 
C 
  100 CONTINUE 
 
C 
C 
C     IF OUTFLOW (UICE<0) CHANGE AREA AFTER HIBLER 
C 
      CALL MEAN(AREA,OUT2,OUT) 
C 
C     NEW BOUNDARY CONDITION CHANGE TO AREA 
C     IF OUTFLOW (UICE<0) CHANGE AREA AFTER HIBLER. 
C 
      I=2 
      DO 200 J=110,NY 
      BIN=HEFFM(I,J)-OUT(I,J) 
      IF(BIN.NE.1.) GO TO 200 
      IF(UICE(I,J,1).GT.0.)GO TO 201 
      AREA(I,J,1)=AREA(I,J,1)+BIN*OUT2(I,J) 
      GO TO 200 
C 
C     IF THE THICKNESS WAS LESS THEN .5 M BUT IS NOW >.5 M, 
C     WE INCREASE AREA 
C     IF THE THICKNESS BECOMES LESS THEN .5 AREA = .15 
C     IF THE THICKNESS WAS AND IS LESS THEN .5 DO NOTHING 
C     IF THE THICKNESS WAS AND IS GREATER THEN .5 DO NOTHING 
C 
  201 AREA(I,J,1)=AREA(I+1,J,1) 
      IF(AREA(I,J,1).EQ.0.0) AREA(I,J,1)=AREA(I,J+1,1) 
      IF(AREA(I,J,1).EQ.0.0) AREA(I,J,1)=AREA(I+1,J+1,1) 
      IF(SHEFF(I,J).GT.0.5 .AND. HEFF(I,J,1) .GT. 0.5)GO TO 200 
      IF(HEFF(I,J,1).EQ.0.) GO TO 202 
C 
      IF(SHEFF(I,J).GT.0.5 .AND. HEFF(I,J,1).LE.0.5) GO TO 202 
      IF(SHEFF(I,J).LE.0.5 .AND. HEFF(I,J,1).LE.0.5) GO TO 200 
C 
      AREA(I,J,1)=AREA(I,J,1)+(1.0-AREA(I,J,1))*(HEFF(I,J,1)- 
     +SHEFF(I,J))/0.5 
C 
      AREA(I,J,1)=AMIN1(1.0,AREA(I,J,1)) 
C 
      GO TO 200 
C 
  202 AREA(I,J,1)=0.15 
  200 CONTINUE 
      RETURN 
      END 
C 
      SUBROUTINE MESH(GRDI,GRDJ,N1,N2) 
C
C**************************START PROLOGUE******************************
C
C  SCCS
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULES NAME:                  MESH
C
C  DESCRIPTION:                   CALCULATES THE FNOC I,J GRID POINTS FOR
C                                 THE MODEL GRID AND CALCULATE THE MESH
C                                 SIZE.
C 
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C              DEPENDENCIES:      UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL MESH(GRDI,GRDJ,N1,N2)
C
C  PARAMETERS:
C     NAME        TYPE     USAGE      DESCRIPTION
C     ----        ----     -----      ----------------
C     GRDI        REAL     IN
C     GRDJ        REAL     IN
C     N1          INT      IN
C     N2          INT      IN
C
C  COMMON BLOCKS:
C
C     BLOCK     NAME     TYPE     USAGE      NOTES
C     -----     ----     ----     -----      --------------
C     /STEP/   DELTAT    REAL     OUT
C     /STEP/   DELTAX    REAL     OUT
C     /STEP/   DELTAY    REAL     OUT
C     /STEP/   DELTA1    REAL     OUT
C     /STEP/   DELTA     REAL     OUT
C     /TGRIDS/ TGRDI     REAL     IN
C     /TGRIDS/ TGRDJ     REAL     IN
C     /TIMES/  RELAXS    REAL     OUT
C     /TIMES/  FORMS     REAL     OUT
C     /TIMES/  ADVCTS    REAL     OUT
C     /TIMES/  GRWTHS    REAL     OUT
C     /TIMES/  HEATS     REAL     OUT
C     /TIMES/  MESHES    REAL     IN
C     /TIMES/  INIT      INT      OUT
C
C  FILES:                         N/A
C
C   NAME  UNIT   FILE TYPE     ATTRIBUTE    USAGE    DESCRIPTION
C   ----  ----   ---------     ---------    ------   ----------------
C          7     PERMANENT     SEQUENTIAL    IN      STARTING AND ENDING
C                                                    POINTS ON ICE MODEL
C                                                    GRID 
C  DATA BASES:                    N/A 
C 
C  NON-FILE INPUT/OUTPUT          N/A
C
C  ERROR CONDITIONS:              NONE
C
C************************MAINTENANCE SECTION******************************
C
C  MODULES CALLED:                NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:
C
C     NAME    TYPE                DESCRIPTION
C     ----    ----                --------------------
C     RI      REAL                FNOC Y GRID POINT LOCATION IN LOOP
C     RJ      REAL                FNOC X GRID POINT LOCATION IN LOOP
C
C  METHOD:        
C
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE MESH 
C 
C     PURPOSE    TO CALCULATE THE FNOC I,J GRID POINTS FOR THE MODEL  
C                GRID AND CALCULATE THE MESH SIZE. 
C 
C     USAGE 
C                INPUT 
C                   READS    IO,I1     DEFINING I GRID POINTS 
C                            JO,J1     DEFINING J GRID POINTS 
C 
C                  IO,I1,JO,J1 DEFINED AS FOLLOWS 
C 
C                    X                         X  
C                  (I,J)                    (I1,J1) 
C 
C 
C 
C                    X                         X  
C                 (IO,JO)                    (I,J) 
C 
C                            N1          NUMBER OF GRID POINTS IN 
C                                        THE X DIRECTION 
C                            N2          NUMBER OF GRID POINTS IN 
C                                        THE Y DIRECTION 
C 
C                            GRDI        I, GRID POINTS FOR THE 
C                                        DYNAMIC VARIABLES  
C                            GRDJ        J, GRID POINTS FOR THE 
C                                        DYNAMIC VARIABLES  
C                            TGRDI       I, GRID POINTS FOR THE 
C                                        THERMODYNAMIC VARIABLES 
C                            TGRDJ       J, GRID POINTS FOR THE 
C                                        THERMODYNAMIC VARIABLES 
C 
C                            DELTAX      GRID SIZE IN THE X AND Y 
C                            DELTAY      DIRECTIONS ARE EQUAL 
C 
C                            DELTA1      FRACTION OF FNOC GRID SIZE 
C                                        USED TO DEFINE DELTAY 
C                            DELTA       FRACTION OF FNOC GRID SIZE 
C                                        USED TO DEFINE DELTAX 
C 
C 
C             THE DEFINING GRID POINTS ARE READ FROM THE INPUT STREAM. 
C     THESE VALUES ARE USED TO CALCULATE THE I,J POINTS OF THE MODEL  
C     GRID.  THE MAP FACTOR IS CALCULATED AT EACH POINT AND THE AVERAGE 
C     MAP FACTOR IS USED TO CALCULATE THE MESH SIZE OF THE GRID. 
C
C  INCLUDE FILES:                    NONE
C
C  COMPILER DEPENDENCIES:            Fortran 77 
C
C  COMPILE OPTIONS:                  -a stack    memory allocation
C                                    -o zeroinc  optimization options
C                                    -d p        disables double precision
C                                    -e z        enables debugger
C                                    -m 4  message category - error message 
C
C  MAKEFILE:                         LOCATION?
C
C***********************END PROLOGUE************************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX2L=156,NY2L=156,NX2=81,NY2=143)
      REAL I0,I1,J0,J1 
c      REAL MESHS/0./ 
      DIMENSION GRDIB(NX2L,NY2L),GRDJB(NX2L,NY2L),TGRDIB(NX2L,NY2L),
     *          TGRDJB(NX2L,NY2L),GRDI(NX2,NY2),GRDJ(NX2,NY2)
      COMMON / STEP / DELTAT,DELTAX,DELTAY,DELTA1,DELTA 
      COMMON / TGRIDS / TGRDI(NX2,NY2),TGRDJ(NX2,NY2)
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      T1 = SECOND() 
C 
C     READ DEFINING POINTS 
C 
      READ (7,1010) I0,I1 
      READ (7,1010) J0,J1 
      READ(7,1012)N1,N2 
 1012 FORMAT(2I5) 
 1010 FORMAT(2F10.2) 
      PRINT 1001 
 1001 FORMAT(1H1) 
      PRINT 1000, I0,I1,J0,J1 
 1000 FORMAT(1X,'DEFINING GRID POINTS',/1X,'I0 AND I1    ',2F8.2, 
     *      /1X,'J0 AND J1    ',2F8.2)  
C 
C     SET GRID INCREMENTS 
C 
C     SET UP J POINTS (J MESH) 
C 
      DELTA1 = (J1 - J0) / FLOAT(N2-1)  
      HDELTA=DELTA1*.5 
      RI=J0 
      DO 15 J=1,N2  
         DO 13 I=1,N1  
         TGRDJB(I,J)=RI+HDELTA 
         GRDJB(I,J) = RI 
 13      CONTINUE 
      RI=RI+DELTA1  
 15   CONTINUE 
C 
C 
C 
C     SET UP I POINTS (I MESH) 
C 
      DELTA = (I1 - I0) / FLOAT(N1-1) 
      HDELTA=DELTA*.5 
      RJ = I0 
      DO 25 I = 1,N1 
         DO 23 J = 1,N2 
         TGRDIB(I,J)=RJ+HDELTA 
         GRDIB(I,J)=RJ  
 23      CONTINUE 
      RJ = RJ + DELTA 
 25   CONTINUE 
C 
C 
      DELTA = (DELTA + DELTA1) * 0.5 
C 
C     COMPUTE THE MESH SIZE 
C 
C 
C     COMPUTE EACH MAP FACTOR AND SUM THEM UP 
C 
      SUM = 0.0 
      DO 100 I = 1,N1 
         DO 110 J = 1,N2 
         RSQ = ((GRDIB(I,J) - 31.0) * * 2) + ((GRDJB(I,J) - 31.0) * * 2) 
         SINL = (973.752-RSQ) / (973.752+RSQ) 
         XMAP = (1 + SINL) / 1.8660254038  
         SUM = SUM + XMAP
 110     CONTINUE 
 100  CONTINUE 
C 
C     CALCULATE THE AVERAGE MAP FACTOR AND USE IT TO CALCULATE 
C     DELTAX = DELTAY BASED ON THE TRUE GRID SIZE AT 60 N (381 KM) 
C 
      XMAVG = SUM / (N1*N2) 
      DELTAX = DELTA * 381000.0 * XMAVG 
      DELTAX = AINT(DELTAX) 
      DELTAY = DELTAX 
      PRINT 1005,XMAVG,DELTAX 
 1005 FORMAT(1X,'AVERAGE MAP FACTOR IS  ',F10.3,  
     *      /1X,'THE MESH SIZE IS (IN METERS)   ',F10.3) 
C
      DO 200 J = 1,NY2
      DO 200 I = 1,NX2
       GRDI(I,J) = GRDIB(I+6,J+2)
       GRDJ(I,J) = GRDJB(I+6,j+2)
       TGRDI(I,J) = TGRDIB(I+6,J+2)
       TGRDJ(I,J) = TGRDJB(I+6,J+2)
200   CONTINUE 
C
       T2 = SECOND() 
      MESHS = MESHS+T2-T1 
      RETURN 
      END 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
C
C     FNOC --
c
c     This routine needs to be changed when the system library calls
c     are installed on the Cray.  For testing purposes, we will read
c     existing interolated PIPS gridded fields.
c
c      SUBROUTINE ICEINTL(NO,GAIR,GRDI,GRDJ,IDTG,N1,N2,ITAU) 
c      IMPLICIT REAL*8 (A-H,O-Z)
c      DIMENSION GRDI(N1,N2),GRDJ(N1,N2),GAIR(N1,N2),LABEL(2), 
c     * IRCD(8) 
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE ICEINTL IS USED TO READ IN THE TRANSFERRED ZRANDIO 
C     FILES A29, A30, A07, A01, A12, A11, A18, A16 USING CRANDIO 
C     TECHNIQUES.  THE DATA FIELDS TO BE READ ARE DEFINED ON THE 
C     63 X 63  POLAR STEREOGRAPHIC GRID AND A 24 WORD IDENT IS USED.  
C     ALL FIELDS ARE READ FROM ICEFILE.  AFTER FILES ARE READ 
C     SUBROUTINE INTERP IS CALLED TO INTERPOLATE THESE DATA TO THE 
C     MODEL GRID POINTS. 
C     SUBROUTINE INPUTS ARE NO, THE KKTH NUMBERED INPUT FIELD, THE 
C     I, J GRID POINT LOCATION, GRDI AND GRDJ, THE DATE TIME GROUP, 
C     IDTG, THE NUMBER OF HOURS INTO THE FORECAST, ITAU AND N1 AND 
C     N2, THE X AND Y MODEL GRID DIMENSIONS.  OUTPUT IS GAIR, THE 
C     INTERPOLATED FIELD. 
C 
C******************************************************************** 
C 
c      COMMON /TIMES/ RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
c      REAL INITS/0.0/ 
c      COMMON/BUFFER/IDENT(24),DATA(81,143),FILL(22697) 
c      DATA IFLAPS  /1H+/ 
c     *,     (IRCD(KK),KK=1,8) /3HA01,3HA07,3HA15, 
c     *          3HA11,3HA18,3HA16,3HA60,3HA61/ 
c      DATA IFILE/7HICEFILE/ 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      T1 = SECOND() 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
C     DEFINE AND PRINT THE IDENT 
C 
c      CALL SYSLBLC(IRCD(NO),IDTG,ITAU,IFLAPS,LABEL) 
c      PRINT 800,LABEL(1),LABEL(2) 
c 800  FORMAT(1H0,'ENTERING INITIAL - READING RECORD ',2A8)  
C 
C     READ IN THE FIELDS 
C 
c      CALL CHECKNC(IFILE,LABEL,11607,LEN,IS) 
c      IF(IS .EQ. 0) STOP 'CHECKNC NO DATA INITIAL' 
c      CALL CREADER(IFILE,LABEL,IDENT,11607,LEN,IS) 
c      DO 100 I=1,81
c      DO 100 J=1,143 
c      GAIR(I,J)=DATA(I,J) 
c100   CONTINUE 
C 
C     PRINT OUT THE PORTION OF THE 63 X 63 DATA SET WHICH IS 
C     USED IN THE INTERPOLATION 
c      JPRNT=1 
C 
C  SKIP PRINTING DIAGNOSTIC OUTPUT IF JPRNT=1 
c      IF(JPRNT .EQ. 0) THEN 
C 
c      DO 500 I = 25,40 
c      PRINT 75,(DATA(I,J),J=27,35) 
c500   CONTINUE 
c      ENDIF 
c75    FORMAT(1X,9E10.4) 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      T2 = SECOND() 
c       INITS = INITS + (t2-t1)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      RETURN 
c      END 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - this is our subroutine for reading in our test data
C            set of NOGAPS forcing.  It does not conform to the
C            required software standards since it will not remain
C            in the operational code. 
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      subroutine rforce
c
c      reads in all the forcing that the model needs
c
      parameter (nx2=81,ny2=143)
      implicit real*8 (a-h,o-z)
      real*4 gax,gay,ta,psa,psb,esa,esb,fsh1,ev,wu,wv
      common /rfor/ gairx(nx2,ny2),gairy(nx2,ny2),tair(nx2,ny2),
     *              ps(nx2,ny2),es(nx2,ny2),fsh(nx2,ny2),
     *              ps1(nx2,ny2),es1(nx2,ny2),evap(nx2,ny2)
      common / wstres / wsu(nx2,ny2),wsv(nx2,ny2)
      dimension gax(nx2,ny2),gay(nx2,ny2),ta(nx2,ny2),
     *          psa(nx2,ny2),esa(nx2,ny2),fsh1(nx2,ny2),
     *          psb(nx2,ny2),esb(nx2,ny2),ev(nx2,ny2),
     *          wu(nx2,ny2),wv(nx2,ny2)
c
      read(8,101)((ta(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((psa(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((esa(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((fsh1(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((psb(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((esb(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((ev(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((wu(i,j),i=1,nx2),j=1,ny2)
      read(8,101)((wv(i,j),i=1,nx2),j=1,ny2)
101   format(1x,6e13.6)
c
      do 2000 j=1,ny2
      do 2000 i=1,nx2
      gairx(i,j)=dble(gax(i,j))
      gairy(i,j)=dble(gay(i,j))
      tair(i,j)=dble(ta(i,j))
      ps(i,j)=dble(psa(i,j))
      es(i,j)=dble(esa(i,j))
      fsh(i,j)=dble(fsh1(i,j)) 
      ps1(i,j)=dble(psb(i,j)) 
      es1(i,j)=dble(esb(i,j))
      evap(i,j)=dble(ev(i,j))
      wsu(i,j)=dble(wu(i,j))
      wsv(i,j)=dble(wv(i,j))
2000  continue
c
      call geowind
c
      RETURN
      END 
c
      SUBROUTINE PRNT(ARRAY,I,J,K,M1,M2,N)
C
C**************************START PROLOGUE******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   PRNT
C
C  DESCRIPTION:                   PRINTS A MODEL ARRAY
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL PRNT(ARRAY,I,J,M1,M2,N)
C
C  PARAMETERS
C      NAME     TYPE    USAGE      DESCRIPTION
C      ----     ----    -----      --------------------
C      ARRAY    REAL    OUT
C      I        INT     OUT
C      J        INT     OUT
C      M1       INT     OUT
C      M2       INT     OUT
C      N        INT     OUT
C
C  COMMON BLOCKS:                  NONE
C
C  FILES:                          NONE
C
C  DATA BASES:                     NONE
C
C  NON-FILE INPUT/OUTPUT:          NONE
C
C  ERROR CONDITIONS:               NONE
C
C************************MAINTENANCE SECTION**************************
C
C  MODULES CALLED:                 NONE
C
C  LOCAL VARIABLES AND
C           STRUCTURES:            NONE
C
C  METHOD: 
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE PRNT 
C 
C     PURPOSE     TO PRINT A MODEL ARRAY 
C 
C     USAGE 
C                 ARRAY      THE ARRAY TO BE PRINTED 
C                 I, J       THE HORIZONTAL DIMENSIONS OF ARRAY 
C                 K          THE DIMENSION OF TIME LEVEL OF ARRAY 
C                 M1,M2      THE BEGINNING AND ENDING ROW NUMBER 
C                            TO BE PRINTED (NOT TO EXCEED 13) 
C                 N          THE NUMBER OF COLUMNS TO BE PRINTED 
C 
C******************************************************************** 
C
C  INCLUDE FILES:                  NONE 
C
C  COMPILER DEPENDENCIES:          Fortran 77 
C
C  COMPILE OPTIONS:                -a stack  memory allocation
C                                  -o zeroinc optimization options
C                                  -d p      disables double precision
C                                  -e z      enables debugger
C                                  -m 4  message category - error message
C 
C
C  MAKEFILE:                       LOCATION?
C
C****************************END PROLOGUE******************************
C 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION ARRAY(I,J,K)  
      PRINT 1005 
      PRINT 1007,M1,M2 
 1007 FORMAT(1X,'J FROM  ',I3,' TO ',I3) 
      DO 10 KK = 1,1 
         DO  9 II = 1,N 
         PRINT 1020,(ARRAY(II,JJ,KK),JJ=M1,M2)
 9       CONTINUE 
 10   CONTINUE 
      PRINT 1005 
 1005 FORMAT(///) 
 1020 FORMAT(1X,13E9.3) 
      RETURN 
      END 
      SUBROUTINE STATPRT(TSTOP)
C
C************************START PROLOGUE******************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   STATPRT
C
C  DESCIPTION:                    PRINTS OUT THE TIME STATISTICS
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRCITIONS:                  NONE
C
C  COMPUTER OPERATING SYSTEM
C               DEPENDENCIES:     UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL STATPRT((TSTOP)
C
C  PARAMETERS
C     NAME      TYPE     USAGE       DESCRIPTION
C     -----     ----     ------      ----------------
C     TSTOP     REAL     OUT
C
C  COMMON BLOCKS:
C
C     BLOCK     NAME     TYPE    USAGE     NOTES
C     -----     -----    ----    -----     -----
C     /TIMES/   RELAXS   REAL    OUT
C     /TIMES/   FORMS    REAL    OUT
C     /TIMES/   ADVCTS   REAL    OUT
C     /TIMES/   GRWTHS   REAL    OUT
C     /TIMES/   HEATS    REAL    OUT
C     /TIMES/   MESHS    REAL    OUT  
C     /TIMES/   INITS    INT     OUT  
C
C  FILES:                          NONE
C
C  DATA BASES:                     NONE
C
C  NON-FILE INPUT/OUTPUT:          NONE
C 
C  ERROR CONDITIONS:               NONE
C
C**************************MAINTENANCE**********************************
C
C  MODULES CALLED:                 NONE
C
C  LOCAL VARIABLES AND
C            STRUCTURES:           NONE
C
C  METHOD:      SUBROUTINE STATPRTPRINT OUT THE TIME STATISTICS
C               THAT IS HOW MUCH TIME THE MODEL HAS SPENT IN
C               THE SPECIFIED SUBROUTINE
C
C  INCLUDE FILES:                  NONE
C
C  COMPILER DEPENDENCIES:          Fortran 77 
C
C  COMPILE OPTIONS:                -a stack  memory allocation
C                                  -o zeroinc optimization option
C                                  -d p      disables double precision
C                                  -e z      enables debugger
C                                  -m 4 message category - error message 
C
C  MAKEFILE:                       LOCATION?
C
C************************END PROLOGUE************************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
      COMMON / TIMES / RELAXS,FORMS,ADVCTS,GRWTHS,HEATS,MESHS,INITS 
      REAL INITS,MESHS 
      PRINT 1001 
 1001 FORMAT('1','   T I M E   S T A T I S T I C S') 
      PRINT 1010,TSTOP 
 1010 FORMAT(1X,'TOTAL ICE MODEL TIME        ',F10.4) 
      PRINT 1020,RELAXS 
 1020 FORMAT(1X,'RELAXATION TIME . . . . . . ',F10.4) 
      PRINT 1030,FORMS 
 1030 FORMAT(1X,'FORMS AND PLASTIC TIME  . . ',F10.4) 
      PRINT 1040,ADVCTS 
 1040 FORMAT(1X,'ADVECTION TIME  . . . . . . ',F10.4) 
      PRINT 1050,GRWTHS 
 1050 FORMAT(1X,'GROWTH TIME . . . . . . . . ',F10.4) 
      PRINT 1060,HEATS 
 1060 FORMAT(1X,'HEAT BUDGET TIME  . . . . . ',F10.4) 
      PRINT 1070,MESHS 
 1070 FORMAT(1X,'INITIALIZING GRID AND OCEAN ',F10.4) 
      PRINT 1080,INITS 
 1080 FORMAT(1X,'READING CRANDIO TIME  . . . ',F10.4) 
      RETURN 
      END
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - This subroutine can be removed permanently..it is never
C            used anymore.
C 
c      SUBROUTINE INTRP  (Z,MM,NN,XX,YY,PHI) 
c      IMPLICIT REAL*8 (A-H,O-Z)
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322 STENNIS SPACE CENTER, MS. 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE INTRP IS USED TO INTERPOLATE DATA TO ANY FNOC GRID 
C     POINT OR FRACTION OF A GRID POINT.  Z(MM,NN) IS THE INPUT ARRAY 
C     TO BE INTERPOLATED, XX AND YY ARE THE X AND Y LOCATIONS IN FNOC 
C     GRID POINT VALUES ( OR FRACTIONS THEREOF), PHI IS THE RETURNED  
C     INTERPOLATED VALUE 
C 
C******************************************************************** 
C 
c      DIMENSION Z(1) 
C 
C SET INITIAL VALUES 
C 
c      M = MM 
c      N = NN 
c      I = XX + 1.0  
c      J = YY + 1.0  
c      X = XX + 1.0  
c      Y = YY + 1.0  
c      IF(I .LT. 1) GO TO 99 
c      IF(I .GT. M) GO TO 99 
c      IF(J .LT. 1) GO TO 99 
c      IF(J .GT. N) GO TO 99 
c      XI = X - I 
c      YJ = Y - J 
c      L=(J-1)*M+I 
c      P4=Z(L) 
C 
C IF XI=0 OR YJ=0 USE SPECIAL FORMULA 
c      IF (XI.GT.0.0) GO TO 50 
c      IF(YJ) 52,53,50 
c 50   IF (YJ.LE.0.0) GO TO 51 
C 
C INITIALIZE SUBSET VALUES 
c      P5 = Z(L + 1) 
c      P8 = Z(L + M) 
c      P9 = Z(L + 1 + M) 
c      XI2 = XI * XI 
c      XI3 = XI2 * XI 
c      YJ2 = YJ * YJ 
c      YJ3 = YJ2 * YJ 
C 
C COMPUTE OUTER-POINT VALUES OF SUBSET  
c      IF (I.NE.1) GO TO 101 
c      P3 = P4 + P4 - P5 
c      P7 = P8 + P8 - P9 
c      GO TO 102 
c 101  P3 = Z(L - 1) 
c      P7 = Z(L - 1 + M) 
c 102  IF (I.NE.(M-1)) GO TO 104 
c      P6 = P5 + P5 - P4 
c      P10 = P9 + P9 - P8 
c      GO TO 106 
c 104  P6 = Z(L + 2) 
c      P10 = Z(L + 2 + M) 
c 106  IF (J.NE.1) GO TO 107 
c      P1 = P4 + P4 - P8 
c      P2 = P5 + P5 - P9 
c      GO TO 108 
c 107  P1 = Z(L-M) 
c      P2 = Z(L +1 - M) 
c 108  IF (J.NE.(N-1)) GO TO 110 
c      P11 = P8 + P8 - P4 
c      P12 = P9 + P9 - P5 
c      GO TO 121 
c 110  P11 = Z(L + M + M) 
c      P12 = Z(L + 1 + M + M)  
C 
C SIMPLIFIED FORMULA: 
c 121  GI2=XI2+XI2+XI2-XI3-XI3 
c      GJ3 = YJ3 - YJ2 
c      GJ4 = YJ - YJ2 + GJ3 
c      GJ2 = YJ2 - GJ3 - GJ3 
c      GJ1 = 1.0 - GJ2 
c      PHI =(P4*GJ1+P8*GJ2+((P11-P4)*GJ3+(P8-P1)*GJ4)*0.5)*(1.-GI2) 
c     +    +(P5*GJ1+P9*GJ2+((P12-P5)*GJ3+(P9-P2)*GJ4)*0.5)*GI2 
c     +    +0.5*( ((P6-P4)*GJ1+(P10-P8)*GJ2)*(XI3-XI2) 
c     +          +((P5-P3)*GJ1+(P9- P7)*GJ2)*(XI-XI2-XI2+XI3) ) 
c      RETURN 
C 
C 
C    SPECIAL CASES: 
C 
C SPECIAL CASE 1: YJ=0 
c 51   P5=Z(L+1) 
c      IF (I.NE.1) GO TO 61 
c      P3 = P4 + P4 - P5 
c      GO TO 62 
c 61   P3=Z(L-1) 
c 62   IF (I.NE.(M-1)) GO TO 63 
c      P6 = P5 + P5 - P4 
c      GO TO 64 
c 63   P6 = Z(L + 2) 
c      GO TO 64 
C 
C SPECIAL CASE 2: XI=0 
c  52  P5 = Z(L + M) 
c      XI = YJ 
c      IF (J.NE.1) GO TO 81 
c      P3 = P4 +P4 - P5 
c      GO TO 82 
c 81   P3=Z(L-M) 
c 82   IF (J.NE.(N-1)) GO TO 83 
c      P6 = P5 + P5 - P4 
c      GO TO 64 
c 83   P6 =Z(L+M+M)  
C 
C SPECIAL FORMULA:  
c 64   XI2 = XI * XI 
c      XI3 = XI2 * XI 
c      GI3=XI3-XI2 
c      GI2=XI2-GI3-GI3 
c      GI4=XI-XI2+GI3 
c      PHI=(1.-GI2)*P4+GI2*P5+(GI3*(P6-P4)+GI4*(P5-P3))*0.5  
c      RETURN 
C 
C 
C SPECIAL CASE 3: XI=YJ=0 
c 53   PHI=P4 
c      RETURN 
C 
C 
C POINT Z(X,Y) OUT OF BOUNDS: 
c 99   PHI=MASK(59)  
c      RETURN 
C 
c      END
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     FNOC - you will have to rewrite subroutine savdata to conform
C            to the new data based standards
C 
c      SUBROUTINE SAVDATA(UICE,VICE,UICEC,VICEC,HEFF,AREA,YNEG,TICE, 
c     &IDTG,NPOCDTG) 
c      IMPLICIT REAL*8 (A-H,O-Z)
c      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C******************************************************************** 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS 39529 
C 
C******************************************************************** 
C 
C     SUBROUTINE SAVDATA WRITES THE MODEL RESTART FIELDS OUT IN 
C     CRANDIO FORMAT TO MASFNOC.  ALL PARAMETERS IN THE CALL 
C     STATEMENT ARE INPUT PARAMETERS OF ICE DRIFT VELOCITIES, UICE, 
C     VICE, UICEC AND VICEC, ICE THICKNESS, HEFF, ICE COMPACTNESS 
C     AREA, NEGATIVE ICE TO BE MELTED, YNEG AND THE ICE TEMPERATURE 
C     TICE.  ALL FIELDS ARE CONTRACTED WHEN WRITTEN OUT.  FTU CARDS 
C     FOR EACH FIELD ARE WRITTEN OUT TO UNIT 10.  
C 
C******************************************************************** 
C 
C 
c      DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),UICEC(NX,NY), 
c     *          VICEC(NX,NY),HEFF(NX1,NY1,3),AREA(NX1,NY1,3), 
c     *          YNEG(NX1,NY1),TICE(NX1,NY1) 
C 
C 
c      DIMENSION NAM(8),M(8),N(8),LBL(2) 
c      DIMENSION IW5(8),ITITLE(8) 
c      DIMENSION ISCALE(8) 
c      COMMON/BUFFER/IDOUT(24),FLDOUT(34280) 
c      DATA IPROJ/ 1H0 /, IHEM/ 1H0 /, IGRD/ 2H 2 / 
c      DATA ISCALE/ 4*45, 40, 46, 2*38 / 
c      DATA M/4*79,4*80/ 
c      DATA N/2*423,2*141,2*426,2*142/ 
c      DATA NAM/3HP02,3HP03,3HP07,3HP08,3HP00,3HP01,3HP09,3HP10/ 
c      DATA IW1,IW2,IW3,IW4/8HPIPS RST,8HRT-NPOC ,8H        , 
c     *                     8HSIZE    /  
c      DATA IW5/8H79,141,3 ,8H79,141,3 ,8H79,141   ,8H79,141   , 
c     *         8H80,142,3 ,8H80,142,3 ,8H80,142   ,8H80,142   / 
c      DATA ITITLE/8HUICE VEL,8HVICE VEL,8HUICE TPO,8HVICE TPO, 
c     *            8HICE THIK,8HICE CONC,8HICE MELT,8HICE TEMP/ 
C 
C 
C      CRANDIO FILE HAS BEEN OPENED IN THE MAIN PROGRAM 
C 
C 
c      IFILOUT=7HMASFNOC 
C 
C      DEFINE SOME REAL VARIABLES 
C 
c      ITAU=24 
c      IFLP=1HQ 
c      CI=0.0 
c      CO=0.0 
c      AD=0.0 
c      RMP=0.0 
c      DELI=36.37 
c      DELJ=28.77 
c      ROT=0. 
c      SCAL=.052 
C 
C     START MAIN LOOP 
C 
c      DO 200 K=1,8  
c      LEN1=M(K)*N(K) 
c      LENGTH=LEN1+24 
c      LEN2=LEN1/2+25 
C 
C     NOTE:  OUTPUT ARRAY IS 1/2 OF THE ORIGINAL ARRAY LENGTH 
C            + 25 WORDS 
C     PLACE THE VARIABLES READ IN INTO AN ARRAY DIMENSIONED (1,LEN) 
C     SO THAT IT WORKS CORRECTLY IN THE BUFFER COMMON BLOCK 
C 
c      IF(K.EQ.1)FLDOUT(1;LEN1)=UICE(1,1,1;LEN1) 
c      IF(K.EQ.2)FLDOUT(1;LEN1)=VICE(1,1,1;LEN1) 
c      IF(K.EQ.3)FLDOUT(1;LEN1)=UICEC(1,1;LEN1) 
c      IF(K.EQ.4)FLDOUT(1;LEN1)=VICEC(1,1;LEN1) 
c      IF(K.EQ.5)FLDOUT(1;LEN1)=HEFF(1,1,1;LEN1) 
c      IF(K.EQ.6)FLDOUT(1;LEN1)=AREA(1,1,1;LEN1) 
c      IF(K.EQ.7)FLDOUT(1;LEN1)=YNEG(1,1;LEN1) 
c      IF(K.EQ.8)FLDOUT(1;LEN1)=TICE(1,1;LEN1) 
C 
C     CREATE THE LABEL 
C 
c      CALL SYSLBLC(NAM(K),IDTG,ITAU,IFLP,LBL) 
C 
C      CREATE 24 WORD IDENT 
C 
c      DO 10 LL=1,24 
c   10 IDOUT(LL)=8H  
c      IDOUT(1)=LBL(1) 
c      IDOUT(2)=LBL(2) 
C 
c      ENCODE(8,36,IDOUT(3))LENGTH 
c   36 FORMAT(I8) 
c      ENCODE(8,37,IDOUT(7))M(K),N(K) 
c   37 FORMAT(2I4) 
c      IFIX = 48 - ISCALE(K) 
c      IUNIT = 2H00  
c      IGRD=2H 2 
c      ENCODE(8,38,IDOUT(6)) IPROJ,IHEM,IGRD,IFIX,IUNIT 
c 38   FORMAT(2A1,A2,I2,A2) 
C 
C 
c      ENCODE(8,39,IDOUT(8))DELI 
c   39 FORMAT(F8.3)  
c      ENCODE(8,39,IDOUT(9))DELJ 
c      ENCODE(8,39,IDOUT(10))ROT 
c      ENCODE(8,39,IDOUT(11))SCAL 
c      ENCODE(8,41,IDOUT(15)) NPOCDTG 
c 41   FORMAT(3HDAY,I5) 
C 
C 
c      IDOUT(12)=ITITLE(K) 
c      IDOUT(13)=IW1 
c      IDOUT(14)=IW2 
c      IDOUT(16)=IW4 
c      IDOUT(17)=IW5(K) 
C 
c      ENCODE(8,40,IDOUT(18))CI 
c  40  FORMAT(F8.2)  
c      ENCODE(8,40,IDOUT(19))CO 
c      ENCODE(8,40,IDOUT(20))AD 
c      ENCODE(8,40,IDOUT(21))RMP 
C 
C 
c      CALL ID203(IDOUT(1),999,ISTAT) 
C 
c      PRINT 11,(LL,IDOUT(LL),LL=1,24) 
c 11   FORMAT('    24 WORD IDENT : ',//, 
c     & 4(6(3X,'(',I2,') ',1X,A8)/)) 
C 
C      WRITE CRANDIO FILE 
C 
C     PACK THE FIELD 
C 
c      CALL CONTRACT(IDOUT(1),LENGTH) 
C 
C     WRITE THE FIELD 
C 
c      CALL CWRITER(IFILOUT,LBL,IDOUT(1),LEN2,ISTATUS) 
C 
c      IF(ISTATUS.EQ.0) GO TO 210 
c      PRINT 90,ISTATUS 
c 90   FORMAT(' ',' CWRITER STATUS = ',I3) 
C 
c 210  CONTINUE 
C 
C     WRITE UP XFRCIO FTU CARDS 
C 
c      IPAK = 31 
c      WRITE(10,8000)LBL,LEN2,IPAK,ISCALE(K) 
c 8000 FORMAT('1MASFNOC  OPSPOOL  2 ',2A8,26X,I7,/, 
c     +'2ICEFILE         ',19X,I2,3X,I2) 
C 
C 
c 200  CONTINUE 
C 
c      RETURN 
c      END 
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c    FNOC - This subroutine was added to write out data for our testing 
c    purposes.  Therefore this subroutine has not been changed to the
c    FNOC software standards  
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine savdata2(uice,vice,uicec,vicec,heff,area,yneg,tice)
      implicit real*8 (a-h,o-z)
      parameter (nx=79,ny=141,nx1=80,ny1=142) 
      dimension uice(nx,ny,3),vice(nx,ny,3),uicec(nx,ny),
     *          vicec(nx,ny),heff(nx1,ny1,3),area(nx1,ny1,3),
     *          yneg(nx1,ny1),tice(nx1,ny1)
c
      write(1)uice
      write(1)vice
      write(1)uicec
      write(1)vicec
      write(1)heff
      write(1)area
      write(1)yneg
      write(1)tice
c
      return
      end
c
      subroutine daynum(idtg,myr,mm,md,mhr)
C
C     THIS SUBROUTINE CREATES THE NECESSARY DATE-TIME-GROUP INFORMATION
C     SIMILAR TO THE FNOC 205 ROUTINE DDTG.  THIS SUBROUTINE IS NOT
C     WRITTEN ACCODING TO FNOC SOFTWARE STANDARDS SINCE IT WILL PROBABLY
C     BE REPLACED BEFORE GOING OPERATIONAL
C 
c     computes the year,month,day and hour from dtg
c
      myr=idtg/1000000
      mm=(idtg-(myr*1000000))/10000
      idtg1=(idtg/10000)*10000
      md=(idtg-idtg1)/100
      idtg2=(idtg/100)*100
      mhr=idtg-idtg2
c
      return
      end
c
      subroutine varicor(ffcor)
      implicit real*8 (a-h,o-z)
      parameter (nx=79,ny=141)
      dimension ffcor(nx,ny)
      data fcor/1.46D-04/
      data az2/973.7512/
      data dxy/.0516129/ 
      do 10 j=1,ny
      do 10 i=1,nx
      x = float(i-1) * dxy + 2.361289
      y = float(j-1) * dxy - 6.8967742
      r2 = x**2+y**2
      ff = (az2-r2)/(az2+r2) 
      ffcor(i,j) = fcor * ff
10    continue
      return
      end
c
      subroutine qmax(u,v,s11,s21)
c
      parameter (nx=79,ny=141,nx1=80,ny1=142)
      parameter (m4x=nx1/2,m4y=ny1/2,m4xy=m4x*m4y)
c
      dimension u(m4xy),v(m4xy)
c
      do 10 n=1,m4xy
      u(n)=abs(u(n))
      v(n)=abs(v(n))
10    continue
c
      s11=u(1)
      s21=v(1)
      do 15 n=1,m4xy
      s11=amax1(s11,u(n))
      s21=amax1(s21,v(n))
15    continue
c
      return
      end
c
      SUBROUTINE RESTRT(UICE,VICE,UICEC,VICEC,HEFF,AREA,YNEG, 
     &TICE,NPOCDTG)
c
C************************START PROLOGUE**********************************
C
C  SCCS IDENTIFICATION:  N/A
C
C  CONFIGURATION IDENTIFICATION:  N/A
C
C  MODULE NAME:                   RESTRT
C
C  DESCRIPTION:                   READS IN THE UNFORMATTED RESTART DATA
C
C  COPYRIGHT:                     N/A
C
C  CONTRACT NUMBER AND TITLE:     N/A
C
C  REFERENCES:                    NONE
C
C  CLASSIFICATION:                UNCLASSIFIED
C
C  RESTRICTIONS:                  NONE
C
C  COMPUTER/OPERATING SYSTEM
C              DEPENDENCIES:      UNIX
C
C  LIBRARIES OF RESIDENCE:        N/A
C
C  USAGE:                         CALL RESTRT(UICE,VICE,UICEC,VICEC,HEFF
C                                 AREA,YNEG,TICE,NPOCDTG)
C  PARAMETERS:
C     NAME     TYPE     USAGE       DESCRIPTION
C     ----     ----     -----       --------------
C     UICE     REAL     OUT
C     VICE     REAL     OUT
C     UICEC    REAL     OUT
C     VICEC    REAL     OUT
C     HEFF     REAL     OUT
C     AREA     REAL     OUT
C     YNEG     REAL     OUT
C     TICE     REAL     OUT
C     NPOCDTG  INT      OUT
C
C  COMMON BLOCKS:                  NONE
C
C  FILES:
C    NAME   UNIT   FILE TYPE    ATTRIBUTE   USAGE    DESCRIPTION
C    ----   ----   ---------    ---------   -----    -----------
C            3     PERMANENT    SEQUENTIAL  OUT
C
C  DATA BASES:                     NONE 
C
C  NON-FILE INPUT/OUTPUT:          NONE
C
C  ERROR CONDITIONS:               NONE
C
C****************************MAINTENANCE SECTIN************************
C
C  MODULES CALLED:                 NONE
C
C  LOCAL VARIABLES AND
C           STRICTURES:            NONE
C
C  METHOD:    SUBROUTINE RESTRT READS IN THE UNFORMATTED RESTART
C             DATA PASSED FROM THE EXTERNAL PROGRAM UPDATIF.  ALL
C             CALLING PARAMETERS ARE OUTPUT FIELDS, THE ICE DRIFT
C             (UICE, VICE, UICEC AND VICEC), THE ICE THICKNESS,
C             HEFF, THE ICE CONCENTRATION, AREAM THE NEGATIVE
C             ICE TO BE MELTED YNEG AND THE ICE TEMPERATURE, TICE.
C
C  INCLUDE FILES:                  NONE
C
C  COMPILER DEPENDENCIES:          Fortran 77 
C
C  COMPILE OPTIONS:                -a stack    memory allocation
C                                  -o zeroinc  optimization options
C                                  -d p        disable double precision
C                                  -e z        enable debugger
C                                  -m 4  message category - error message 
C
C  MAKEFILE:                       LOCATION?
C
C*****************************END PROLOGUE****************************
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NX=79,NY=141,NX1=80,NY1=142) 
C 
C********************************************************************* 
C 
C     RUTH H. PRELLER NRL CODE 322, STENNIS SPACE CENTER, MS. 39529 
C 
C********************************************************************* 
C
       real*4 u,v,u2,v2,h,a,y,t
       DIMENSION UICE(NX,NY,3),VICE(NX,NY,3),UICEC(NX,NY),
     *          VICEC(NX,NY),HEFF(NX1,NY1,3),AREA(NX1,NY1,3), 
     *          YNEG(NX1,NY1),TICE(NX1,NY1) 
       dimension u(nx,ny),v(nx,ny),u2(nx,ny),v2(nx,ny),h(nx1,ny1),
     *           a(nx1,ny1),y(nx1,ny1),t(nx1,ny1)
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c       had to change this to read in our formatted file
c
c        READ(3) UICE  
c        READ(3) UICE  
c        READ(3) VICE  
c        READ(3) UICEC 
c        READ(3) VICEC 
c        READ(3) HEFF  
c        READ(3) AREA  
c        READ(3)YNEG 
c        READ(3)TICE 
c      READ(3,1101,END=90) NPOCDTG  
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        READ(3,101)((U(i,j),i=1,nx),j=1,ny)  
        READ(3,101)((V(i,j),i=1,nx),j=1,ny)  
        READ(3,101)((u2(i,j),i=1,nx),j=1,ny) 
        READ(3,101)((V2(i,j),i=1,nx),j=1,ny) 
        READ(3,101)((H(i,j),i=1,nx1),j=1,ny1)  
        READ(3,101)((A(i,j),i=1,nx1),j=1,ny1)  
        READ(3,101)((Y(i,j),i=1,nx1),j=1,ny1) 
        READ(3,101)((T(i,j),i=1,nx1),j=1,ny1) 
101     format(1x,6e13.6)
c
        do 5 k=1,3
        do 5 i=1,nx
        do 5 j=1,ny
        uice(i,j,k)=dble(u(i,j))
        vice(i,j,k)=dble(v(i,j))
        uicec(i,j)=dble(u2(i,j))
        vicec(i,j)=dble(v2(i,j))
5       continue
        do 11 k=1,3
        do 11 i=1,nx1
        do 11 j=1,ny1
        heff(i,j,k)=dble(h(i,j))
        area(i,j,k)=dble(a(i,j))
11      continue
        do 12 i=1,nx1
        do 12 j=1,ny1
        yneg(i,j)=dble(y(i,j))
        tice(i,j)=dble(t(i,j))
12      continue
c
        RETURN 
 90   CONTINUE 
      NPOCDTG=0 
      RETURN 
C 
 100  STOP 'RESTART DATA NOT AVAILABLE' 
C 
      END 
