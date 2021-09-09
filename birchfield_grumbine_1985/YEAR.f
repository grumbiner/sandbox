      SUBROUTINE YEAR(PHI,JP) 
C     JP IS POSITION IN YEAR (1 TO KYEAR) 
C     JP = 1 IS THE VERNAL EQUINOX
      REAL WW, COSZB
      REAL PHI
      INTEGER JP
      COMMON/NORBIT/DUM, XOB, ECC, PRE, PERH
  
C     XOB OBLIQUITY 
C     ECC ECCENTRICITY
C     PRE ECC*SIN(PERH)  =  PRECESSIONAL PARAMETER
C     PERH LONGITUDE OF PERIHELION
      COMMON/TEMP/INSOL 
      REAL INSOL,PI,SC
      DATA PI/3.141592635898/ 
      DATA SC/1367.0/ 
C     MODIFIED TO RUN ONLY ON COARSE GRID 8/24/83.
      TEST = 0.0001 
      PIR = PI/180. 
  
C     INSOLATION WILL BE IN SAME UNITS AS SOLAR CONSTANT, RATHER THAN 
C     DAILY,AS IN BERGERS PROGRAM, BECAUSE THE CONVERSION FACTOR TAU
C     (TAU = 86.4(K SEC/DAY)) IS OMITTED. 
      SF = SC/PI
C     SO SINE OF OBLIQUITY
      SO = SIN(XOB*PIR) 
C     XL LONGITUDE OF PERIHELION + 180 DEGREES
      XL = PERH+180.
      XLLP = XL*PIR 
C     COMPUTE MEAN LONGITUDE AT THE VERNAL EQUINOX
      XEE = ECC*ECC 
      XSE = SQRT(1.-XEE)
      XLAM = ECC*(0.5+XEE/8.)*(1.+XSE)*SIN(XLLP)
     1 -XEE/4.*(.5+XSE)*SIN(2.*XLLP)
     2 +ECC*XEE/8*(1./3.+XSE)*SIN(3.*XLLP)
      XLAM = 2.*XLAM/PIR
C     INCREMENT FOR MEAN ANOMALY - ALM
      ALMINC = 6.0
      ALM = XLAM-XL-ALMINC
C     MULTIPLY ALMINC BY JP TO POSITION US AT THE RIGHT PART OF YEAR
      ALM = ALM+ALMINC*JP 
      RANM = ALM*PIR
  
C     FIND TRUE ANOMALY - RANV
      RANV = RANM+ECC*(2.-XEE/4)*SIN(RANM)
     1 +5./4.*XEE*SIN(2.*RANM)
     2 +13./12.*ECC*XEE*SIN(3.*RANM)
      ANV = RANV/PIR
C     TRUE LONGITUDE OF THE SUN - DLAM
      DLAM = ANV+XL 
      RLAM = DLAM*PIR 
  
C     EARTH-SUN DISTANCE RAU
      RAU    = (1.-XEE)/(1.+ECC*COS(RANV))
      RSQIN  = 1./(RAU*RAU) 
      RSQPIN = 1./(PI*RAU*RAU)
      S    = SF/(RAU*RAU) 
  
C     COMPUTE SIN,COS OF DECLINATION OF SUN 
      SD = SO*SIN(RLAM) 
      CD = SQRT(1-SD*SD)
  
C     DELTA IS DECLINATION OF SUN 
      RDELTA = ATAN(SD/CD)
      DELTA  = RDELTA/PIR 
      ADELTA = ABS(DELTA) 
C     START PHI AT -90 DEGREES
      RPHI = PHI*PIR
      SP   = SD*SIN(RPHI) 
      CP   = CD*COS(RPHI) 
      APHI = ABS(PHI) 
      TT   = ABS(APHI-90.)
  
C     SINGULARITY FOR APHI = 90 AND DELTA = 0 
C     PARTICULAR CASES FOR PHI = 0 OR DELTA = 0 
      IF ((TT.LE.TEST).AND.(ADELTA.LE.TEST)) GO TO 2
      IF (ADELTA.LE.TEST) GO TO 6 
      IF (APHI.LE.TEST) GO TO 7 
      AT = 90-ADELTA
      SPD = PHI*DELTA 
      IF (APHI.LT.AT) GO TO 3 
      IF (SPD) 2,3,4
C     POLAR CONTINUAL NIGHT 
  2   WW    = 0.0 
C     THE VALUE OF COSZB IS ARBITRARY 
      COSZB = 0.0 
      GO TO 200 
  
C     POLAR CONTINUAL DAY 
  4   WW    = S*SP*PI 
      COSZB = SP*RSQIN
      GO TO 200 
  
C     DAILY SUNRISE AND SUNSET
  3   TP    = -SP/CP
      STP   = SQRT(1.-TP*TP)
      RDAYL = ACOS(TP)
      WW    = S*(RDAYL*SP+CP*STP) 
      COSZB = RSQIN*(SP+CP*STP/RDAYL) 
      GO TO 200 
  
C     EQUINOXES 
  6   WW    = S*COS(RPHI) 
      COSZB = 2.0*RSQPIN*COS(RPHI)
      GO TO 200 
  
C     EQUATOR 
  7   WW    = S*COS(RDELTA) 
      COSZB = 2.0*RSQPIN*COS(RDELTA)
 200  CONTINUE
  
      INSOL = WW
  
  
      RETURN
      END 
