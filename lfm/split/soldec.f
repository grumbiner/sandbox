      SUBROUTINE SOLDEC (DD,MM,TT,IY)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SOLDEC         COMPUTES SOLAR DECLINATION ANGLE
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: COMPUTES SOLAR DECLINATION ANGLE.
C   MODIFIED SO    BOTH EQUATION OF TIME AND DECLINATION ARE COMPUTED
C   IN DEGREES AND THE OUTPUT IS IN TRIG FUNCTIONS INSTEAD OF ANGLES
C  E  IS EQUATION OF TIME I.E. DIFFERENCE BETWEEN MEAN TIME AND SOLAR
C       TIME
C  SLHR IS THE SHA OF THE SUN FOR 100E LONGITUDE FOR SYMMETRY REASONS
C     FORMULA AND SOME NOTATION FROM FINGER, WOOLFE + ANDERSON
C     MON. WEA. REV. OCT. 1965, PP 622 - 623
C         WITH CORRECTIONS
C
C USAGE:  CALL SOLDEC(DD,MM,TT,IY)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     DD          DAY OF MONTH                              ARGUMENT
C     MM          MONTH OF YEAR                               DITTO
C     TT          GREENWICH TIME  (HOURS)                     DITTO
C     IY          YEAR   FIXED POINT                          DITTO
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SSLDC       SIN OF SOLAR DECLINATION ANGLE            /TTME/
C     CSLDC       COS        DITTO                          /TTME/
C     SSLHR       SIN OF LOCAL HOUR ANGLE                   /TTME/
C     CSLHR       COS        DITTO                          /TTME/
C     SHR         SIN OF ONE (RADIAN HOUR) FOR INCREMENT    /TTME/
C     CHR         COS        DITTO                          /TTME/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     SIN,COS,MOD,ASIN                                      FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
C
C     TABLE OF DAY OF YEAR OF LAST DAY OF EACH MONTH WITH PHASE SHIFTED
C     ***************    NOT - REPEAT - NOT LEAP YEAR      *************
      DIMENSION DAYEAR(12)
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/  IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/   MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1                SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2                ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE (12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12,23 ), BB( 12, 33, 23 )
      SAVE
C
      DATA DAYEAR/0.E0,31.E0,59.E0,90.E0,120.E0,151.E0,
     1   181.E0,212.E0,243.E0,273.E0,304.E0,334.E0/
C
C     0.98565 = 360/365.242
C     0.39785 = SIN(23, 26, 37.8)
C     0.0174533 = RADIANS/DEGREE
C
C     LEAP YEAR
      X = 0.E0
      IF (MOD(IY,4).EQ.0 .AND. MM.GT.2) X = 1.E0
      PI    = 2.E0 * ASIN(1.0)
      PI180 = PI / 180.E0
      D = (DD + DAYEAR(MM) -1.E0+ X) / 365.242E0
      D = D*2.E0*PI
      SIG=PI180*(279.9348E0+1.914827E0*SIN(D)-.07952E0*COS(D)+
     1 .019938E0*SIN(2.E0*D) - .00162E0*COS(2.E0*D)) + D
      SSLDC=0.39783E0*SIN(SIG)
      CSLDC=SQRT(1.E0-SSLDC*SSLDC)
      T = (DD + DAYEAR(MM) -80.E0+ X) / 365.242E0
      T = T*2.E0*PI
      E=-0.03E0*SIN(T)-0.12E0*COS(T)+0.165E0*SIN(2.*T)-.0008E0*COS(2.*T)
C .........   SOLHR EQN MODIFIED FOR LFM GRID CENTER LINE AT 105 DEG W
      SOLHR = PI180 * (15.E0*(E + TT +12.E0+5.E0))
      SSLHR = SIN(SOLHR)
      CSLHR = COS(SOLHR)
C   SIN AND COS OF ONE HOUR FOR INCREMENTING PURPOSE
      SHR = SIN(PI/12.E0)
      CHR = COS(PI/12.E0)
      RETURN
      END
