C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                         SUBROUTINE SOLARD(R1)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                               .
C SUBPROGRAM:    SOLARD      COMPUTE THE SOLAR-EARTH DISTANCE
C   PRGRMMR: Q.ZHAO           ORG: W/NMC2     DATE: 96-7-23       
C     
C ABSTRACT:
C     SOLARD CALCULATES THE SOLAR-EARTH DISTANCE ON EACH DAY
C     FOR USE IN SHORT-WAVE RADIATION.
C     
C PROGRAM HISTORY LOG:
C   96-07-23  Q.ZHAO      - ORIGINATOR
C   98-10-09  Q.ZHAO      - CHANGED TO USE IW3JDN IN W3LIB TO
C                           CALCULATE JD.
C     
C USAGE: CALL SOLARD FROM SUBROUTINE INIT
C
C   INPUT ARGUMENT LIST:
C       NONE
C  
C   OUTPUT ARGUMENT LIST: 
C       R1   - THE NON-DIMENSIONAL DISTANCE BETWEEN SUN AND THE EARTH
C              (LESS THAN 1.0 IN SUMMER AND LARGER THAN 1.0 IN WINTER).
C     
C   INPUT FILES:
C     NONE
C        
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: IW3JDN
C  
C   COMMON BLOCKS: CTLBLK
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
#include "sp.h"
C***********************************************************************
                             P A R A M E T E R
     & (PI=3.1415926,PI2=2.*PI)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,LEAP
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     1 NDM (12)
C-----------------------------------------------------------------------
                             D A T A
     1   JYR19/1900/,     JMN/0/,  CCR/1.3E-6/
C
                             D A T A
     1   NDM/0,31,59,90,120,151,181,212,243,273,304,334/
C
C.....TPP = DAYS BETWEEN EPOCH AND PERIHELION PASSAGE OF 1900
C.....JDOR1 = JD OF DECEMBER 30, 1899 AT 12 HOURS UT
C.....JDOR2 = JD OF EPOCH WHICH IS JANUARY 0, 1990 AT 12 HOURS UT
C
                                   D A T A
     1   TPP/1.55/,    JDOR1/2415019/,     JDOR2/2415020/
C
C    *******************************************************************
C     COMPUTES JULIAN DAY AND FRACTION FROM YEAR, MONTH, DAY AND TIME UT
C     ACCURATE ONLY BETWEEN MARCH 1, 1900 AND FEBRUARY 28, 2100
C     BASED ON JULIAN CALENDAR CORRECTED TO CORRESPOND TO GREGORIAN
C        CALENDAR DURING THIS PERIOD
C    *******************************************************************
      JYR=IDAT(3)
      JMNTH=IDAT(1)
      JDAY=IDAT(2)
      JHR=IHRST
C
      JD=IW3JDN(JYR,JMNTH,JDAY)
      IF(JHR.GE.12) THEN
      JD=JD-1
      FJD=.5E0+.041666667E0*FLOAT(JHR)+.00069444444E0*FLOAT(JMN)
      ELSE
  7   FJD=.041666667E0*FLOAT(JHR-12)+.00069444444E0*FLOAT(JMN)
      END IF
      DAYINC=JHR/24.0
      JD=JD+FJD+DAYINC
      FJD=JD+FJD+DAYINC-JD
C***
C*** CALCULATE THE SOLAR-EARTH DISTANCE
C***
      DAT=FLOAT(JD-JDOR2)-TPP+FJD
C***
C    COMPUTES TIME IN JULIAN CENTURIES AFTER EPOCH
C***
      T=FLOAT(JD-JDOR2)/36525.E0
C***
C    COMPUTES LENGTH OF ANOMALISTIC AND TROPICAL YEARS (MINUS 365 DAYS)
C***
      YEAR=.25964134E0+.304E-5*T
C***
C    COMPUTES ORBIT ECCENTRICITY  FROM T
C***
      EC=.01675104E0-(.418E-4+.126E-6*T)*T
      YEAR=YEAR+365.E0
C***
C    DATE=DAYS SINCE LAST PERIHELION PASSAGE
C***
      DATE = MOD(DAT,YEAR)
C***
C    SOLVE ORBIT EQUATIONS BY NEWTON'S METHOD
C***
      EM=PI2*DATE/YEAR
      E=1.E0
      ITER = 0
 31   EP=E-(E-EC*SIN(E)-EM)/(1.E0-EC*COS(E))
      CR=ABS(E-EP)
      E=EP
      ITER = ITER + 1
      IF(ITER.GT.10) GOTO 1031
      IF(CR.GT.CCR) GO TO 31
 1031 CONTINUE
      R1=1.E0-EC*COS(E)
C
      WRITE(6,1000)JYR,JMNTH,JDAY,JHR,R1
 1000 FORMAT('SUN-EARTH DISTANCE CALCULATION FINISHED IN SOLARD'/
     &       'YEAR=',I5,'  MONTH=',I3,'  DAY=',I3,' HOUR='
     &,      I3,' R1=',F9.4)
C***
C    RETURN TO RADTN
C***
      RETURN
      END
