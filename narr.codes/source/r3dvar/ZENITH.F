C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                         SUBROUTINE ZENITH(TIMES,DAYI,HOUR)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                               .
C SUBPROGRAM:    ZENITH      COMPUTE THE SOLAR ZENITH ANGLE
C   PRGRMMR: BLACK           ORG: W/NMC22    DATE: 93-10-28       
C     
C ABSTRACT:
C     ZENITH CALCULATES THE COSINE OF THE SOLAR ZENITH ANGLES
C     AT EACH POINT FOR USE IN SWRAD
C     
C PROGRAM HISTORY LOG:
C   87-08-??  BLACK      - ORIGINATOR
C   98-07-??  BLACK      - MODIFIED TO NEED ONLY DATE AS INPUT
C   98-11-10  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL ZENITH FROM SUBROUTINE RADTN
C
C   INPUT ARGUMENT LIST:
C       TIMES  - THE FORECAST TIME IN SECONDS
C  
C   OUTPUT ARGUMENT LIST: 
C       DAYI   - THE DAY OF THE YEAR
C       HOUR   - THE HOUR OF THE DAY
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
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  PHYS
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C***********************************************************************
                             P A R A M E T E R
     & (GSTC1=24110.54841,GSTC2=8640184.812866
     &, GSTC3=9.3104E-2,GSTC4=-6.2E-6
     &, PI=3.1415926,PI2=2.*PI,PIH=0.5*PI
     &, DEG2RD=1.745329E-2,OBLIQ=23.440*DEG2RD,ZEROJD=2451545.0)
C-----------------------------------------------------------------------
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (LP1=LM+1)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,LEAP
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
                             D I M E N S I O N
     1 MONTH (12)
C-----------------------------------------------------------------------
                             D A T A
     1 MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C***********************************************************************
      SAVE MONTH
      DAY=0.
      LEAP=.FALSE.
      IF(MOD(IDAT(3),4).EQ.0)THEN
        MONTH(2)=29
        LEAP=.TRUE.
      ENDIF
      IF(IDAT(1).GT.1)THEN
        KMNTH=IDAT(1)-1
        DO 10 KNT=1,KMNTH
        DAY=DAY+REAL(MONTH(KNT))
   10   CONTINUE
      ENDIF
C***
C***  CALCULATE EXACT NUMBER OF DAYS FROM BEGINNING OF YEAR TO
C***  FORECAST TIME OF INTEREST 
C***
      DAY=DAY+REAL(IDAT(2)-1)+(REAL(IHRST)+TIMES/3600.)/24.
      DAYI=REAL(INT(DAY)+1)
      HOUR=(DAY-DAYI+1.)*24.
      YFCTR=2000.-IDAT(3)
C-----------------------------------------------------------------------
C***
C***  FIND CELESTIAL LONGITUDE OF THE SUN THEN THE SOLAR DECLINATION AND
C***  RIGHT ASCENSION.
C***
C-----------------------------------------------------------------------
      IDIFYR=IDAT(3)-2000
C***
C***  FIND JULIAN DATE OF START OF THE RELEVANT YEAR
C***  ADDING IN LEAP DAYS AS NEEDED
C***
      IF(IDIFYR.LT.0)THEN
        ADDDAY=REAL(IDIFYR/4)
      ELSE
        ADDDAY=REAL((IDIFYR+3)/4)
      ENDIF
      STARTYR=ZEROJD+IDIFYR*365.+ADDDAY-0.5
C***
C***  THE JULIAN DATE OF THE TIME IN QUESTION
C***
      DATJUL=STARTYR+DAY
C
C***  DIFFERENCE OF ACTUAL JULIAN DATE FROM JULIAN DATE
C***  AT 00H 1 January 2000
C
      DIFJD=DATJUL-ZEROJD
C
C***  MEAN GEOMETRIC LONGITUDE OF THE SUN
C
      SLONM=(280.460+0.9856474*DIFJD)*DEG2RD+YFCTR*PI2
C
C***  THE MEAN ANOMOLY
C
      ANOM=(357.528+0.9856003*DIFJD)*DEG2RD
C
C***  APPARENT GEOMETRIC LONGITUDE OF THE SUN
C
      SLON=SLONM+(1.915*SIN(ANOM)+0.020*SIN(2.*ANOM))*DEG2RD
      IF(SLON.GT.PI2)SLON=SLON-PI2
C
C***  DECLINATION AND RIGHT ASCENSION
C 
      DEC=ASIN(SIN(SLON)*SIN(OBLIQ))
      RA=ACOS(COS(SLON)/COS(DEC))
      IF(SLON.GT.PI)RA=PI2-RA
C***
C***  FIND THE GREENWICH SIDEREAL TIME THEN THE LOCAL SOLAR
C***  HOUR ANGLE.
C***
      DATJ0=STARTYR+DAYI-1.
      TU=(DATJ0-2451545.)/36525.
      STIM0=GSTC1+GSTC2*TU+GSTC3*TU**2+GSTC4*TU**3
      SIDTIM=STIM0/3600.+YFCTR*24.+1.00273791*HOUR
      SIDTIM=SIDTIM*15.*DEG2RD
      IF(SIDTIM.LT.0.)SIDTIM=SIDTIM+PI2
      IF(SIDTIM.GT.PI2)SIDTIM=SIDTIM-PI2
      HRANG=SIDTIM-RA
C
      DO 100 J=MYJS,MYJE
      DO 100 I=MYIS,MYIE
      HRLCL=HRANG-GLON(I,J)
C***
C***  THE ZENITH ANGLE IS THE COMPLEMENT OF THE ALTITUDE THUS THE
C***  COSINE OF THE ZENITH ANGLE EQUALS THE SINE OF THE ALTITUDE.
C***
      SINALT=SIN(DEC)*SIN(GLAT(I,J))+COS(DEC)*COS(HRLCL)*
     1 COS(GLAT(I,J))
      IF(SINALT.LT.0.)SINALT=0.
      CZEN(I,J)=SINALT
  100 CONTINUE
C***
C***  IF THE FORECAST IS IN A DIFFERENT YEAR THAN THE START TIME,
C***  RESET DAYI TO THE PROPER DAY OF THE NEW YEAR (IT MUST NOT BE
C***  RESET BEFORE THE SOLAR ZENITH ANGLE IS COMPUTED).
C***
      IF(DAYI.GT.365.)THEN
        IF(.NOT.LEAP)THEN
          DAYI=DAYI-365.
        ELSEIF(LEAP.AND.DAYI.GT.366.)THEN
          DAYI=DAYI-366.
        ENDIF
      ENDIF
      RETURN
      END
