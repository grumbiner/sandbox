C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                     SUBROUTINE GRADFS
C    ******************************************************************
C    *                                                                *
C    *  THIS SUBROUTINE WAS MODIFIED BY Q. ZHAO TO BE USED IN THE     *
C    *  ETA MODEL.      1993.  11.   18.                              *
C    *                                                                *
C    ******************************************************************
C***
C***  REQUIRED INPUT
C***
     1 (SIGL,KCCO2,NFILE)
C**************
C*   SIGL(LP1):	MIDLAYER PRESSURES IN PA (LP1=LM+1)
C*   KCCO2:	=0 (NOT USED)
C*   NFILE:	THE FILE NUMBER FOR O3 DATA
C**************
      INCLUDE "parmeta"
      INCLUDE "rdparm"
#include "sp.h"
C***********************************************************************
      PARAMETER (LNGTH=37*L)
      COMMON /RDFSAV/ EMISP,EMIST,XLATT,XLATP,Q19001,HP98,H3M6,
     *     HP75,H6M2,HP537,H74E1,H15E1,Q14330,HP2,TWENTY,HNINE,
     *     DEGRAD,HSIGMA,DAYSEC,RCO2,
     *     CAO3SW(5),CAH2SW(5),CBSW(5)
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
      DIMENSION SIGL(LP1)
CZHAO DIMENSION SIGL(L)
      DIMENSION XAO3SW(5),XAH2SW(5),XBSW(5)
      DATA
     1   XAO3SW / 0., .690, .480, .210, 0./ ,
     2   XAH2SW / 0., .690, .480, .210, 0./ ,
     3   XBSW   / 0., .035, .020, .005, 0./
CCCCCCMIC$ GUARD 0004
C..**************************
C..**************************
C      ********************************************************
C      *      ONE TIME COMPUTATION OF NECESSARY QUANTITIES    *
C      ********************************************************
C....    INITIALIZE ARRAYS,GET CONSTANTS,ETC...
      PI=3.1415927
      Q19001=19.001
      HP98=0.98
      H3M6=3.0E-6
      HP537=0.537
      H74E1=74.0
      H15E1=15.0
      Q14330=1.43306E-6
      HP2=0.2
      TWENTY=20.0
      HNINE=9.0
      DEGRAD=180.0/PI
      HSIGMA=5.673E-5
      DAYSEC=1.1574E-5
C  ATMOSPERIC CARBON DIOXIDE CONCENTRATION IS NOW READ BY CONRAD,
C  BUT IT DEFAULTS TO 330 PPM FOR BACKWARD COMPATIBILITY.
      RCO2=3.3E-4
      CALL HCONST
C.... INTERPOLATE CLIMO O3 TO THE CURRENT VERTICAL COORDINATE...
C..      NEED LAYER SIGMA, GET FROM PSFC AND LAYER P FOR I=1.....
      DO 3 I = 1 , 5
      CAO3SW(I) = XAO3SW(I)
      CAH2SW(I) = XAH2SW(I)
      CBSW(I) = XBSW(I)
    3 CONTINUE
C***
C***  CONVERT SIGL FROM PA TO MB TO BE USED IN O3INT
C***
      DO 100 LV=1,LP1
      SIGL(LV)=0.01*SIGL(LV)
 100  CONTINUE
      CALL O3INT(SIGL)
      CALL CONRAD(NFILE)
C....  AVERAGE CLIMATOLOGICAL VALUS OF O3 FROM 5 DEG LAT MEANS, SO THAT
C      TIME AND SPACE INTERPOLATION WILL WORK (DONE ELSEWHERE IN RADFS)
      DO 5 I=1,LNGTH
      AVG=.25E0*(RAD1(I)+RAD2(I)+RAD3(I)+RAD4(I))
      A1=.5E0*(RAD2(I)-RAD4(I))
      B1=.5E0*(RAD1(I)-RAD3(I))
      B2=.25E0*((RAD1(I)+RAD3(I))-(RAD2(I)+RAD4(I)))
      RAD1(I)=AVG
      RAD2(I)=A1
      RAD3(I)=B1
      RAD4(I)=B2
    5 CONTINUE
      EMIST = .6E0
      EMISP = .3E0
      XLATP = 60.E0
      XLATT = 30.E0
C
      RETURN
      END
