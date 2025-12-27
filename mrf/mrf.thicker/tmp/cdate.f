      SUBROUTINE CDATE(JD,FJD,MUNTH,IM,ID,IYEAR,IHR,XMIN)
CFPP$ NOCONCUR R
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CDATE       COMPUTES DAY,MONTH,YR FROM JULIAN DAY
C   PRGMMR: KENNETH CAMPANA  ORG: W/NMC23    DATE: 89-07-07
C
C ABSTRACT: THIS CODE WRITTEN AT GFDL ....
C   COMPUTES MONTH,DAY,YEAR FROM JULIAN DAY.
C   ACCURATE ONLY BETWEEN MARCH 1, 1900 AND FEBRUARY 28, 2100....
C   BASED ON JULIAN CALENDER CORRECTED TO CORRESPOND TO GREGORIAN
C   CALENDER DURING THIS PERIOD.
C
C PROGRAM HISTORY LOG:
C   77-06-07  ROBERT WHITE,GFDL
C
C USAGE:    CALL CDATE(JD,FJD,MUNTH,IM,ID,IYEAR,IHR,XMIN)
C   INPUT ARGUMENT LIST:
C     JD       - JULIAN DAY FOR CURRENT FCST HOUR.
C     FJD      - FRACTION OF THE JULIAN DAY.
C   OUTPUT ARGUMENT LIST:
C     MUNTH    - MONTH (CHARACTER).
C     IM       - MONTH (INTEGER).
C     ID       - DAY OF THE MONTH.
C     IYEAR    - YEAR.
C     IHR      - HOUR OF THE DAY.
C     XMIN     - MINUTE OF THE HOUR.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
C
C    *******************************************************************
C    *                                                                 *
C    *                            C D A T E                            *
C    *                                                                 *
C    *******************************************************************
C
C         STATEMENTS BLOCKED BY ROBERT K. WHITE.......7 JUNE 1977
C
C..... CDATE COMPUTES MONTH, DAY, AND YEAR FROM JULIAN DAY.
C.....ACCURATE ONLY BETWEEN MARCH 1, 1900 AND FEBRUARY 28, 2100
C.....BASED ON JULIAN CALENDER CORRECTED TO CORRESPOND TO GREGORIAN
C.....CALENDER DURING THIS PERIOD
C
                              D I M E N S I O N
     1   DY(13),             MONTH(12)
C
C
                                   D A T A
     1   DY                  /
     2   0.,                 31.,                59.,
     3   90.,                120.,               151.,
     4   181.,               212.,               243.,
     5   273.,               304.,               334.,
     6   365.                /
C
                                   D A T A
     1   MONTH               /
     2   4HJAN.,             4HFEB.,             4HMAR.,
     3   4HAPR.,             4HMAY ,             4HJUNE,
     4   4HJULY,             4HAUG.,             4HSEP.,
     5   4HOCT.,             4HNOV.,             4HDEC.
     6   /
C
C.....JDOR = JD OF DECEMBER 30, 1899 AT 12 HOURS UT
C
                                   D A T A
     1   JDOR                /         2415019             /,
     2   IYR                 /         1900                /
C
C    *******************************************************************
C
      IYEAR=IYR
      NDAY=JD-JDOR
      IF(FJD.GE..5 E 0) NDAY=NDAY+1
 61   IF(NDAY.LT.1462) GO TO 62
      NDAY=NDAY-1461
      IYEAR=IYEAR+4
      GO TO 61
 62   NDIY=365
      IF(MOD(IYEAR,4).EQ.0) NDIY=366
      IF(NDAY.LE.NDIY) GO TO 65
      IYEAR=IYEAR+1
      NDAY=NDAY-NDIY
      GO TO 62
 65   IF(NDAY.GT.INT(DY(2))) GO TO 66
      IM=1
      ID=NDAY
      GO TO 67
 66   IF(NDAY.NE.60) GO TO 68
      IF(NDIY.EQ.365) GO TO 68
      IM=2
      ID=29
      GO TO 67
 68   IF(NDAY.GT.(INT(DY(3))+NDIY-365)) GO TO 69
      IM=2
      ID=NDAY-31
      GO TO 67
 69   DO 70 I=3,12
      IF(NDAY.GT.(INT(DY(I+1))+NDIY-365)) GO TO 70
      IM=I
      ID=NDAY-INT(DY(I))-NDIY+365
      GO TO 67
 70   CONTINUE
 67   MUNTH=MONTH(IM)
      HR=24. E 0*FJD
      IHR=HR
      XMIN=60. E 0*(HR-FLOAT(IHR))
      IHR=IHR+12
      IF(IHR.GE.24) IHR=IHR-24
      RETURN
      END
