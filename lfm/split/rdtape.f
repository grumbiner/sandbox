      SUBROUTINE RDTAPE
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: RDTAPE         READS INITIAL OR RESTART SIGMA STRIPS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: READS SIGMA STRIPS INTO BUFF.  USING SUBROUTINE TTOARY
C   THE STRIPS ARE CONVERTED AND STORED IN COMMON BLOCK /DEABLK/.
C
C USAGE:  CALL RDTAPE
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNS00      LOGICAL UNIT #, SIGMA STRIPS              /FORTAP/
C     LABS00      SIGMA STRIP LABEL                         /FORTAP/
C     HOUR1       SIGMA STRIP LABEL RECORD (FORECAST HR)    /FORTAP/
C     BUFF        SIGMA STRIP INPUT BUFFER                  LUNS00
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LOLD...     TIME LEVEL POINTERS                       /INDEX/
C     IHOUR       FORECAST HOUR OF INPUT SIGMA STRIPS       /TTME/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TTOARY                                                LFMLIB
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
      REAL        BUFF( 3816 )
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
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
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      COMMON /VTEMP/ SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C   THE TAPE HAS 1 ROW OF TAU, TAU-1 IN EACH RECORD.
C   THAT IS LI * (9 LEVELS) * (4 VARIABLES) * (4 BYTES PER WORD) *
C   (2 TIME LEVELS)
C     LOLD, LMID, LNEW - TIME LEVELS FOR BOUNDARY FILES INITIALIZED.
C
      LOLD = 0
      LMID = 1
      LNEW = 2
      READ (LUNS00) LABS00
C
      READ (LUNS00) HOUR1
C
      IHOUR = HOUR1(1)
      DO 10 LOOP = 1, 45
        READ (LUNS00) BUFF
        CALL TTOARY(LOOP,BUFF)
   10 CONTINUE
      REWIND LUNS00
      RETURN
      END
