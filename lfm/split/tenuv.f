       SUBROUTINE TENUV
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENUV          CONTROLS CALLS TO MOMENTUM FCST SUBS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: CONTROLS CALLS TO MOMENTUM FORECAST SUBROUTINES.  ALSO
C   KEEPS TRACK OF LEVEL INDICIES.
C USAGE:  CALL TENUV
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     K           LAYER INDEX                               /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     KL          WORKING LAYER (MOD(2)+1) =S 1 OR 2        /INDEX/
C     KH              DITTO    PLUS ONE                     /INDEX/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TENDA,TENDB                                           LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /INDEX/ LI,LJ,LK,LI1,LJ1,LK1,LI2,NIJ,LOLD,LMID,LNEW,
     1     K7OLD,K7MID,K7NEW,K3OLD,K3MID,K3NEW,K2OLD,K2MID,K2NEW,
     2     K,K1,K2,K3,KL,KH
      COMMON /FDATE/ IYEAR, IMO, IDAYMO, IZTIME, IHR1, IOUT
      COMMON /SVHOUR/ NWDS, IHOUR1( 1205 ), HOUR1(8)
      COMMON /TTME/ MNSTEP,IODD,IHOUR,IMONTH,ITSW,IVEL,NPHOUR,
     1     SSLDC,CSLDC,SSLHR,CSLHR,SHR,CHR,
     2     ALP,XDAYMO
      COMMON /FORTAP/ LUNBND,LUNSIN,LUNS00,LUNS06,LUNS12,LUNV06,LUNV12,
     A                LABSIN(8),LABBND(8),LABS00(8),LABS06(8),LABS12(8),
     B                LABV06(8),LABV12(8),PARM(25)
      COMMON /PBNDPE/ ALARGE( 53, 12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
       IF ( K.GT.1 ) GO TO 10
       KL = 1
       KH = 2
       CALL TENDA
       GO TO 3
10     KTT = KL
       KL = KH
       KH = KTT
3      CALL TENDB
       RETURN
       END
