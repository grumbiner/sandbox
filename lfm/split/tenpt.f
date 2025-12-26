       SUBROUTINE TENPT
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TENPT          CONTROLS CALLS TO MASS AND MOISTURE FCST
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: CONTROLS CALLS TO MASS (PSIG & TS) AND MOISTURE (WS)
C   FORECAST SUBROUTINES.  ALSO SETS LEVEL POINTERS.
C
C USAGE:  CALL TENPT
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     K           VALUE OF LAYER INDEX (1,2,3...7)          /INDEX/
C     IVEL        VERT VEL FLAG 1 = YES, 0 = NO             /TTME/
C     MNSTEP      TIME STEP IN HOUR (1,2,3...NPHOUR)        /TTME/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     KL          WORKING LAYER (MOD(2)+1) =   1 OR 2       /INDEX/
C     KH              DITTO     PLUS ONE                    /INDEX/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     TEND1,TEND2,TEND3,TEND4,TEND5,TEND6,VVEL              LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
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
      COMMON /PBNDRY/ AA( 53 ,12,23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ), PI( 53 , 45, 7 )
      SAVE
C
        IF (K.GT.1) GO TO 10
        KL = 1
        KH = 2
        CALL TEND1(PHI,PI)
        CALL TEND3
        GO TO 3
10      KTT = KL
        KL  = KH
        KH  = KTT
3       CALL TEND2
        IF (IVEL .EQ.1 ) CALL VVEL
4       IF (MNSTEP.GT.1 .OR. K .GT.1) GO TO 5
        CALL TEND4
5       CALL TEND5
        CALL TEND6
        RETURN
        END
