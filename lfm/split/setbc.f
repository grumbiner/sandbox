       SUBROUTINE SETBC
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SETBC          SETS LATERAL BOUNDARY CONDITIONS ON U & V
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES LATERAL BOUNDARY DIFFUSIVE NUDGE FOR U & V.
C   PERIMETER VALUES ARE ALSO RESTORED.
C
C USAGE:  CALL SETBC
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      LATERAL BOUNDARY VALUES                   /PBNDPE/
C     BLARGE                DITTO                           /PBNDPE/
C     US,VS       U & V COMPONENTS OF THE WIND              /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS       U & V WITH BOUNDARY CONDITIONS APPLIED    /DEABLK/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     BNDLAP                                                LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
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
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53, 45, 8 )
      SAVE
C
       DO 10 K = 1,7
         L7 = K + LNEW * 7
         CALL BNDLAP(ALARGE(1,1,K),BLARGE(1,1,K),US(1,1,L7))
         CALL BNDLAP(ALARGE(1,1,K+7),BLARGE(1,1,K+7),VS(1,1,L7))
10     CONTINUE
       RETURN
       END
