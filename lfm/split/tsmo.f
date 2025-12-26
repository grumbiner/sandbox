       SUBROUTINE TSMO
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TSMO           CONTROLS CALLS TO TIME SMOOTHER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: APPLIES ROBERT TIME FILTER TO US,VS,TS,PSIG,AND WS
C
C USAGE:  CALL TSMO
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES AT THREE TIME LEVELS   /DEABLK/
C     ALP         ALPHA COEFFICIENT FOR TIME SMOOTHER       /TTME/
C     LOLD        TIME INDEX FOR TAU-1 LEVEL                /INDEX/
C     LMID             DITTO     TAU   LEVEL                /INDEX/
C     LNEW             DITTO     TAU+1 LEVEL                /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES AFTER T SMOOTHER       /DEABLK/
C                 (LMID (TAU) LEVEL HAS BEEN SMOOTHED)
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DOTSMO                                                LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        ICE, MF
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
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53 , 12, 23 ), BB( 12, 33, 23 )
      COMMON /DEABLK/ US( 53 , 45, 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      SAVE
C
       XAA =1.E0 -2.E0 * ALP
       LEN =  53  *  45  * 7
       L1 = 1 + LOLD * 7
       L2 = 1 + LMID * 7
       L3 = 1 + LNEW * 7
       CALL DOTSMO(US(1,1,L1),US(1,1,L2),US(1,1,L3),LEN,XAA,ALP)
       CALL DOTSMO(VS(1,1,L1),VS(1,1,L2),VS(1,1,L3),LEN,XAA,ALP)
       CALL DOTSMO(TS(1,1,L1),TS(1,1,L2),TS(1,1,L3),LEN,XAA,ALP)
       LEN =  53  *  45  * 3
       L1 = 1 + LOLD * 3
       L2 = 1 + LMID * 3
       L3 = 1 + LNEW * 3
       CALL DOTSMO(WS(1,1,L1),WS(1,1,L2),WS(1,1,L3),LEN,XAA,ALP)
       LEN =  53  *  45  * 2
       L1 = 1 + LOLD * 2
       L2 = 1 + LMID * 2
       L3 = 1 + LNEW * 2
       CALL DOTSMO(PSIG(1,1,L1),PSIG(1,1,L2),PSIG(1,1,L3),LEN,XAA,ALP)
       RETURN
       END
