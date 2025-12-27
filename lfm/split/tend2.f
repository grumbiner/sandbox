       SUBROUTINE TEND2
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND2          COMPUTES TERMS USED BY MANY EQUATIONS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES TERMS THAT ARE COMMON TO MOST EQUATIONS STORED
C   IN VECTOR TEMPORARY ARRAY VT.
C
C USAGE:  CALL TEND2
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     ALL FORECAST VARIABLES & FIXED FIELDS     /DEABLK/
C     K,KL,KH     LAYER POINTERS                            /INDEX/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     VT          VECTOR TEMPORARY STORAGE SHOWN BELOW      /VTEMP/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY                                                  LFM LIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C----------------------------------------------------------------------
C   STORAGE IN VT AS FOLLOWS:
C
C   ********************************
C
C   --------------- KH -------------
C
C   ********************************
C
C   --------------- KL -------------  K
C
C   ********************************
C
C    VT(1,1,1)             = MAP FACTOR SQUARED BAR XY (DONE IN TEND3)
C    VT(1,1,KL+1-KH+1)     = US BAR XY
C    VT(1,1,KL+3-KH+3)     = VS BAR XY
C    VT(1,1,KL+5-KH+5)     = TS BAR XY
C    VT(1,1,KL+7-KH+7)     = WS BAR XY
C    VT(1,1,10-11)         = PSIG BAR XY        (DONE IN TEND3)
C    VT(1,1,12-13)         = D(PSIG)/DX BAR Y   (DONE IN TEND3)
C    VT(1,1,14-15)         = D(PSIG)/DY BAR X   (DONE IN TEND3)
C
C---------------------------------------------------------------------
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        HOUR1
      REAL        ICE, MF
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 ,3)
      COMMON /VERTV/ IVVEL( 53 , 45 ,8)
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
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
      SAVE
C
       IF (K .GT. 1) GO TO 100
       CALL DSXY(VT(1,1,KL+1),US(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+3),VS(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+5),TS(1,1,K7MID))
       CALL DSXY(VT(1,1,KL+7),WS(1,1,K3MID))
100    IF (K .GE. 7) GO TO 200
       CALL DSXY(VT(1,1,KH+1),US(1,1,K7MID+1))
       CALL DSXY(VT(1,1,KH+3),VS(1,1,K7MID+1))
       CALL DSXY(VT(1,1,KH+5),TS(1,1,K7MID+1))
       IF (K .GE. 3) GO TO 200
       CALL DSXY(VT(1,1,KH+7),WS(1,1,K3MID+K))
200    RETURN
       END
