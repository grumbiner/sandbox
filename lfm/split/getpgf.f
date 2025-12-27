       SUBROUTINE GETPGF(LTIME)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: GETPGF         COMPUTE PRESS. GRAD. AVERAGE
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES THETA*D(PI)/DX + D(PHI)/DX AND
C  THETA*D(PI)/DY + D(PHI)/DY STORING THE RESULT IN PHI & PI.
C  THIS ROUTINE IS CALLED THREE TIMES, ONCE FOR EACH TIME LEVEL.
C  QUANTITIES ARE SCALED AND ACCUMULATED  IN PHI AND PI SO THAT AFTER
C  THE THIRD CALL THE PRESSURE GRADIENT AVERAGE IS IN PHI AND PI
C  DX VALUES IN PHI AND DY VALUES IN PI.
C  INPUT VALUES OF PHI AND PI HAVE BEEN COMPUTED AND STORED IN
C  US(LNEW) AND VS(LNEW) RESPECTIVELY.
C
C USAGE:  CALL GETPGF(LTIME)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LTIME       TIME LEVEL OF INPUT (TAU-1,TAU, OR TAU+1) ARGUMENT
C                 LOLD=TAU-1, LMID=TAU, LNEW=TAU+1
C     A1BRN       PRESS.GRAD. AVERAGE WEIGHT (TAU LEVEL)    /CNST/
C     A2BRN                 DITTO            (TAU-1,TAU+1)  /CNST/
C     TS          POT. TEMP. ALL THREE TIME LEVELS          /DEABLK/
C     US(1,1,K7NEW)   PHI AT LTIME LEVEL                    /DEABLK/
C     VS(1,1,K7NEW)   PI  AT LTIME LEVEL                    /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         PRESS. GRAD. AVERAGE (X-COMPONENT)        /GEOPOT/
C     PI                  DITTO         Y    DITTO          /GEOPOT/
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DSXY,DSYDX,DSXDY                                      LFM LIB
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
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ), PSIG( 53 , 45 ,6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 , 9 ),
     6     PREC( 53 , 45 ),ZSTAR( 53 , 45 ),
     7     ALON( 53 , 45 ),ALAT( 53 , 45 ),
     8     MF( 53 , 45 ),H2( 53 , 45 , 3 )
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
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
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53 , 45 ,7 ), PI( 53, 45, 7 )
      SAVE
C
       ALPWT = A1BRN
       IF (LTIME .EQ. LMID) ALPWT = A2BRN
       DO 100 L = 1,7
       LTLEV = L + LTIME*7
       CALL DSXY(VT(1,1,2),TS(1,1,LTLEV))
       CALL DSYDX(VT(1,1,3),US(1,1,K7NEW+L-1))
       CALL DSXDY(VT(1,1,4),US(1,1,K7NEW+L-1))
       CALL DSYDX(VT(1,1,5),VS(1,1,K7NEW+L-1))
       CALL DSXDY(VT(1,1,6),VS(1,1,K7NEW+L-1))
       DO 1 J = 1, 45
       DO 1 I = 1, 53
       PHI(I,J,L) = PHI(I,J,L) + ALPWT*(CP*VT(I,J,2)*VT(I,J,5) +
     A              VT(I,J,3))
       PI(I,J,L) = PI(I,J,L) + ALPWT*(CP*VT(I,J,2)*VT(I,J,6) +
     A             VT(I,J,4))
1      CONTINUE
100    CONTINUE
       RETURN
       END
