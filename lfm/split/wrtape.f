      SUBROUTINE WRTAPE
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: WRTAPE         WRITES SIGMA STRIPS AND VERT VELS TO DISK
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: CHANGES FORECAST QUANTITIES TO SIGMA STRIP FORMAT AND
C   WRITES THE STRIPS TO DISK FOR RESTART OR USE BY POST PROCESSOR
C
C USAGE:  CALL WRTAPE
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LUNV06-12   LOGICAL UNIT NUMBERS FOR VERT VEL OUTPUT  /FORTAP/
C     LUNS06-12            DITTO           SIGMA STRIP      /FORTAP/
C     HOUR1       RECORD LABEL                              /SVHOUR/
C     IVVEL       VERTICAL VELOCITIES (2 HR AVERAGE)        /VERTV/
C     LOLD..      TIME INDICIES                             /INDEX/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     HOUR1       LABEL FOR SIGMA STRIPS                    LUNS06-12
C     WO          VERTICAL VELOCITIES (ONE SLAB)            LUNV06-12
C     BUFF        SIGMA STRIP (ONE SLAB)                    LUNS06-12

C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     MOD                                                   FORTLIB
C     ARYTOT                                                LFM
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
      REAL        HOUR1
      REAL        ICE, MF
      REAL        WO( 53 ,7), BUFF( 3816 )
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
     2     ST( 53 , 45 ),ICE( 53 , 45 ),
     3     COSZEN( 53 , 45 ),XM( 53 , 45 ),
     4     F( 53 , 45 ),CD( 53 , 45 ),
     5     SATW( 53 , 45 ,3),WS( 53 , 45 ,9),
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
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE(12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 , 23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C  IWO AND IPO POINT TO PROPER LOGICAL UNIT NUMBERS FOR OUTPUT
C
       IWO = LUNV12
       IPO = LUNS12
       IF (MOD(IHOUR+1,12) .EQ. 0 ) GO TO 10
         IWO = LUNV06
         IPO = LUNS06
10     CONTINUE
C
C  INCREMENT AND STORE FORECAST HOUR IN LABEL RECORD
C
       HOUR1(1) = IHOUR + 1
       WRITE ( IPO  ) HOUR1
       N7I = 7 *  53
C  WRITE VERT VELS TO DISK (FIRST AND LAST SLAB ARE ZERO)
C
      DO 88890 IQ2W6E = 1,N7I
         WO(IQ2W6E,1) = 0.0E0
88890 CONTINUE
       WRITE ( IWO ) WO
       DO 11 J = 2,44
       DO 12 K = 1,7
       DO 12 I = 2,52
       WO(I,K) = IVVEL(I,J,K) * 1.E-3
12     CONTINUE
       WRITE (IWO) WO
11     CONTINUE
      DO 88900 IQ2W6E = 1,N7I
         WO(IQ2W6E,1) = 0.0E0
88900 CONTINUE
       WRITE (IWO) WO
C
C SIGMA STRIPS CONTAIN VALUES AT TAU AND TAU-1 PLUS ALL FIXED FIELDS
C USE ARYTOT TO BUILD THE STRIPS AND THEN WRITE THEM TO IPO
C
       LTT  = LOLD
       LOLD = LMID
       LMID = LNEW
       DO 20 J = 1, 45
         CALL ARYTOT(J,BUFF)
         WRITE (IPO) BUFF
20     CONTINUE
       LMID = LOLD
       LOLD = LTT
       RETURN
       END
