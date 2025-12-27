      SUBROUTINE SATM
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: SATM           COMPUTES MOISTURE SATURATION VALUES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 20 JUN 83
C
C ABSTRACT: CALCULATES SATURATION PRECIPITAL WATER VALUES WHICH
C   ARE SCALED BY SATRH.
C
C USAGE:  CALL SATM
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LNEW..      TIME INDEX                                /INDEX/
C     TS          POT. TEMPERATURE (K)                      /DEABLK/
C     PI          EXNER FUNCTION (P/1000)**R/CP BAR BP      /GEOPOT/
C     PSIG        DP/D(SIGMA)  (MB)                         /DEABLK/
C     SATRH       SATURATION CRITERION (PERCENT X .01)      /CNST/
C     WS          PRECIPITAL WATER                          /DEABLK/
C     AX          TABLE OF SATURATION VALUES                /FASTER/
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SATW        SATURATION PRECIPITAL WATER (CM)          /DEABLK/
C     WS          PRECIPITAL WATER  (CM)                    /DEABLK/
C
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     MAX,MIN                                               FORTLIB
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
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
      COMMON /DEABLK/ US( 53 , 45 , 21 ),
     A     VS( 53 , 45 , 21 ),
     1     TS( 53 , 45 , 21 ),PSIG( 53 , 45 , 6 ),
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
      COMMON /PBNDPE/ ALARGE( 53 ,12,23), BLARGE( 12, 33 ,23 )
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33, 23 )
      COMMON /CNST/ BTHICK,BTHIK1,BTHK3,DT,RDELX,
     1              DTDS0,DTDS1,DTDS2,DTDX,DS1DX,DS2DX,
     2              CP,R,ROCP,TSTRAT,CPTS,SATRH,RSAT60,
     3              RDT,BTHICH,BTHK3H,BTK398,RBT,A1BRN,
     4              A2BRN,TDTDX4,RPK,VVCNST
      COMMON /DIAG/ SUMK(8),SUMP1(8),SUMP2(8),SUMS(8),SUMF(8),SUMFD(8),
     1     SUMVOR(8),SUMDIV(8),DP(8),SUMP(8),SUMTS(8),SUMZ(8),
     2     SIGB1,SIGT1,SIGT2,SIGS1,SIGS2,RMSSFC,RMSTRP
      COMMON /FASTER/ PIE(1100),AX(120)
      COMMON /GEOPOT/ PHI( 53, 45, 7 ), PI( 53, 45, 7 )
      COMMON /VTEMP/  SIGDOT( 53, 45, 5 ), VT( 53, 45, 25 )
      SAVE
C
C--------------------------------------------------------------------
C  USE VT SCRATCH AS FOLLOWS:
C       VT(1)  -   T(1)
C       VT(2)  -   T(2)
C       VT(3)  -   T(3)
C       VT(4)  -   P(1)
C       VT(5)  -   P(2)
C       VT(6)  -   P(3)
C--------------------------------------------------------------------
C
      DATA CA/3.765578E0/,CB/2.270245E0/
      DATA BRH/0.3E0/
C
      NJ3 = 3 * 2385
      NJ2 = 2 * 2385
C  CONVERT POT. TEMP. TO TEMPERATURE AND FIND MID LAYER PRESSURES
C
      DO 88890 IQ2W6E=1,NJ3
         VT(IQ2W6E,1,1)=TS(IQ2W6E,1,1+LNEW*7)*PI(IQ2W6E,1,1)-273.16E0
88890 CONTINUE
      DO 88900 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,4)=50.E0+3.E0*PSIG(IQ2W6E,1,K2NEW+1)+3.E0 * 
     *   PSIG(IQ2W6E,1,K2NEW)+BTHICH
         VT(IQ2W6E,1,5)=VT(IQ2W6E,1,4)-BTHICH-.5E0*PSIG(IQ2W6E,1,K2NEW
     *   )
         VT(IQ2W6E,1,6)=VT(IQ2W6E,1,5)-PSIG(IQ2W6E,1,K2NEW)
88900 CONTINUE
C
C     TETENS FORMULA FOR SAT. VAP. PRESSURE
C     NUMBERS FROM MURRAY. (JAM, VOL. 6, NO. 1)
C     FOLLOWED BY STANDARD FORMULA   FOR SPECIFIC HUMIDITY
C     ARGUMENT OF EXPONENTIAL HAS PLUS 10 ADDED TO FORCE IT
C     TO BE POSITIVE
C     MURRAYS NUMBERS TIMES       E**-10 ( = 0.000045 ) TO COMPENSATE
C     T(K) = 6.1078*EXP (17.269*T(K) /(T(K)  + 237.3))
C     T(K) = .622 * T(K)/(P(K) - .375*T(K))
C
C  USE TABLE OF SAT. VALUES WHICH ARE STORED IN AX
C
      DO 9012 K = 1,3
      DO 9012 J = 1, 45
      DO 9012 I = 1, 53
       KT = VT(I,J,K) + 71.E0
       KT = MAX(KT,1)
       KT = MIN(KT,120)
9012  VT(I,J,K+6) = AX(KT)
      DO 88930 IQ2W6E=1,NJ3
         VT(IQ2W6E,1,1)=CA*VT(IQ2W6E,1,7)/(VT(IQ2W6E,1,4)-CB*VT(IQ2W6E
     *   ,1,7))
88930 CONTINUE
C
C  51.020408 EQUALS 50/0.98 FOR BOUNDARY LAYER THICKNESS SCALING
C
      DO 88940 IQ2W6E=1,NIJ
         SATW(IQ2W6E,1,1)=SATRH*VT(IQ2W6E,1,1)*51.020408E0
         VT(IQ2W6E,1,7)=PSIG(IQ2W6E,1,K2NEW)/0.98E0
         VT(IQ2W6E,1,8)=VT(IQ2W6E,1,7)
88940 CONTINUE
      DO 88970 IQ2W6E=1,NJ2
         SATW(IQ2W6E,1,2)=SATRH*VT(IQ2W6E,1,2)*VT(IQ2W6E,1,7)
88970 CONTINUE
      DO 299 J = 2,44
      DO 299 I = 2,52
C     DONT ALLOW REL HUM BELOW BRH PERCENT IN BOUNDARY LAYER
C      BUT ONLY OVER OPEN WATER
      IF (XM(I,J).EQ.0.E0) GO TO 299
      WS(I,J,K3NEW) = MAX(WS(I,J,K3NEW) , BRH * SATW(I,J,1))
      WS(I,J,K3MID) = MAX(WS(I,J,K3MID) , BRH * SATW(I,J,1))
  299 CONTINUE
      RETURN
      END
