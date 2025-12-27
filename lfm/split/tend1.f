       SUBROUTINE TEND1(PHI,PI)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TEND1          COMPUTES GEOPOTENTIAL AND PI MID-LAYER
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES GEOPOTENTIAL AND PI (P/1000)**R/CP AT THE
C   MIDPOINT OF EACH LAYER FOR THE TAU TIME LEVEL.  VALUES ARE
C   CALCULATED USING THE BROWN-PHILLIPS VERSION OF THE MID-LAYER
C   AVERAGE.  DOCUMENTED IN NMC OFFICE NOTES 92 AND 104.
C
C USAGE:  CALL TEND1(PHI,PI)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     BTHICK      DP/D(SIGMA) IN BOUNDARY LAYER (50 MB)     /CNST/
C     PSIG        DP/D(SIGMA) TROP & STRAT  (MB)            /DEABLK/
C     K2MID..     LAYER INDEX                               /INDEX/
C     RPK         1 /( 1 + R/CP) FOR PIBAR BP               /CNST/
C     TS          POT. TEMPERATURE ARRAY  (K)               /DEABLK/
C     ZSTAR       TERRAIN HEIGHT (METERS)                   /DEABLK/
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     PHI         GEOPOTENTIAL AT MID LAYERS (M**2/SEC**2)  ARGUMENT
C     PI          EXNER FUNCTION    DITTO                   ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     WKXLOX                                                LFM LIB
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
      REAL        PHI( 53 , 45 ,7 ), PI( 53 , 45 ,7 )
      REAL        SIGT(5),SIGS(8)
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
      COMMON /PBNDPE/ ALARGE( 53 ,12,23), BLARGE( 12, 33 ,23 )
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
      COMMON /VERTV/ IVVEL( 53 , 45 , 8 )
      COMMON /VTEMP/ SIGDOT( 53 , 45 ,5 ), VT( 53, 45, 25 )
      SAVE
C
      DATA SIGT  /1.E0,1.E0,0.66667E0,0.33333E0,0.E0/,
     A     SIGS  /4*0.E0,1.E0,0.66667E0,0.33333E0,0.E0/
C ---------------------------------------------------------------------
C   FORM PRESSURE VALUES IN PI(1,1,1 - 7)
C ---------------------------------------------------------------------
      DO 88890 IQ2W6E = 1,NIJ
         PI(IQ2W6E,1,1) = 50.E0 + BTHICK + 3.E0 * PSIG(IQ2W6E,1,K2MID)
     *   + 3.E0 * PSIG(IQ2W6E,1,K2MID+1)
         PI(IQ2W6E,1,2) = PI(IQ2W6E,1,1)-BTHICK
         PI(IQ2W6E,1,3) = PI(IQ2W6E,1,2)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,4) = PI(IQ2W6E,1,3)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,5) = PI(IQ2W6E,1,4)-PSIG(IQ2W6E,1,K2MID)
         PI(IQ2W6E,1,6) = PI(IQ2W6E,1,5)-PSIG(IQ2W6E,1,K2MID+1)
         PI(IQ2W6E,1,7) = PI(IQ2W6E,1,6)-PSIG(IQ2W6E,1,K2MID+1)
88890 CONTINUE
C ---------------------------------------------------------------------
C    NOW CHANGE P TO PI FOR ALL SEVEN INTERFACES
C ---------------------------------------------------------------------
       DO 2345 L = 1, 7
         CALL WKXLOX(VT(1,1,L),PI(1,1,L),L)
2345   CONTINUE
C ---------------------------------------------------------------------
C    CHANGE SIX INTERFACE VALUES OF PI TO PI BAR BP (LAYER)
C ---------------------------------------------------------------------
       DO 2 KK = 1,6
       DO 2 J = 1, 45
       DO 2 I = 1, 53
       PI(I,J,KK) = ((PI(I,J,KK)*VT(I,J,KK) - PI(I,J,KK+1)*VT(I,J,KK+1))
     A              /(PI(I,J,KK) - PI(I,J,KK+1))) * RPK
2      CONTINUE
C ---------------------------------------------------------------------
C    NOW FINISH THE TOP LAYER (7) PI BAR BP
C ---------------------------------------------------------------------
       PPITOP = 50.E0*.05**REAL(ROCP)
       DO 3 J = 1, 45
       DO 3 I = 1, 53
       PI(I,J,7) = ((PI(I,J,7)*VT(I,J,7) - PPITOP)
     A             /(PI(I,J,7)-50.E0 )) * RPK
3      CONTINUE
C ---------------------------------------------------------------------
C    READY TO CALCULATE GZ FOR THE SEVEN LAYERS TROPOSPHERE FIRST
C    FIRST FORM PRELIMINARY QUANTITIES IN VT
C----------------------------------------------------------------------
       KTT = 4
       KTTM = KTT - 1
       DO 20 L = 1,KTTM
      DO 88960 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,L+8)=((TS(IQ2W6E,1,K7MID+L-1)+TS(IQ2W6E,1,K7MID+L
     *   ))*.5E0)*(PI(IQ2W6E,1,L)-PI(IQ2W6E,1,L+1))
         VT(IQ2W6E,1,L+15)=(TS(IQ2W6E,1,K7MID+L-1)-TS(IQ2W6E,1,K7MID+L
     *   ))*((PI(IQ2W6E,1,L)+PI(IQ2W6E,1,L+1))*0.5E0-VT(IQ2W6E,1,L+1))
88960 CONTINUE
20     CONTINUE
C
C$           1.2  THICKNESS - SURFACE TO MID LAYER AND IN LAYERS (/CP)
C
      DO 88980 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=TS(IQ2W6E,1,K7MID)*(VT(IQ2W6E,1,1)-PI(IQ2W6E,
     *   1,1))
         VT(IQ2W6E,1,23)=VT(IQ2W6E,1,22)
88980 CONTINUE
       DO 50 L = 2,KTT
      DO 89000 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+SIGT(L)*VT(IQ2W6E,1,L+14)
89000 CONTINUE
50     CONTINUE
C
C$           1.3  GET THE TROP LAYER HEIGHTS
C
       DO 60 J = 1, 45
       DO 60 I = 1, 53
       PHI(I,J,1) = ZSTAR(I,J)*9.8E0+VT(I,J,22) * CP
60     CONTINUE
       DO 80 L = 1,KTTM
      DO 89010 IQ2W6E=1,NIJ
         PHI(IQ2W6E,1,L+1)=PHI(IQ2W6E,1,L)+CP*VT(IQ2W6E,1,L+8)
89010 CONTINUE
80     CONTINUE
C
C$           2.  STRATOSPHERE
C$           2.1  PRELIMINARY COMPUTATIONS
C
       KSB  = 5
       KST  = 7
       KSTM = KST - 1
       KSBP = KSB + 1
       DO 120 L = KSB,KSTM
      DO 89020 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,L+8)=((TS(IQ2W6E,1,K7MID+L-1)+TS(IQ2W6E,1,K7MID+L
     *   ))*0.5E0)*(PI(IQ2W6E,1,L)-PI(IQ2W6E,1,L+1))
         VT(IQ2W6E,1,L+15)=(TS(IQ2W6E,1,K7MID+L-1)-TS(IQ2W6E,1,K7MID+L
     *   ))*((PI(IQ2W6E,1,L)+PI(IQ2W6E,1,L+1))*0.5E0-VT(IQ2W6E,1,L+1))
89020 CONTINUE
120    CONTINUE
C
C$           2.2  THICKNESS ACROSS TROPOPAUSE AND IN LAYERS (/CP)
C
      DO 89040 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,23)+TS(IQ2W6E,1,K7MID+KTT-1)*(PI(
     *   IQ2W6E,1,KTT)-VT(IQ2W6E,1,KSB))+TS(IQ2W6E,1,K7MID+KSB-1)*(VT(
     *   IQ2W6E,1,KSB)-PI(IQ2W6E,1,KSB))-VT(IQ2W6E,1,22)
89040 CONTINUE
       DO 150 L = 2,KTT
      DO 89050 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+VT(IQ2W6E,1,L+14)
89050 CONTINUE
150    CONTINUE
       DO 170 L = KSBP,KST
      DO 89060 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,22)=VT(IQ2W6E,1,22)+SIGS(L)*VT(IQ2W6E,1,L+14)
89060 CONTINUE
170    CONTINUE
C
C$           2.3  MID LAYER HEIGHTS
C
       DO 180 J = 1, 45
       DO 180 I = 1, 53
       PHI(I,J,KSB) = PHI(I,J,KTT) + VT(I,J,22) * CP
180    CONTINUE
       DO 200 L = KSB,KSTM
      DO 89070 IQ2W6E=1,NIJ
         PHI(IQ2W6E,1,L+1)=PHI(IQ2W6E,1,L)+CP*VT(IQ2W6E,1,L+8)
89070 CONTINUE
200    CONTINUE
C
C$           3.  RETURN
C
       RETURN
       END
