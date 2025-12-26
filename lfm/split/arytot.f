      SUBROUTINE ARYTOT(JROW,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: ARYTOT         CREATES SIGMA STRIPS FOR OUTPUT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: CONVERTS MODEL COMMON BLOCK (DEABLK) FORMAT TO SIGMA
C   STRIPS (ONE ROW AT A TIME) AS REQUIRED BY THE POST PROCESSOR.
C   THE SIGMA STRIPS CONTAIN MODEL OUTPUT AT TAU AND TAU-1 TIME
C   LEVELS PLUS ALL OF THE FIXED FIELDS.
C
C USAGE:  CALL ARYTOT (JROW,A)
C
C
C  - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JROW        ROW INDEX                                 ARGUMENT
C     LI,LJ--     DOMAIN SIZES AND TIME INDICIES            /INDEX/
C     US,VS--     MODEL FORECAST VARIABLES                  /DEABLK/
C                 AND FIXED FIELDS
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A           SIGMA STRIP BLOCK FOR POST PROCESSOR      ARGUMENT
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
      REAL        A( 3816 )
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
      COMMON /PBNDPE/ ALARGE( 53 ,12, 23 ), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      COMMON /DEABLK/ US( 53, 45, 21 ),
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
      SAVE
C--------------------------------------------------------------------
C  SET UP POINTERS FOR THE BLOCK OF SIGMA STRIPS
C--------------------------------------------------------------------
      N0  =  1
      N1  =  53 * 36 + 1
      ND  =  53 * 9
      N2  =  N1 + ND
      N3  =  N2 + ND
      N4  =  N3 + ND
      N5  =  53 * 28
      N6  =  53 * 8
      N7  =  53
      N8  =  53 * 12
      N9  =  ND +  1
      N10 =  53 * 18 + 1
      N11 =  53 * 27 + 1
      N12 =  53 *  7 + 1
      N13 =  53 *  8 + 1
      N14 =  53 * 43 + 1
      N15 =  53 * 16 + 1
      N16 =  53 * 17 + 1
      N17 =  53 * 26 + 1
      N18 =  53 * 30 + 1
      N19 =  53 * 33 + 1
      N20 =  53 * 69 + 1
      N21 =  53 * 25 + 1
      N22 =  53 * 29 + 1
      N23 =  53 * 44 + 1
      N24 =  53 * 65 + 1
      N25 =  53 * 52 + 1
C-------------------------------------------------------------------
C  MOVE DATA FROM MODEL FORECAST FIELDS TO A
C-------------------------------------------------------------------
      DO 10 K = 1, 7
      MOLD  = K + LOLD *  7
      MMID  = K + LMID *  7
      NPLUS = (K-1) *  53
      DO 88890 IQ2W6E=1,N7
         A(N0+NPLUS+IQ2W6E-1)=US(IQ2W6E,JROW,MMID)
         A(N1+NPLUS+IQ2W6E-1)=US(IQ2W6E,JROW,MOLD)
         A(N9+NPLUS+IQ2W6E-1)=VS(IQ2W6E,JROW,MMID)
         A(N2+NPLUS+IQ2W6E-1)=VS(IQ2W6E,JROW,MOLD)
         A(N10+NPLUS+IQ2W6E-1)=TS(IQ2W6E,JROW,MMID)
         A(N3+NPLUS+IQ2W6E-1)=TS(IQ2W6E,JROW,MOLD)
88890 CONTINUE
      IF (K .GT. 3) GO TO 10
      MOLD = K + LOLD * 3
      MMID = K + LMID * 3
      DO 88950 IQ2W6E=1,N7
         A(N19+NPLUS+IQ2W6E-1)=WS(IQ2W6E,JROW,MMID)
         A(N20+NPLUS+IQ2W6E-1)=WS(IQ2W6E,JROW,MOLD)
         A(N18+NPLUS+IQ2W6E-1)=SATW(IQ2W6E,JROW,K)
88950 CONTINUE
      IF (K .GT. 2) GO TO 10
      MOLD = K + LOLD * 2
      MMID = K + LMID * 2
      DO 88980 IQ2W6E=1,N7
         A(N11+NPLUS+IQ2W6E-1)=PSIG(IQ2W6E,JROW,MMID)
         A(N4+NPLUS+IQ2W6E-1)=PSIG(IQ2W6E,JROW,MOLD)
88980 CONTINUE
10    CONTINUE
      DO 89000 IQ2W6E=1,N7
         A(N12+IQ2W6E-1) = ST(IQ2W6E,JROW)
         A(N13+IQ2W6E-1) = ICE(IQ2W6E,JROW)
         A(N14+IQ2W6E-1) = COSZEN(IQ2W6E,JROW)
         A(N15+IQ2W6E-1) = XM(IQ2W6E,JROW)
         A(N16+IQ2W6E-1) = F(IQ2W6E,JROW)
         A(N17+IQ2W6E-1) = CD(IQ2W6E,JROW)
         A(N21+IQ2W6E-1) = PREC(IQ2W6E,JROW)
         A(N22+IQ2W6E-1) = ZSTAR(IQ2W6E,JROW)
         A(N23+IQ2W6E-1) = ALON(IQ2W6E,JROW)
         A(N24+IQ2W6E-1) = ALAT(IQ2W6E,JROW)
         A(N25+IQ2W6E-1) = MF(IQ2W6E,JROW)
89000 CONTINUE
      RETURN
      END
