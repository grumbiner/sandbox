      SUBROUTINE TTOARY(JROW,A)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: TTOARY         SIGMA STRIPS TO MODEL FORMAT
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 23 JUN 83
C
C ABSTRACT: MOVES ALL MODEL VARIABLES FROM SIGMA STRIP BUFFER
C   TO COMMON BLOCK. INCLUDES TAU-1 AND TAU-1 TIME LEVELS PLUS
C   ALL FIXED FIELDS.
C
C USAGE:  CALL TTOARY(JROW,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JROW        ROW INDEX FOR SIGMA STRIP BUFFER          ARGUMENT
C     A           SIGMA STRIP BUFFER (ONE ROW)              ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     US,VS..     FORECAST VARIABLES PLUS FIXED FIELDS      /DEABLK/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
C
      IMPLICIT REAL (A-H,O-Z)
      REAL A( 3816 )
      REAL ICE, MF
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
      DO 10 K = 1, 7
      KOLD = K
      KMID = K +  7
      NPLUS = (K-1) *  53
      DO 88890 IQ2W6E=1,N7
         US(IQ2W6E,JROW,KMID)=A(NPLUS+IQ2W6E)
         US(IQ2W6E,JROW,KOLD)=A(N1+NPLUS+IQ2W6E-1)
         VS(IQ2W6E,JROW,KMID)=A(N9+NPLUS+IQ2W6E-1)
         VS(IQ2W6E,JROW,KOLD)=A(N2+NPLUS+IQ2W6E-1)
         TS(IQ2W6E,JROW,KMID)=A(N10+NPLUS+IQ2W6E-1)
         TS(IQ2W6E,JROW,KOLD)=A(N3+NPLUS+IQ2W6E-1)
88890 CONTINUE
      IF (K .GT. 3) GO TO 10
      KMID = K + 3
      DO 88950 IQ2W6E=1,N7
         WS(IQ2W6E,JROW,KMID)=A(N19+NPLUS+IQ2W6E-1)
         WS(IQ2W6E,JROW,KOLD)=A(N20+NPLUS+IQ2W6E-1)
         SATW(IQ2W6E,JROW,K)=A(N18+NPLUS+IQ2W6E-1)
88950 CONTINUE
      IF (K .GT. 2) GO TO 10
      KMID = K + 2
      DO 88980 IQ2W6E=1,N7
         PSIG(IQ2W6E,JROW,KMID)=A(N11+NPLUS+IQ2W6E-1)
         PSIG(IQ2W6E,JROW,KOLD)=A(N4+NPLUS+IQ2W6E-1)
88980 CONTINUE
10    CONTINUE
      DO 89000 IQ2W6E=1,N7
         ST(IQ2W6E,JROW)=A(N12+IQ2W6E-1)
         ICE(IQ2W6E,JROW)=A(N13+IQ2W6E-1)
         COSZEN(IQ2W6E,JROW)=A(N14+IQ2W6E-1)
         XM(IQ2W6E,JROW)=A(N15+IQ2W6E-1)
         F(IQ2W6E,JROW)=A(N16+IQ2W6E-1)
         CD(IQ2W6E,JROW)=A(N17+IQ2W6E-1)
         PREC(IQ2W6E,JROW)=A(N21+IQ2W6E-1)
         ZSTAR(IQ2W6E,JROW)=A(N22+IQ2W6E-1)
         ALON(IQ2W6E,JROW)=A(N23+IQ2W6E-1)
         ALAT(IQ2W6E,JROW)=A(N24+IQ2W6E-1)
         MF(IQ2W6E,JROW)=A(N25+IQ2W6E-1)
89000 CONTINUE
      RETURN
      END
