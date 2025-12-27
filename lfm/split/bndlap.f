       SUBROUTINE BNDLAP(A,B,C)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: BNDLAP         COMPUTES LATERAL BOUNDARY CONDITIONS
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 22 JUN 83
C
C ABSTRACT: COMPUTES LATERAL BOUNDARY REGION LAPLACIAN (DIFFUSIVE
C   NUDGE) AND RESTORES PRIMETER VALUES.
C
C USAGE:  CALL BNDLAP(A,B,C)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     A           ONE LAYER OF BOUNDARY VALUES              ARGUMENT
C                 (TOP AND BOTTOM ROWS OF GRID)
C     B           ONE LAYER OF BOUNDARY VALUES (SIDES OF GRID) ARG
C     C           ANY FORECAST VARIABLE (ONE LAYER)
C     XBND        ARRAY OF DIFFUSION COEFICIENTS            ARGUMENT
C                 (ZERO INTERIOR OF GRID, .04 ELSEWHERE)
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     C           FORECAST VARIABLE WITH B.C. APPLIED       ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     DEL2XY                                                LFMLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT    REAL (A-H,O-Z)
      REAL        A( 53 ,12 ), B( 12, 33 ),C( 53 , 45 )
      REAL        HOUR1
      CHARACTER*4 LABSIN,LABBND,LABS00,LABS06,LABS12,LABV06,LABV12
C
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
      COMMON /PBNDRY/ AA( 53 ,12, 23 ), BB( 12, 33 ,23 )
      COMMON /VTEMP/  SIGDOT( 53 , 45 ,5 ), VT( 53 , 45 ,25 )
      COMMON /BCOEF/  XDIF( 53 , 45 ),XBND( 53 , 45 )
      SAVE
C
C  FIRST FILL VT(16) WITH VALUES FROM A & B AT PROPER LOCATIONS
C
       NIJ =      2385
      DO 88890 IQ2W6E=1,NIJ
         VT(IQ2W6E,1,16)=0.E0
88890 CONTINUE
       N6I = 6 *  53
      DO 88900 IQ2W6E=1,N6I
         VT(IQ2W6E,1,16)=A(IQ2W6E,1)
         VT(IQ2W6E,45-5,16)=A(IQ2W6E,7)
88900 CONTINUE
       JEND =  45  - 6
       DO 1 J = 7,JEND
      DO 88920 IQ2W6E=1,6
         VT(IQ2W6E,J,16)=B(IQ2W6E,J-6)
         VT(IQ2W6E+47,J,16)=B(IQ2W6E+6,J-6)
88920 CONTINUE
1      CONTINUE
       CALL DEL2XY(VT(1,1,17),VT(1,1,16))
       DO 2 J = 1, 45
       DO 2 I = 1, 53
       C(I,J) = C(I,J) - XBND(I,J) * VT(I,J,17)
2      CONTINUE
      DO 88940 IQ2W6E=1,LI
         C(IQ2W6E,1)=A(IQ2W6E,1)
         C(IQ2W6E,45)=A(IQ2W6E,12)
88940 CONTINUE
       DO 3 J = 2,6
         C(1,J)    = A(1,J)
         C( 53 ,J) = A( 53 ,J)
3      CONTINUE
       JST =  45  - 5
       DO 4 J = JST,       44
         C(1,J)    = A(1,J - JST + 7)
         C( 53 ,J) = A( 53 ,J - JST + 7)
4      CONTINUE
       JEND =  45  - 6
       DO 5 J = 7,JEND
         C(1,J)    = B(1,J-6)
         C( 53 ,J) = B(12,J-6)
5      CONTINUE
       RETURN
       END
