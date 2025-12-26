      SUBROUTINE BLDBND(L,BUFF)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: BLDBND         CONSTRUCT BOUNDARY VALUES
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: MOVES INITIAL VALUES FROM BUFF (SIGMA STRIPS) TO
C   ALARGE AND BLARGE. ALARGE CONTAINS THE LOWER SIX ROWS OF THE
C   LFM GRID,AND THE TOP SIX ROWS AS FOLLOWS: ALARGE(I,J,K) I= 1 TO
C    53 , J= 1 TO 12, WHERE 1-6 ARE THE LOWER SIX J ROWS AND 7 - 12
C   CONTAIN THE TOP SIX J ROWS, K = 1 TO 23, US(1-7), VS(8-14),
C   TS(15-21), AND PSIG(22-23).
C   BLARGE CONTAINS THE REMAINING SIDE ROWS AS FOLLOWS: BLARGE(I,J,K)
C   I= 1 TO 12, WHERE 1-6 ARE THE LEFT SIDE OF THE GRID AND 7-12
C   CONTAIN THE RIGHT SIDE OF THE GRID,J = 1 TO JMAX-12, CORRESPONDING
C   TO JROWS 7 THROUGH JMAX-6, K = 1 TO 23 SAME AS USE IN ALARGE.
C
C   *****************************************************************
C   *                                                               *
C   *               ALARGE I= 1 , 53                                *
C   *                      J= 7 , 12                                *
C   *                                                               *
C   *****************************************************************
C   *            *                                      *           *
C   *  BLARGE    *                                      *  BLARGE   *
C   *            *            INTERIOR NO BOUNDARY      *           *
C   *  I= 1, 6   *            NUDGE                     *  I= 7,12  *
C   *  J= 7,40   *                                      *  J= 7,40  *
C   *            *                                      *           *
C   *            *                                      *           *
C   *            *                                      *           *
C   *****************************************************************
C   *                                                               *
C   *               ALARGE I= 1 , 53                                *
C   *                      J= 1 , 6                                 *
C   *                                                               *
C   *****************************************************************
C
C USAGE:  CALL BLDBND(L,BUFF)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     L           JROW COUNTER                              ARGUMENT
C     BUFF        SIGMA STRIP BUFFER FROM INPUT DISK FILE   ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ALARGE      BOUNDARY VALUES AS DESCRIBED ABOVE        /PBNDPE/
C     BLARGE                 DITTO                          /PBNDPE/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C
C$$$
C
      IMPLICIT    REAL (A-H,O-Z)
      REAL        BUFF( 3816 )
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
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
C  SET UP POINTERS FOR THE SIGMA STRIPS STORED IN BUFF
C
      N1 = 53 * 36 + 1
      ND = 53 * 9
      N2 = N1 + ND
      N3 = N2 + ND
      N4 = N3 + ND
C
C MOVE VALUES FROM BUFF INTO ALARGE AND BLARGE
C
      IF(L.LT.7.OR.L.GT.  45 -6 )GO TO 10
      J = L - 6
      DO 1 K = 1,7
      KIND =  53 * (K-1)
      IBU  =  N1 + KIND
      DO 88890 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K)=BUFF(IBU+IQ2W6E-1)
88890 CONTINUE
      IBU=IBU+  53 -6
      DO 88900 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K)=BUFF(IBU+IQ2W6E-1)
88900 CONTINUE
      IBV= N2 +KIND
      DO 88910 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+7)=BUFF(IBV+IQ2W6E-1)
88910 CONTINUE
      IBV = IBV + 53 - 6
      DO 88920 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+7)=BUFF(IBV+IQ2W6E-1)
88920 CONTINUE
      IBT = N3 + KIND
      DO 88930 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+14)=BUFF(IBT+IQ2W6E-1)
88930 CONTINUE
      IBT = IBT + 53 - 6
      DO 88940 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+14)=BUFF(IBT+IQ2W6E-1)
88940 CONTINUE
      IF (K.GT.2) GO TO 1
      IBP = N4 + KIND
      DO 88950 IQ2W6E=1,6
         BLARGE(IQ2W6E,J,K+21)=BUFF(IBP+IQ2W6E-1)
88950 CONTINUE
      IBP = IBP + 53 - 6
      DO 88960 IQ2W6E=1,6
         BLARGE(IQ2W6E+6,J,K+21)=BUFF(IBP+IQ2W6E-1)
88960 CONTINUE
1     CONTINUE
      GO TO 20
10    J = L
      N53 =  53
      DO 15 K = 1,7
      NPLUS = (K-1) *  53
      IF (L.GT.45-6) J = L- 45 + 12
      DO 88970 IQ2W6E=1,N53
         ALARGE(IQ2W6E,J,K)=BUFF(N1+NPLUS+IQ2W6E-1)
         ALARGE(IQ2W6E,J,K+7)=BUFF(N2+NPLUS+IQ2W6E-1)
         ALARGE(IQ2W6E,J,K+14)=BUFF(N3+NPLUS+IQ2W6E-1)
88970 CONTINUE
      IF ( K.GT.2 ) GO TO 15
      DO 89000 IQ2W6E=1,N53
         ALARGE(IQ2W6E,J,K+21)=BUFF(N4+NPLUS+IQ2W6E-1)
89000 CONTINUE
15    CONTINUE
20    RETURN
      END
