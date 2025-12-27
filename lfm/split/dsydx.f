      SUBROUTINE DSYDX(SYDX,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSYDX          COMPUTES D(A)/DX BAR Y
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 17 JUN 83
C
C ABSTRACT: COMPUTES D(A)/DX BAR Y (FORTH ORDER) AND STORES THE
C   RESULTS IN SYDX. NO SCALING FOR DX.
C
C USAGE:  CALL DSYDX(SYDX,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF THE GRID             /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SYDX        RESULT OF D(A)/DX BAR Y                   ARGUMENT
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
      REAL        SYDX(*),A(*)
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
      COMMON /PBNDPE/ ALARGE( 53, 12, 23), BLARGE( 12, 33, 23 )
      COMMON /PBNDRY/ AA( 53, 12, 23 ), BB( 12, 33, 23 )
      SAVE
C
      DATA C243/0.6328125E0/, C27/-7.03125E-2/, C9/-2.34375E-2/,
     *     C1  /2.6041667E-3/
C
C------ALL INTERIOR POINTS (FOURTH ORDER) FOLLOWS
      ISTART = LI + 2
      IEND = LI*(LJ-2) - 2
      DO 10 I = ISTART,IEND
        SYDX(I) = C243*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))  +
     *            C27 *(A(I-LI+1)-A(I-LI)+A(I+2*LI+1)-A(I+2*LI))  +
     *            C9  *(A(I+2)-A(I-1)+A(I+LI+2)-A(I+LI-1))  +
     *            C1  *(A(I-LI+2)-A(I-LI-1)+A(I+2*LI+2)-A(I+2*LI-1))
10    CONTINUE
C______NOW THE EDGES OF THE GRID SECOND ORDER
C------LEFT EDGE FIRST
      IEND = IEND + 3
      DO 20 I = 1,IEND,  53
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
20    CONTINUE
C------RIGHT EDGE OF GRID FOLLOWS
      IEND = IEND + LI -2
      DO 30 I = LI1,IEND,  53
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
30    CONTINUE
C------BOTTOM ROW OF GRID FOLLOWS
      DO 40 I = 1,LI
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
40    CONTINUE
C------AND FINALLY THE TOP ROW OF THE GRID
      ISTART = LI*(LJ-2) + 1
      IEND   = ISTART + LI - 2
      DO 50 I = ISTART,IEND
        SYDX(I) = .5E0*(A(I+1)-A(I)+A(I+LI+1)-A(I+LI))
50    CONTINUE
      RETURN
      END
