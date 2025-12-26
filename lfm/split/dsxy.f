      SUBROUTINE DSXY(SXY,A)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: DSXY           FORTH ORDER BAR
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 18 AUG 83
C
C ABSTRACT: COMPUTES BAR XY (FORTH ORDER) OF ANY ARRAY A AND STORES
C   THE RESULTS IN SXY.
C
C USAGE:  CALL DSXY(SXY,A)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     LI,LJ..     POINTERS FOR SIZE OF GRID A AND SXY       /INDEX/
C     A           ANY (LIXLJ) ARRAY                         ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SXY         RESULT OF A BAR XY                        ARGUMENT
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
      REAL        SXY(*), A(*)
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
      DATA C81/3.1640625E-1/,  C9/-3.515625E-2/,
     A     C1 /3.90625E-3/
C
C----------  FIRST DO ALL INTERIOR POINTS FORTH ORDER
         ISTART = LI + 2
         IEND  = LI * (LJ-2) - 2
         DO 10 I = ISTART,IEND
         SXY(I) = C81*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1)) +
     A            C9 *(A(I+LI-1)+A(I+LI+2)+A(I-1)+A(I+2)+A(I+2*LI) +
     B                 A(I+2*LI+1)+A(I-LI)+A(I-LI+1)) +
     C            C1 *(A(I+2*LI-1)+A(I+2*LI+2)+A(I-LI-1)+A(I-LI+2))
10       CONTINUE
C
C------ NOW THE EDGES OF THE GRID
C
C-----  LEFT EDGE FIRST
C
         IEND = IEND + 3
         DO 20 I = 1,IEND, 53
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
20       CONTINUE
C------ RIGHT EDGE HERE
         IEND = IEND + LI - 2
         DO 30 I = LI1,IEND, 53
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
30       CONTINUE
C----- BOTTOM ROW OF THE GRID
         DO 40 I = 1,LI1
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
40       CONTINUE
C------ TOP ROW OF THE GRID
         ISTART = LI * (LJ-2) + 1
         IEND   = ISTART +  LI - 2
         DO 50 I = ISTART,IEND
           SXY(I) = 0.25E0*(A(I)+A(I+1)+A(I+LI)+A(I+LI+1))
50       CONTINUE
         RETURN
       END
