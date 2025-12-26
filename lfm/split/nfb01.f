      SUBROUTINE NFB01(XI,XJ,SLAT,CLAT,SLON,CLON)
C$$$  SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: NFB01          FINDS LAT. AND LONG. GIVEN GRID POINT I,J
C   AUTHOR: DENNIS DEAVEN    ORG: W/NMC23    DATE: 21 JUN 83
C
C ABSTRACT: COMPUTES SINE AND COSINE OF LATITUDE AND LONGITUDE GIVEN
C   THE GRID POINT INDEX (FLOATING POINT) ON THE LFM HALF BEDIENT GRID.
C
C USAGE:  CALL NFB01(XI,XJ,SLAT,CLAT,SLON,CLON)
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     XI,XJ       VALUE OF GRID POINT INDEX ON LFM GRID     ARGUMENT
C 
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     SLAT,CLAT   SIN AND COSINE OF LATITUDE                ARGUMENT
C     SLON,CLON       DITTO         LONGITUDE               ARGUMENT
C 
C - - - - - - - - - S U B P R O G R A M S   C A L L E D - - - - - - - -
C     NAME(S)                                               LIBRARY
C     -------                                               -------
C     SIN,COS,ATAN,SQRT                                     FORTLIB
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
C$$$
      IMPLICIT REAL (A-H,O-Z)
      GI    =  62.40784
      XIP   =  27
      XJP   =  49
C     LATITUDE AND LONGITUDE AT GRID POINT XI, XJ
      TERMI = XI - XIP
      TERMJ = XJ - XJP
      GI2   = GI * GI
      R2    = TERMI * TERMI + TERMJ * TERMJ
      IF (TERMJ.EQ.0.0) GO TO 2
      TAN   = TERMI / TERMJ
      TLONG = ATAN(TAN)
      IF (TERMJ.GE.0.0) GO TO 7
        TLONG = TLONG + 3.1415927E0
 7    IF (TLONG.GE.0.0) GO TO 10
        TLONG = TLONG + 6.2831853E0
        GO TO 10
 2    CONTINUE
      IF (TERMI.GE.0.0) GO TO 4
        TLONG = 4.7123890E0
        GO TO 10
 4    CONTINUE
        TLONG = 1.5707963E0
 10   CONTINUE
        SLAT  = (GI2-R2) / (GI2+R2)
        CLAT  = SQRT(1.E0-SLAT*SLAT)
        SLON  = SIN(TLONG)
        CLON  = COS(TLONG)
      RETURN
      END
