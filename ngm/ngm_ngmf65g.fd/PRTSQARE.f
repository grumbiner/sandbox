      SUBROUTINE PRTSQARE (NG, I0, J0)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PRTSQARE    PRINTS H, HU, HV, HTG, HQ AROUND I0,J0
C   PRGMMR: JIM TUCCILLO     ORG: W/NMC4     DATE: 90-06-22
C
C ABSTRACT: PRINTS H, HU, HV, HTH, AND HQ FOR GRID POINTS
C   SURROUNDING POINT (I0, J0) FOR ALL LEVELS OF THE FORECAST MODEL.
C
C PROGRAM HISTORY LOG:
C   90-06-22  JIM TUCCILLO
C
C USAGE:    CALL PRTSQARE (NG, I0, J0)
C   INPUT ARGUMENT LIST:
C     NG       - GRID NUMBER
C     I0       - FIRST INDICE OF GRID POINT AROUND WHICH THE VARIABLES
C                ARE PRINTED
C     J0       - SECOND INDICE OF GRID POINT AROUND WHICH THE VARIABLES
C                ARE PRINTED
C
C   OUTPUT FILES:
C     FT06F001 - FOR PRINTOUT
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - COMCONST
C                  COMBLANK
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
C     COMMON BLOCK /COMCONST/ CONTAINS GRID-RELATED PARAMETERS,
C          AND A FEW OTHER COMMON CONSTANTS.
      COMMON /COMCONST/ IJMAX, KM, LVLTOT, IJRAD, LOFCLDS(2,4),
     1                  ICALLRAD, IPHYSPL, NGRDUSE, NH,
     2                  NTIME, ITIME, NSTEPS,
     3                  IMG(INGRDUSE), JMG(INGRDUSE),
     4                  IAG(INGRDUS1), JAG(INGRDUS1),
     5                  IBG(INGRDUS1), JBG(INGRDUS1),
     6                  IADDRG(INIADDRS, INGRDUSE),
     7                  NPTSFH(2, INGRDUSE),
     8                  KUMULUS, LGRIDPPT, KLIFT1, KLIFT2, IBUCKET,
     9                  XPOLEH(INGRDUSE), YPOLEH(INGRDUSE), RADIUS,
     1                  DELSIG(IKM), PR(IKM), PRESS(IKM),
     2                  SIGINT(IKMP1),
     3                  DTOVDX, ANGVEL,
     4                  SIGMACC, SIGMAGSP, SIGMADHQ, CRITCONV,
     5                  SATDEL, RHFACTOR, QBOUND,
     6                  ANEM, BLKDR, CHARN, CONAUST, DDORF, PKATO,
     7                  SCALEHT, SIGDOT, DLAMNGM
C
      COMMON            SCR     (IIJMAX,  INSCR),
     1                  SCRGEOG (IIJMAX,  INSCRGEO),
     2                  SCR3    (IIJKMAX, INSCR3),
     3                  FILLER  (INFILLER),
     4                  VBL     (INVBL),
     5                  BITGRDH (IIJMAX, 2, INGRDUSE),
     6                  BITGRDU (IIJMAX, 2, INGRDUSE),
     7                  BITGRDV (IIJMAX, 2, INGRDUSE),
     8                  BITSEA  (IIJMAX, INGRDUSE),
     9                  BITSNO  (IIJMAX, INGRDUSE),
     1                  BITWVL  (IIJMAX, INGRDUSE)
C
      LOGICAL BITGRDH, BITGRDU, BITGRDV, BITSEA, BITSNO, BITWVL
C
      INDEXHU  = 1
      INDEXHV  = 2
      INDEXHTH = 3
      INDEXHQ  = 4
      INDEXH   = 5
C
      IM = IMG(NG)
      JM = JMG(NG)
      IJ = IM * JM
C
C              ENSURE THAT ONLY ACTUAL GRID-POINT VALUES ARE PRINTED.
      I00 = I0
      J00 = J0
C
      IF (I0 .LE. 2) THEN
         I0 = 3
      END IF
C
      IF (I0 .GE. IM-1) THEN
         I0 = IM-2
      END IF
C
      IF (J0 .LE. 2) THEN
         J0 = 3
      END IF
C
      IF (J0 .GE. JM-1) THEN
         J0 = JM-2
      END IF
C
C              FIRST PRINT THE H FIELD.
      I1 = I0 + (J0-1)*IM
     1     + IADDRG(INDEXH, NG) - 1
     2     + 2*IM - 2
C
      I2 = I1 + 4
C
      WRITE (6, 100) NG, I0, J0, NSTEPS
  100 FORMAT ('0', 'THE H FIELD ON GRID',I2,' AROUND (I0, J0) = (', I2,
     1             ',', I2, ') FOR NSTEPS=', I4, ':' )
C
      DO 300 J=1,5
C
         WRITE (6, 200) (VBL(I), I=I1, I2)
  200    FORMAT (' ', 5F6.3)
C
         I1 = I1 - IM
         I2 = I1 + 4
C
  300 CONTINUE
C
C              THE HU FIELD.
      I1 = I0 + (J0-1)*IM
     1     + IADDRG(INDEXHU, NG) - 1
     2     + 2*IM - 2
      I2 = I1 + 4
C
      WRITE (6, 400) NG, I0, J0, NSTEPS
  400 FORMAT ('0', 'THE HU FIELD ON GRID',I2,' AROUND (I0, J0) = (',I2,
     1             ',', I2, ') FOR NSTEPS=', I4, ':' )
C
      DO 700 K=1, KM, 4
C
         WRITE (6, 450)
  450    FORMAT (' ')
C
         DO 600 J=1,5
C
            WRITE (6, 500) (VBL(IA), IA=I1,      I2     ),
     1                     (VBL(IB), IB=I1+  IJ, I2+  IJ),
     2                     (VBL(IC), IC=I1+2*IJ, I2+2*IJ),
     3                     (VBL(ID), ID=I1+3*IJ, I2+3*IJ)
C
  500       FORMAT (' ', 5F6.2, 4X, 5F6.2, 4X, 5F6.2, 4X, 5F6.2)
C
            I1 = I1 - IM
            I2 = I1 + 4
C
  600    CONTINUE
C
      I1 = I1 + 5*IM + 4*IJ
      I2 = I1 + 4
C
  700 CONTINUE
C
C              THE HV FIELD.
      I1 = I0 + (J0-1)*IM
     1     + IADDRG(INDEXHV, NG) - 1
     2     + 2*IM - 2
      I2 = I1 + 4
C
      WRITE (6, 800) NG, I0, J0, NSTEPS
  800 FORMAT ('0', 'THE HV FIELD ON GRID',I2,' AROUND (I0, J0) = (',I2,
     1             ',', I2, ') FOR NSTEPS=', I4, ':' )
C
      DO 1100 K=1, KM, 4
C
         WRITE (6, 450)
C
         DO 1000 J=1,5
C
            WRITE (6, 500) (VBL(IA), IA=I1,      I2     ),
     1                     (VBL(IB), IB=I1+  IJ, I2+  IJ),
     2                     (VBL(IC), IC=I1+2*IJ, I2+2*IJ),
     3                     (VBL(ID), ID=I1+3*IJ, I2+3*IJ)
C
            I1 = I1 - IM
            I2 = I1 + 4
C
 1000    CONTINUE
C
      I1 = I1 + 5*IM + 4*IJ
      I2 = I1 + 4
C
 1100 CONTINUE
C
C              THE HTH FIELD.
      I1 = I0 + (J0-1)*IM
     1     + IADDRG(INDEXHTH, NG) - 1
     2     + 2*IM - 2
      I2 = I1 + 4
C
      WRITE (6, 1200) NG, I0, J0, NSTEPS
 1200 FORMAT ('0', 'THE HTH FIELD ON GRID', I2,
     1              ' AROUND (I0, J0) = (', I2,
     2             ',', I2, ') FOR NSTEPS=', I4, ':' )
C
      DO 1400 K=1, KM, 4
C
         WRITE (6, 450)
C
         DO 1300 J=1,5
C
            WRITE (6, 1250) (VBL(IA), IA=I1,      I2     ),
     1                      (VBL(IB), IB=I1+  IJ, I2+  IJ),
     2                      (VBL(IC), IC=I1+2*IJ, I2+2*IJ),
     3                      (VBL(ID), ID=I1+3*IJ, I2+3*IJ)
C
 1250       FORMAT (' ', 5F6.1, 4X, 5F6.1, 4X, 5F6.1, 4X, 5F6.1)
C
            I1 = I1 - IM
            I2 = I1 + 4
C
 1300    CONTINUE
C
      I1 = I1 + 5*IM + 4*IJ
      I2 = I1 + 4
C
 1400 CONTINUE
C
C              THE HQ FIELD.
      I1 = I0 + (J0-1)*IM
     1     + IADDRG(INDEXHQ, NG) - 1
     2     + 2*IM - 2
      I2 = I1 + 4
C
      WRITE (6, 1450) NG, I0, J0, NSTEPS
 1450 FORMAT ('0', 'THE HQ FIELD (UNITS:  10**-3) ON GRID', I2,
     1             ' AROUND (I0, J0) = (', I2,
     2             ',', I2, ') FOR NSTEPS=', I4, ':' )
C
      DO 1700 K=1, KM, 4
C
         WRITE (6, 450)
C
         DO 1600 J=1,5
C
            WRITE (6, 1500) (VBL(IA), IA=I1,      I2     ),
     1                      (VBL(IB), IB=I1+  IJ, I2+  IJ),
     2                      (VBL(IC), IC=I1+2*IJ, I2+2*IJ),
     3                      (VBL(ID), ID=I1+3*IJ, I2+3*IJ)
C
 1500       FORMAT (' ', 5(3PF6.2), 4X, 5(3PF6.2), 4X,
     1                   5(3PF6.2), 4X, 5(3PF6.2) )
C
            I1 = I1 - IM
            I2 = I1 + 4
C
 1600    CONTINUE
C
      I1 = I1 + 5*IM + 4*IJ
      I2 = I1 + 4
C
 1700 CONTINUE
C
      I0  = I00
      J0  = J00
C
      RETURN
      END
