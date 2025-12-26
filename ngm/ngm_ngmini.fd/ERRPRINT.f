      SUBROUTINE ERRPRINT
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ERRPRINT    PRINTS SOME NGM DIAGNOSTICS.
C   PRGMMR: JIM TUCCILLO        ORG: W/NMC4     DATE: 90-06-13
C
C ABSTRACT:  PRINT OUT PARTIAL CONTENTS OF SELECTED COMMON BLOCKS.
C   .
C PROGRAM HISTORY LOG:
C   90-06-13  J TUCCILLO
C
C USAGE:  CALL ERRPRINT
C   OUTPUT FILES:
C     FT06F001  - FOR PRINTOUT OF ERROR MESSAGES
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - COMCONST
C
C
C REMARKS:
C   - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   VARIED          CONTENTS OF COMMON                     COMMON
C   .
C   - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C   .
C   NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----       ----------------------------------        ---------
C   VARIED          CONTENTS OF COMMON                     PRINT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      include 'parmodel'
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
      PRINT 50
   50 FORMAT (1H1, '  **********  PRINT FROM SUBROUTINE ERRPRINT ',
     1             'FOLLOWS. ***********')
C
      PRINT 100, IJMAX, KM, LVLTOT, NGRDUSE, NH
  100 FORMAT (1H1, 2X, 'IJMAX, KM, LVLTOT, NGRDUSE, NH =', 5I10)
C
      PRINT 200, NTIME, ITIME, NSTEPS
  200 FORMAT (1H0, 2X, 'NTIME, ITIME, NSTEPS= ', 3I10 /)
C
C
      RETURN
      END
